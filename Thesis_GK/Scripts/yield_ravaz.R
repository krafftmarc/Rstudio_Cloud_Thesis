
rm(list = ls())

load("yield_preloaded.RData")

ls()

# 1. LIBRARIES AND SETUP
library(tidyverse)
library(ggplot2)
library(stats)
library(nlme)      # For mixed models
library(car)       # For VIF analysis
library(corrplot)  # For correlation matrices
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)

# Create directories if they don't exist
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# 2. FUNCTIONS
calculate_vpd <- function(tmax_f, rh_min) {
  # Input validation
  if(length(tmax_f) != length(rh_min)) {
    stop("Temperature and humidity vectors must be the same length")
  }
  
  # Vectorized function with input validation
  vpd <- vapply(seq_along(tmax_f), function(i) {
    if(is.na(tmax_f[i]) || is.na(rh_min[i])) return(NA_real_)
    if(rh_min[i] < 0 || rh_min[i] > 100) return(NA_real_)
    
    tmax_c <- (tmax_f[i] - 32) * 5/9
    es <- 0.611 * exp((17.27 * tmax_c)/(tmax_c + 237.3))
    ea <- es * (rh_min[i]/100)
    return(es - ea)
  }, FUN.VALUE = numeric(1))
  
  return(vpd)
}

save_table_formats <- function(data, filename, caption) {
  tryCatch({
    # Save as text file
    sink(paste0("tables/", filename, ".txt"))
    print(kable(data, caption = caption, format = "pipe"))
    sink()
    
    # Save as PNG
    png(paste0("tables/", filename, ".png"), 
        width = 8, height = min(nrow(data) + 3, 12), 
        units = "in", res = 300)
    grid.table(data, 
               rows = NULL,
               theme = ttheme_minimal(
                 core = list(fg_params = list(hjust = 0, x = 0.1),
                             bg_params = list(fill = c("white", "grey95"))),
                 colhead = list(fg_params = list(fontface = "bold"))))
    grid.text(caption, y = 0.98, gp = gpar(fontface = "bold"))
    dev.off()
  }, error = function(e) {
    warning(paste("Error saving table:", filename, "-", e$message))
  })
}

# 3. THEME DEFINITION
clean_theme <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

# 4. DATA VALIDATION
required_data <- c(
  "CIMIS_2023",
  "CIMIS_growing_season_2022",
  "Clean_pruning_weights_2023",
  "Clean_Tyree_Water_Potentials_2023",
  "Pruning_Weights_Tyree_2023_xlsx_Sheet1",
  "Tyree_2022_Cleaned",
  "Tyree_2023_Yield_xlsx_Cabernet_Sauvignon_9212023_",
  "Tyree_2023_Yield_xlsx_Chardonnay",
  "Tyree_Chardonnay_Harvest_8_24_2022",
  "Tyree_CS_Harvest_9_16_22_2022"
)

missing_data <- setdiff(required_data, ls())
if(length(missing_data) > 0) {
  stop("Required data frames not found: ", paste(missing_data, collapse = ", "))
}

# 5. DATA PROCESSING
# First let's examine our data structures
print("Column names in Clean_pruning_weights_2023:")
names(Clean_pruning_weights_2023)

print("\nColumn names in Pruning_Weights_Tyree_2023_xlsx_Sheet1:")
names(Pruning_Weights_Tyree_2023_xlsx_Sheet1)

# Process 2022 pruning weights (collected in early 2023)
pruning_2022 <- Clean_pruning_weights_2023 %>%
  select(Block, Row, Vine, Pruning_weight) %>%
  mutate(
    `Weight (Kg)` = as.numeric(Pruning_weight)  # Ensure numeric conversion
  ) %>%
  select(-Pruning_weight) %>%  # Remove old column
  filter(!is.na(`Weight (Kg)`)) %>%
  filter(`Weight (Kg)` > 0) %>%
  arrange(Block, Row, Vine) %>%
  mutate(year = 2022)

# Process 2023 pruning weights
pruning_2023 <- Pruning_Weights_Tyree_2023_xlsx_Sheet1 %>%
  select(Block, Row, Vine, `Weight (kg)`) %>%
  rename(`Weight (Kg)` = `Weight (kg)`) %>%
  mutate(
    `Weight (Kg)` = as.numeric(`Weight (Kg)`)  # Ensure numeric conversion
  ) %>%
  filter(!is.na(`Weight (Kg)`)) %>%
  filter(`Weight (Kg)` > 0) %>%
  arrange(Block, Row, Vine) %>%
  mutate(year = 2023)

# Create separate summaries for each year
pruning_summary_2022 <- pruning_2022 %>%
  group_by(Block) %>%
  summarize(
    pruning_weight = mean(`Weight (Kg)`, na.rm = TRUE),
    sd_pruning = sd(`Weight (Kg)`, na.rm = TRUE),
    cv_pruning = (sd_pruning/pruning_weight) * 100,
    n_vines_pruning = n(),
    year = 2022
  )

pruning_summary_2023 <- pruning_2023 %>%
  group_by(Block) %>%
  summarize(
    pruning_weight = mean(`Weight (Kg)`, na.rm = TRUE),
    sd_pruning = sd(`Weight (Kg)`, na.rm = TRUE),
    cv_pruning = (sd_pruning/pruning_weight) * 100,
    n_vines_pruning = n(),
    year = 2023
  )

# Combine pruning summaries
pruning_summary <- bind_rows(
  pruning_summary_2022,
  pruning_summary_2023
)

# Verify our data
print("\nSummary statistics for 2022 pruning weights:")
summary(pruning_2022$`Weight (Kg)`)
print(paste("Number of vines in 2022:", nrow(pruning_2022)))

print("\nSummary statistics for 2023 pruning weights:")
summary(pruning_2023$`Weight (Kg)`)
print(paste("Number of vines in 2023:", nrow(pruning_2023)))

# Process 2022 yield data with vine-level info - year from filename
chard_2022_summary <- Tyree_Chardonnay_Harvest_8_24_2022 %>%
  group_by(Block, Row, Vine) %>%
  summarize(
    yield = mean(`Weight (Kg)`, na.rm = TRUE),
    clusters = mean(`Cluster Count`, na.rm = TRUE),
    variety = "Chardonnay",
    .groups = "drop"
  ) %>%
  mutate(year = 2022)  # Add year as a column based on filename

cs_2022_summary <- Tyree_CS_Harvest_9_16_22_2022 %>%
  group_by(Block, Row, Vine) %>%
  summarize(
    yield = mean(`Weight (Kg)`, na.rm = TRUE),
    clusters = mean(`Cluster Count`, na.rm = TRUE),
    variety = "Cabernet Sauvignon",
    .groups = "drop"
  ) %>%
  mutate(year = 2022)  # Add year as a column based on filename

# Process 2023 yield data - year from filename
chard_2023_summary <- Tyree_2023_Yield_xlsx_Chardonnay %>%
  group_by(Block, Row, Vine) %>%
  summarize(
    yield = mean(`Wt(kg)`, na.rm = TRUE),
    clusters = mean(`Cluster Count`, na.rm = TRUE),
    variety = "Chardonnay",
    .groups = "drop"
  ) %>%
  mutate(year = 2023)  # Add year as a column based on filename

# Process 2023 Cabernet Sauvignon yield data with Row extraction
cs_2023_summary <- Tyree_2023_Yield_xlsx_Cabernet_Sauvignon_9212023_ %>%
  mutate(
    Row = as.integer(`Row Vine`),  # Extract Row number
    Vine = Vine                     # Vine is already separate
  ) %>%
  group_by(Block, Row, Vine) %>%
  summarize(
    yield = mean(`Wt(Kg)`, na.rm = TRUE),
    clusters = mean(`Cluster Count`, na.rm = TRUE),
    variety = "Cabernet Sauvignon",
    .groups = "drop"
  ) %>%
  mutate(year = 2023)  # Add year as a column based on filename

# ADD THIS CODE BLOCK HERE
# Combine all yield data into a single dataset
yield_data <- bind_rows(
  chard_2022_summary,
  cs_2022_summary,
  chard_2023_summary,
  cs_2023_summary
) %>%
  arrange(year, variety, Block, Row, Vine)  # Sort for easier viewing


# After yield_data creation and before vine_treatments...

# Calculate Ravaz indices for each year
ravaz_data_2022 <- yield_data %>%
  filter(year == 2022) %>%
  left_join(pruning_2022, by = c("Block", "Row", "Vine")) %>%
  mutate(
    ravaz_index = yield / `Weight (Kg)`,
    yield_per_cluster = yield / clusters,
    balance_category = case_when(
      ravaz_index < 3 ~ "Undercropped",
      ravaz_index >= 3 & ravaz_index <= 10 ~ "Balanced",
      ravaz_index > 10 ~ "Overcropped"
    )
  )

ravaz_data_2023 <- yield_data %>%
  filter(year == 2023) %>%
  left_join(pruning_2023, by = c("Block", "Row", "Vine")) %>%
  mutate(
    ravaz_index = yield / `Weight (Kg)`,
    yield_per_cluster = yield / clusters,
    balance_category = case_when(
      ravaz_index < 3 ~ "Undercropped",
      ravaz_index >= 3 & ravaz_index <= 10 ~ "Balanced",
      ravaz_index > 10 ~ "Overcropped"
    )
  )

# Combine Ravaz data from both years
ravaz_data <- bind_rows(
  ravaz_data_2022,
  ravaz_data_2023
) %>%
  filter(!is.na(ravaz_index))  # Remove any rows where we couldn't calculate Ravaz index

# Combine treatment information with location identifiers
vine_treatments <- Clean_Tyree_Water_Potentials_2023 %>%
  select(block, row, vine_id, Tx, Stress) %>%
  filter(!is.na(Tx)) %>%
  distinct() %>%
  rename(
    Block = block,
    Row = row,
    Vine = vine_id
  ) %>%
  mutate(
    location_id = paste(Block, Row, Vine, sep="_"),  # Create unique location identifier
    treatment_group = paste(Tx, Stress)              # Combined treatment group
  )

# Create per-vine summary with all metrics
per_vine_metrics <- yield_data %>%
  mutate(
    location_id = paste(Block, Row, Vine, sep="_"),
    cluster_weight = yield / clusters  # Calculate average cluster weight
  ) %>%
  left_join(vine_treatments, 
            by = c("location_id", "Block", "Row", "Vine")) %>%  # Join on all matching columns
  arrange(year, variety, treatment_group)  # Remove Block, Row, Vine from arrange since order is less important than grouping

# Create summary statistics grouped by our key variables
treatment_summary <- per_vine_metrics %>%
  group_by(year, variety, Tx, Stress) %>%
  summarise(
    n_vines = n(),
    mean_yield = mean(yield, na.rm = TRUE),
    sd_yield = sd(yield, na.rm = TRUE),
    mean_clusters = mean(clusters, na.rm = TRUE),
    sd_clusters = sd(clusters, na.rm = TRUE),
    mean_cluster_weight = mean(cluster_weight, na.rm = TRUE),
    sd_cluster_weight = sd(cluster_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, variety, Tx, Stress)

# Create visualization with raw data points
# First, create an ordered factor for x-axis sorting
ordered_data <- per_vine_metrics %>%
  mutate(
    # Create ordered grouping variable
    group_order = paste(year, variety, Tx, Stress, sep="_"),
    # Create x-axis labels
    location_label = paste(Block, Row, Vine, sep="_")
  ) %>%
  arrange(year, variety, Tx, Stress, Block, Row, Vine) %>%
  # Create ordered factors for proper plot sorting
  mutate(
    location_label = factor(location_label, levels = unique(location_label)),
    treatment_group = factor(treatment_group)
  )

# Create yield plot
yield_plot <- ggplot(ordered_data, 
                     aes(x = location_label, y = yield, 
                         color = treatment_group, shape = variety)) +
  geom_point(size = 2) +
  facet_grid(year ~ variety, scales = "free_x", space = "free_x") +
  clean_theme +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Yield per Vine by Location",
    subtitle = "Grouped by Year, Variety, and Treatment",
    x = "Vine Location (Block_Row_Vine)",
    y = "Yield (Kg)",
    color = "Treatment",
    shape = "Variety"
  )

# Create cluster weight plot
cluster_weight_plot <- ggplot(ordered_data, 
                              aes(x = location_label, y = cluster_weight, 
                                  color = treatment_group, shape = variety)) +
  geom_point(size = 2) +
  facet_grid(year ~ variety, scales = "free_x", space = "free_x") +
  clean_theme +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Cluster Weight per Vine by Location",
    subtitle = "Grouped by Year, Variety, and Treatment",
    x = "Vine Location (Block_Row_Vine)",
    y = "Cluster Weight (Kg)",
    color = "Treatment",
    shape = "Variety"
  )

# Save plots
ggsave("figures/per_vine_yield.png", plot = yield_plot, 
       width = 15, height = 10, dpi = 300)
ggsave("figures/per_vine_cluster_weight.png", plot = cluster_weight_plot, 
       width = 15, height = 10, dpi = 300)

# Print summary statistics
print("Treatment Summary Statistics:")
print(treatment_summary)

# Save summary table
save_table_formats(treatment_summary, 
                   "treatment_vine_summary",
                   "Yield and Cluster Weight Summary by Treatment")

# Process water stress data
water_stress_metrics_2022 <- Tyree_2022_Cleaned %>%
  mutate(
    Variety = case_when(
      Variety == "CH" ~ "Chardonnay",
      Variety == "CS" ~ "Cabernet Sauvignon",
      TRUE ~ Variety
    ),
    Date = as.Date(paste(Year, Month, Day, sep = "-")),
    year = 2022
  ) %>%
  group_by(Block_ID, Variety) %>%
  summarize(
    mean_psi = mean(PSI, na.rm = TRUE),
    min_psi = min(PSI, na.rm = TRUE),
    sd_psi = sd(PSI, na.rm = TRUE),
    stress_days = sum(PSI < -12, na.rm = TRUE),
    early_mean_psi = mean(PSI[Month <= 7], na.rm = TRUE),
    late_mean_psi = mean(PSI[Month > 7], na.rm = TRUE),
    veraison_psi = mean(PSI[Month %in% c(7,8)], na.rm = TRUE),
    year = 2022,
    .groups = "drop"
  )

# Process 2023 water stress data FIRST
water_stress_metrics_2023 <- Clean_Tyree_Water_Potentials_2023 %>%
  mutate(
    Variety = case_when(
      variety == "CH" ~ "Chardonnay",
      variety == "CS" ~ "Cabernet Sauvignon",
      TRUE ~ NA_character_
    ),
    Year = year(date),
    Month = month(date),
    Day = day(date),
    PSI = `Ψ`,
    Block_ID = block
  ) %>%
  group_by(Block_ID, Variety) %>%
  summarize(
    mean_psi = mean(PSI, na.rm = TRUE),
    min_psi = min(PSI, na.rm = TRUE),
    sd_psi = sd(PSI, na.rm = TRUE),
    stress_days = sum(PSI < -12, na.rm = TRUE),
    early_mean_psi = mean(PSI[Month <= 7], na.rm = TRUE),
    late_mean_psi = mean(PSI[Month > 7], na.rm = TRUE),
    veraison_psi = mean(PSI[Month %in% c(7,8)], na.rm = TRUE),
    n_measurements = n(),  # Add count of measurements
    year = 2023,
    .groups = "drop"
  ) %>%
  filter(!is.na(Variety))  # Remove any rows where variety couldn't be determined

# Now combine both years' data
water_stress_metrics <- bind_rows(
  water_stress_metrics_2022,
  water_stress_metrics_2023
)


# Process treatment information
treatment_info <- Clean_Tyree_Water_Potentials_2023 %>%
  select(block, row, vine_id, Tx, Stress) %>%
  filter(!is.na(Tx)) %>%
  distinct() %>%
  rename(
    Block = block,
    Row = row,
    Vine = vine_id,
    emitter_treatment = Tx,
    stress_treatment = Stress
  ) %>%
  mutate(
    treatment = paste(emitter_treatment, stress_treatment),
    treatment_description = case_when(
      treatment == "2L S" ~ "2L emitters, stressed during heatwaves",
      treatment == "2L NS" ~ "2L emitters, not stressed during heatwaves",
      treatment == "4L S" ~ "4L emitters, stressed during heatwaves",
      treatment == "4L NS" ~ "4L emitters, not stressed during heatwaves",
      TRUE ~ NA_character_
    )
  )

# Create combined analysis with clean data and explicit year matching
combined_analysis <- ravaz_data %>%
  left_join(treatment_info, by = c("Block", "Row", "Vine")) %>%
  left_join(water_stress_metrics %>% 
              filter(year == 2022) %>% 
              select(-year),  # Remove year to avoid duplicate columns
            by = c("Block" = "Block_ID", "variety" = "Variety")) %>%
  group_by(variety, year.x) %>%  # Use year.x consistently
  mutate(
    across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
  ) %>%
  ungroup() %>%
  mutate(
    treatment = ifelse(is.na(treatment), "Control", treatment),
    across(where(is.numeric), ~scale(.) %>% as.vector(), .names = "scaled_{.col}")
  )

# Add this line after the combined_analysis definition
filtered_combined_analysis <- combined_analysis %>%
  filter(treatment != "Control" & treatment != "Unknown")

# ===== INSERT NA CHECK HERE =====
# Check for remaining NAs after initial imputation
na_summary <- colSums(is.na(combined_analysis))
print("Columns with NA values (after initial imputation):")
print(na_summary[na_summary > 0])

# Check rows with NAs
na_rows <- combined_analysis %>% filter(if_any(everything(), is.na))
print("Rows with NA values (after initial imputation):")
print(na_rows)

# ===== IMPUTE REMAINING NAs =====
# Impute character and factor columns
combined_analysis <- combined_analysis %>%
  mutate(across(where(is.character), ~replace_na(., "Unknown")),
         across(where(is.factor), ~fct_explicit_na(.)))

# Impute remaining numeric NAs with zero
combined_analysis <- combined_analysis %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# ===== FINAL NA CHECK =====
na_summary_final <- colSums(is.na(combined_analysis))
print("Final NA check (should be zero):")
print(na_summary_final[na_summary_final > 0])

# Stop if any NAs are left
stopifnot(sum(is.na(combined_analysis)) == 0)


# Identify columns with remaining NA values
na_summary <- colSums(is.na(combined_analysis))
print("Columns with NA values:")
print(na_summary[na_summary > 0])


# Check the cleaned data
print("Summary of cleaned data:")
summary(combined_analysis)

print("\nBalance categories by variety:")
table(combined_analysis$variety, combined_analysis$balance_category)

# First, let's look at the structure of our joins
print("Checking join structures:")
print("Water stress metrics years:")
table(water_stress_metrics$year)

print("\nUnique blocks in ravaz_data:")
unique(ravaz_data$Block)

print("\nUnique blocks in water_stress_metrics:")
unique(water_stress_metrics$Block_ID)

# Modify the combined analysis to only use 2022 water stress data
# Create combined analysis with clean data and explicit year matching
combined_analysis <- ravaz_data %>%
  left_join(treatment_info, by = c("Block", "Row", "Vine")) %>%
  left_join(water_stress_metrics %>% 
              filter(year == 2022) %>% 
              select(-year),  # Remove year to avoid duplicate columns
            by = c("Block" = "Block_ID", "variety" = "Variety")) %>%
  group_by(variety, year.x) %>%  # Group by variety and year for mean imputation
  mutate(
    across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
  ) %>%
  ungroup() %>%
  mutate(
    treatment = ifelse(is.na(treatment), "Control", treatment),
    across(where(is.numeric), ~scale(.) %>% as.vector(), .names = "scaled_{.col}")
  )

# Now let's check the joined data
print("\nChecking combined analysis structure:")
glimpse(combined_analysis)

# Verify we have the correct matches
print("\nNumber of rows in combined analysis:")
nrow(combined_analysis)

print("\nNumber of blocks per variety:")
table(combined_analysis$variety, combined_analysis$Block)

# Statistical Analysis
# Analyze by variety and year
varieties <- unique(combined_analysis$variety)
years <- unique(combined_analysis$year.x)  # Use year.x instead of year

# Function to run analysis for each variety-year combination
analyze_subset <- function(data, yr, var) {
  subset_data <- data %>%
    filter(year.x == yr, variety == var) %>%
    mutate(across(where(is.numeric), ~scale(.) %>% as.vector(), .names = "scaled_{.col}"))
  
  cat(sprintf("\n=== Analysis for %s %s ===\n", var, yr))
  cat("Sample size:", nrow(subset_data), "\n")
  
  # Only run models if we have sufficient data and treatment variation
  if(nrow(subset_data) >= 10 && length(unique(subset_data$treatment)) > 1) {
    tryCatch({
      models <- list(
        # Treatment effects
        yield_aov = aov(yield ~ treatment + mean_psi + Block, data = subset_data),
        ravaz_aov = aov(ravaz_index ~ treatment + mean_psi + Block, data = subset_data),
        # Mixed effects including block
        yield_mixed = lme(scaled_yield ~ treatment + scaled_mean_psi, 
                          random = ~1 | Block, 
                          data = subset_data, 
                          control = lmeControl(opt = "optim")),
        ravaz_mixed = lme(scaled_ravaz_index ~ treatment + scaled_mean_psi,
                          random = ~1 | Block, 
                          data = subset_data, 
                          control = lmeControl(opt = "optim"))
      )
      
      # Model summaries
      cat("\nYield ANOVA:\n")
      print(summary(models$yield_aov))
      cat("\nRavaz Index ANOVA:\n")
      print(summary(models$ravaz_aov))
      cat("\nYield Mixed Model:\n")
      print(summary(models$yield_mixed))
      cat("\nRavaz Index Mixed Model:\n")
      print(summary(models$ravaz_mixed))
      
      return(data.frame(
        variety = var,
        year = yr,
        yield_treatment_f = summary(models$yield_aov)[[1]]$`F value`[1],
        yield_treatment_p = summary(models$yield_aov)[[1]]$`Pr(>F)`[1],
        ravaz_treatment_f = summary(models$ravaz_aov)[[1]]$`F value`[1],
        ravaz_treatment_p = summary(models$ravaz_aov)[[1]]$`Pr(>F)`[1],
        yield_mixed_aic = AIC(models$yield_mixed),
        ravaz_mixed_aic = AIC(models$ravaz_mixed)
      ))
    }, error = function(e) {
      cat("Error in model fitting:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("Insufficient data or treatment variation for analysis\n")
    return(NULL)
  }
}

# Run analysis for each combination
results_list <- list()
for(var in varieties) {
  for(yr in years) {
    results_list[[paste(var, yr)]] <- analyze_subset(combined_analysis, yr, var)
  }
}

# Combine results into a single table, removing NULL entries
results_table <- bind_rows(results_list[!sapply(results_list, is.null)]) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  arrange(year, variety)

# Save results with validation
if(nrow(results_table) > 0) {
  save_table_formats(results_table,
                     "statistical_analysis_summary",
                     "Summary of Statistical Analyses by Variety and Year")
} else {
  warning("No valid statistical results to save")
}

# 6. VISUALIZATION AND RESULTS
# Create plots
ravaz_plot <- ggplot(ravaz_data, aes(x = Block, y = ravaz_index, fill = variety)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(3, 10), linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 1, y = 11, label = "Overcropped threshold", color = "red") +
  annotate("text", x = 1, y = 2, label = "Undercropped threshold", color = "red") +
  clean_theme +
  labs(title = "Ravaz Index by Block and Variety",
       subtitle = "Dashed lines indicate balance thresholds (3-10 optimal range)",
       y = "Ravaz Index (Yield/Pruning Weight)",
       x = "Block") +
  scale_fill_brewer(palette = "Set2")

# Create clean water stress data for 2022 (used in joins)
water_stress_2022 <- water_stress_metrics %>%
  filter(year == 2022) %>%
  select(Block_ID, Variety, mean_psi, min_psi, stress_days, 
         early_mean_psi, late_mean_psi, veraison_psi)

water_stress_comparison <- water_stress_metrics %>%
  ggplot(aes(x = Variety, y = mean_psi, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  clean_theme +
  labs(title = "Water Stress Comparison Between Years",
       x = "Variety",
       y = "Mean Water Potential (PSI)",
       fill = "Year") +
  scale_fill_brewer(palette = "Set2")

# Save plots
ggsave("figures/ravaz_distribution.png", plot = ravaz_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/water_stress_comparison.png", plot = water_stress_comparison, width = 10, height = 6, dpi = 300)

# Correlation analysis
cor_matrix <- combined_analysis %>%
  select(yield, ravaz_index, mean_psi, min_psi, early_mean_psi, late_mean_psi) %>%
  cor(use = "pairwise.complete.obs")

# Save correlation plot
png("figures/correlation_matrix.png", width = 8, height = 6, units = "in", res = 300)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45,
         cl.pos = "n",
         col = colorRampPalette(c("#FFFFFF", "#4477AA"))(100))
dev.off()

# Add after the existing visualization section:
# Treatment effect plots
treatment_stress_plot <- filtered_combined_analysis %>%
  ggplot(aes(x = treatment, y = mean_psi, fill = variety)) +
  geom_boxplot() +
  clean_theme +
  labs(title = "Water Stress by Treatment and Variety",
       x = "Treatment",
       y = "Mean Water Potential (PSI)",
       fill = "Variety") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

treatment_ravaz_plot <- filtered_combined_analysis %>%
  ggplot(aes(x = treatment, y = ravaz_index, fill = variety)) +
  geom_boxplot() +
  clean_theme +
  labs(title = "Ravaz Index by Treatment and Variety",
       x = "Treatment",
       y = "Ravaz Index",
       fill = "Variety") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create year and variety specific figures
# 1. Yield by treatment - Fix using year.x instead of year
ggplot(filtered_combined_analysis, 
       aes(x = treatment, y = yield, fill = treatment)) +
  geom_boxplot() +
  facet_grid(year.x ~ variety) +  # Change year to year.x
  clean_theme +
  labs(title = "Yield by Treatment, Year, and Variety",
       x = "Treatment",
       y = "Yield (Kg)") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/yield_by_treatment_year_variety.png", width = 12, height = 8, dpi = 300)

# 2. Ravaz Index by treatment
ggplot(filtered_combined_analysis, 
       aes(x = treatment, y = ravaz_index, fill = treatment)) +
  geom_boxplot() +
  facet_grid(year.x ~ variety) +  # Change year to year.x
  clean_theme +
  labs(title = "Ravaz Index by Treatment, Year, and Variety",
       x = "Treatment",
       y = "Ravaz Index") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/ravaz_by_treatment_year_variety.png", width = 12, height = 8, dpi = 300)

# 3. Water Stress by treatment
ggplot(filtered_combined_analysis, 
       aes(x = treatment, y = mean_psi, fill = treatment)) +
  geom_boxplot() +
  facet_grid(year.x ~ variety) +  # Change year to year.x
  clean_theme +
  labs(title = "Water Stress by Treatment, Year, and Variety",
       x = "Treatment",
       y = "Mean Water Potential (PSI)") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/water_stress_by_treatment_year_variety.png", width = 12, height = 8, dpi = 300)

# Create summary tables by year and variety - using year.x instead of year
year_variety_summary <- combined_analysis %>%
  group_by(year.x, variety) %>%  # Change year to year.x
  summarize(
    n_vines = n(),
    mean_yield = mean(yield, na.rm = TRUE),
    sd_yield = sd(yield, na.rm = TRUE),
    mean_ravaz = mean(ravaz_index, na.rm = TRUE),
    sd_ravaz = sd(ravaz_index, na.rm = TRUE),
    mean_psi = mean(mean_psi, na.rm = TRUE),
    sd_psi = sd(mean_psi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# Save year-variety summary table
save_table_formats(year_variety_summary,
                   "year_variety_summary",
                   "Summary Statistics by Year and Variety")

# Add clusters analysis with correct year variable
clusters_by_treatment <- ggplot(filtered_combined_analysis,
                                aes(x = treatment, y = clusters, fill = treatment)) +
  geom_boxplot() +
  facet_grid(year.x ~ variety) +  # Change year to year.x
  clean_theme +
  labs(title = "Clusters by Treatment, Year, and Variety",
       x = "Treatment",
       y = "Cluster Count") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/clusters_by_treatment_year_variety.png", width = 12, height = 8, dpi = 300)

# Create treatment response plot
treatment_response <- ggplot(filtered_combined_analysis,
                             aes(x = mean_psi, y = yield, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(year.x ~ variety) +  # Change year to year.x
  clean_theme +
  labs(title = "Treatment Response to Water Stress",
       x = "Mean Water Potential (PSI)",
       y = "Yield (Kg)") +
  scale_color_brewer(palette = "Set2")
ggsave("figures/treatment_response.png", width = 12, height = 8, dpi = 300)

# Create yield by treatment bar plot
yield_treatment_bar <- ggplot(combined_analysis, 
                              aes(x = treatment, y = yield, fill = treatment)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_grid(year.x ~ variety) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  clean_theme +
  labs(title = "Mean Yield by Treatment",
       x = "Treatment",
       y = "Yield (Kg)",
       fill = "Treatment") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create Ravaz Index bar plot
ravaz_treatment_bar <- ggplot(combined_analysis, 
                              aes(x = treatment, y = ravaz_index, fill = treatment)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_grid(year.x ~ variety) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  clean_theme +
  labs(title = "Mean Ravaz Index by Treatment",
       x = "Treatment",
       y = "Ravaz Index",
       fill = "Treatment") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create water stress comparison bar plot
water_stress_bar <- ggplot(water_stress_metrics,
                           aes(x = Variety, y = mean_psi, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_psi - sd_psi, 
                    ymax = mean_psi + sd_psi),
                position = position_dodge(0.9),
                width = 0.2) +
  clean_theme +
  labs(title = "Water Stress by Variety and Year",
       x = "Variety",
       y = "Mean Water Potential (PSI)",
       fill = "Year") +
  scale_fill_brewer(palette = "Set2")

# Save the new bar plots
ggsave("figures/yield_treatment_bar.png", plot = yield_treatment_bar, 
       width = 12, height = 8, dpi = 300)
ggsave("figures/ravaz_treatment_bar.png", plot = ravaz_treatment_bar, 
       width = 12, height = 8, dpi = 300)
ggsave("figures/water_stress_bar.png", plot = water_stress_bar, 
       width = 10, height = 6, dpi = 300)

# Create a combined visualization grid
combined_plots <- grid.arrange(
  yield_treatment_bar, 
  ravaz_treatment_bar,
  water_stress_bar,
  ncol = 1,
  heights = c(1, 1, 0.8)
)

# Save the combined visualization
ggsave("figures/combined_treatment_analysis.png", plot = combined_plots, 
       width = 12, height = 20, dpi = 300)

# Save new plots
ggsave("figures/treatment_stress.png", plot = treatment_stress_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/treatment_ravaz.png", plot = treatment_ravaz_plot, width = 10, height = 6, dpi = 300)

# 5. DATA PROCESSING


ravaz_summary <- ravaz_data %>%
  group_by(variety, balance_category) %>%
  summarise(
    n_blocks = n(),
    mean_ravaz = round(mean(ravaz_index, na.rm = TRUE), 2),
    sd_ravaz = round(sd(ravaz_index, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Now create treatment-specific plots after treatment data is joined
treatment_stress_plot <- combined_analysis %>%
  filter(!is.na(treatment)) %>%
  ggplot(aes(x = treatment, y = mean_psi, fill = variety)) +
  geom_boxplot() +
  clean_theme +
  labs(title = "Water Stress by Treatment and Variety",
       x = "Treatment", y = "Mean Water Potential (PSI)", fill = "Variety") +
  scale_fill_brewer(palette = "Set2")


treatment_ravaz_plot <- combined_analysis %>%
  filter(!is.na(treatment)) %>%  # Ensure we only plot rows with treatment info
  ggplot(aes(x = treatment, y = ravaz_index, fill = variety)) +
  geom_boxplot() +
  clean_theme +
  labs(title = "Ravaz Index by Treatment and Variety",
       x = "Treatment",
       y = "Ravaz Index",
       fill = "Variety") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save all plots
ggsave("figures/ravaz_distribution.png", plot = ravaz_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/water_stress_comparison.png", plot = water_stress_comparison, width = 10, height = 6, dpi = 300)
ggsave("figures/treatment_stress.png", plot = treatment_stress_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/treatment_ravaz.png", plot = treatment_ravaz_plot, width = 10, height = 6, dpi = 300)
# Format basic stats table

basic_stats <- combined_analysis %>%
  left_join(pruning_summary, by = c("Block", "year.x" = "year")) %>%
  group_by(variety) %>%
  summarize(
    mean_ravaz = mean(ravaz_index, na.rm = TRUE),
    sd_ravaz = sd(ravaz_index, na.rm = TRUE),
    mean_yield = mean(yield, na.rm = TRUE),
    mean_pruning = mean(pruning_weight, na.rm = TRUE),
    mean_clusters = mean(clusters, na.rm = TRUE)
  )


basic_stats_formatted <- basic_stats %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  rename(
    "Mean Ravaz" = mean_ravaz,
    "SD Ravaz" = sd_ravaz,
    "Mean Yield" = mean_yield,
    "Mean Pruning" = mean_pruning,
    "Mean Clusters" = mean_clusters
  )
# Combine water stress metrics
water_stress_metrics <- bind_rows(
  water_stress_metrics_2022,
  water_stress_metrics_2023
)

# Format water stress table with proper error handling
water_stress_formatted <- tryCatch({
  water_stress_metrics %>%
    select(Variety, year, mean_psi, min_psi, stress_days) %>%
    mutate(across(c(mean_psi, min_psi, stress_days), round, 2))
}, error = function(e) {
  warning("Error formatting water stress table:", e$message)
  return(NULL)
})


# Save water stress table with validation
if (!is.null(water_stress_formatted)) {
  save_table_formats(water_stress_formatted,
                     "water_stress_summary",
                     "Water Stress Summary by Variety and Year")
} else {
  warning("Water stress table could not be created")
}



# Save all formatted tables
save_table_formats(basic_stats_formatted, "basic_statistics", "Summary Statistics by Variety")
save_table_formats(water_stress_formatted, "water_stress_summary", "Water Stress Summary by Variety and Year")
save_table_formats(ravaz_summary, "ravaz_summary", "Ravaz Index Summary by Variety and Balance Category")

# Create and save treatment summary - fixed version
treatment_summary <- combined_analysis %>%
  group_by(treatment, variety, year.x) %>%  # Add year.x to grouping
  summarize(
    n_vines = n(),
    mean_psi = mean(mean_psi, na.rm = TRUE),
    sd_psi = sd(mean_psi, na.rm = TRUE),
    mean_ravaz = mean(ravaz_index, na.rm = TRUE),
    sd_ravaz = sd(ravaz_index, na.rm = TRUE),
    mean_yield = mean(yield, na.rm = TRUE),
    sd_yield = sd(yield, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(treatment) & treatment != "Unknown") %>%  # Remove NA and Unknown treatments
  arrange(year.x, variety, treatment) %>%  # Sort the results
  mutate(across(where(is.numeric), ~round(., 2)))  # Round numeric columns

# Save treatment summary with error checking
tryCatch({
  save_table_formats(treatment_summary, 
                     "treatment_summary",
                     "Water Stress, Yield, and Ravaz Index by Treatment")
}, error = function(e) {
  print(paste("Error details:", e$message))
  # Print the structure of treatment_summary for debugging
  print("Treatment summary structure:")
  str(treatment_summary)
})

# Save treatment summary
save_table_formats(treatment_summary, 
                   "treatment_summary",
                   "Water Stress and Ravaz Index by Treatment")

