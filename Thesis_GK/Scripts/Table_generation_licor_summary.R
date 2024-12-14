# Load required packages
library(tidyverse)
library(kableExtra)  # For formatted tables
library(flextable)   # Alternative for Word documents
library(gt)          # For modern HTML tables
library(stargazer)   # For regression tables
library(gtsummary)   # For clinical/summary tables
licor_data <- licor_comb_2022_final

# Create summary statistics grouped by Tx and Stress
# First, let's look at unique values in Tx column
unique(licor_data$Tx)

summary_table <- licor_data %>%
  mutate(Tx = as.character(Tx)) %>%
  group_by(Var, Tx, Stress) %>%
  summarise(
    # Photosynthesis (A)
    A_mean = mean(A, na.rm = TRUE),
    A_se = sd(A, na.rm = TRUE)/sqrt(sum(!is.na(A))),
    
    # Stomatal conductance (gsw)
    gs_mean = mean(gsw, na.rm = TRUE),
    gs_se = sd(gsw, na.rm = TRUE)/sqrt(sum(!is.na(gsw))),
    
    # Transpiration (E) - multiply by 1000 to shift decimal
    E_mean = mean(E, na.rm = TRUE) * 1000,
    E_se = sd(E, na.rm = TRUE)/sqrt(sum(!is.na(E))) * 1000,
    
    # Intrinsic Water Use Efficiency (WUEi)
    WUEi_mean = mean(A/gsw, na.rm = TRUE),
    WUEi_se = sd(A/gsw, na.rm = TRUE)/sqrt(sum(!is.na(A/gsw))),
    
    .groups = 'drop'
  ) %>%
  mutate(
    Var = case_when(
      Var == "ch" ~ "Chardonnay",
      Var == "cs" ~ "Cabernet Sauvignon",
      TRUE ~ Var
    ),
    Tx = case_when(
      Tx == "NaN" ~ "Control",
      Tx == "2" ~ "2L Emitter",
      Tx == "4" ~ "4L Emitter",
      TRUE ~ Tx
    )
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# Create table using gt
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Gas Exchange Parameters by Variety, Treatment, and Stress Level",
  ) %>%
  tab_spanner(
    label = "Photosynthesis (μmol m⁻² s⁻¹)",
    columns = c(A_mean, A_se)
  ) %>%
  tab_spanner(
    label = "Stomatal Conductance (mol m⁻² s⁻¹)",
    columns = c(gs_mean, gs_se)
  ) %>%
  tab_spanner(
    label = "Transpiration (mmol m⁻² s⁻¹)",
    columns = c(E_mean, E_se)
  ) %>%
  tab_spanner(
    label = "WUEi (μmol CO₂/mol H₂O)",
    columns = c(WUEi_mean, WUEi_se)
  ) %>%
  cols_label(
    A_mean = "Mean",
    A_se = "SE",
    gs_mean = "Mean",
    gs_se = "SE",
    E_mean = "Mean",
    E_se = "SE",
    WUEi_mean = "Mean",
    WUEi_se = "SE"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  ) %>%
  tab_source_note(
    source_note = "Values represent means ± standard error"
  )

# Display the table
gt_table

# Fix the flextable code first
flex_table <- flextable(summary_table) %>%
  theme_vanilla() %>%
  add_header_row(values = c("", "", "", "Photosynthesis", "Photosynthesis", 
                            "Conductance", "Conductance", 
                            "Transpiration", "Transpiration",
                            "WUEi", "WUEi"),
                 colwidths = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)) %>%
  align(align = "center", part = "all") %>%
  autofit()

# Fix the gt table code
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Gas Exchange Parameters by Treatment",
    subtitle = "Means ± Standard Error"
  ) %>%
  cols_label(
    Var = "Variety",
    Tx = "Treatment",
    Stress = "Stress",
    A_mean = "Mean",
    A_se = "SE",
    gs_mean = "Mean",
    gs_se = "SE",
    E_mean = "Mean",
    E_se = "SE",
    WUEi_mean = "Mean",
    WUEi_se = "SE"
  ) %>%
  tab_spanner(
    label = "Photosynthesis",
    columns = c(A_mean, A_se)
  ) %>%
  tab_spanner(
    label = "Conductance",
    columns = c(gs_mean, gs_se)
  ) %>%
  tab_spanner(
    label = "Transpiration",
    columns = c(E_mean, E_se)
  ) %>%
  tab_spanner(
    label = "WUEi",
    columns = c(WUEi_mean, WUEi_se)
  )

# Save the tables
# For Word
save_as_docx(flex_table, path = "summary_table.docx")

# For HTML
gtsave(gt_table, "summary_table.html")

# For CSV
write.csv(summary_table, "summary_table.csv", row.names = FALSE)

# The high temperature summary looks good and doesn't need changes

# Create publication-quality summary for high temperature data
high_temp_summary <- licor_data %>%
  filter(Tleaf > 30) %>%
  group_by(Var, block) %>%
  summarise(
    n = n(),
    A_mean = mean(A, na.rm = TRUE),
    A_se = sd(A, na.rm = TRUE)/sqrt(n()),
    gs_mean = mean(gsw, na.rm = TRUE),
    gs_se = sd(gsw, na.rm = TRUE)/sqrt(n()),
    Tleaf_mean = mean(Tleaf, na.rm = TRUE),
    Tleaf_se = sd(Tleaf, na.rm = TRUE)/sqrt(n())
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# Create enhanced high temperature summary (T > 37°C)
high_temp_summary <- licor_data %>%
  filter(Tleaf > 37) %>%
  mutate(Tx = as.character(Tx)) %>%
  group_by(Var, Tx, Stress) %>%
  summarise(
    n = n(),
    A_mean = mean(A, na.rm = TRUE),
    A_se = sd(A, na.rm = TRUE)/sqrt(n()),
    gs_mean = mean(gsw, na.rm = TRUE),
    gs_se = sd(gsw, na.rm = TRUE)/sqrt(n()),
    E_mean = mean(E, na.rm = TRUE) * 1000, # Convert to mmol
    E_se = sd(E, na.rm = TRUE)/sqrt(n()) * 1000,
    Tleaf_mean = mean(Tleaf, na.rm = TRUE),
    Tleaf_se = sd(Tleaf, na.rm = TRUE)/sqrt(n()),
    WUEi_mean = mean(A/gsw, na.rm = TRUE),
    WUEi_se = sd(A/gsw, na.rm = TRUE)/sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    Var = case_when(
      Var == "ch" ~ "Chardonnay",
      Var == "cs" ~ "Cabernet Sauvignon",
      TRUE ~ Var
    ),
    Tx = case_when(
      Tx == "NaN" ~ "Control",
      Tx == "2" ~ "2L Emitter",
      Tx == "4" ~ "4L Emitter",
      TRUE ~ Tx
    )
  )

# Create publication-quality table
high_temp_table <- high_temp_summary %>%
  gt() %>%
  tab_header(
    title = "Gas Exchange Parameters During Severe Heat Stress (T leaf > 37°C)",
    subtitle = "Treatment Means ± Standard Error"
  ) %>%
  tab_spanner(
    label = "Photosynthesis (μmol m⁻² s⁻¹)",
    columns = c(A_mean, A_se)
  ) %>%
  tab_spanner(
    label = "Stomatal Conductance (mol m⁻² s⁻¹)",
    columns = c(gs_mean, gs_se)
  ) %>%
  tab_spanner(
    label = "Transpiration (mmol m⁻² s⁻¹)",
    columns = c(E_mean, E_se)
  ) %>%
  tab_spanner(
    label = "Leaf Temperature (°C)",
    columns = c(Tleaf_mean, Tleaf_se)
  ) %>%
  tab_spanner(
    label = "WUEi (μmol CO₂/mol H₂O)",
    columns = c(WUEi_mean, WUEi_se)
  ) %>%
  cols_label(
    Var = "Variety",
    Tx = "Treatment",
    Stress = "Stress Level",
    n = "n",
    A_mean = "Mean",
    A_se = "SE",
    gs_mean = "Mean",
    gs_se = "SE",
    E_mean = "Mean",
    E_se = "SE",
    Tleaf_mean = "Mean",
    Tleaf_se = "SE",
    WUEi_mean = "Mean",
    WUEi_se = "SE"
  ) %>%
  fmt_number(
    columns = matches("mean$"),
    decimals = 2
  ) %>%
  fmt_number(
    columns = matches("se$"),
    decimals = 3
  ) %>%
  tab_source_note(
    source_note = "Note: Only includes measurements where leaf temperature exceeded 37°C"
  )

# Display the table
high_temp_table

# Check sample sizes with proper type conversion
sample_check <- licor_data %>%
  filter(Tleaf > 37) %>%
  mutate(Tx = as.character(Tx)) %>%  # Convert Tx to character first
  group_by(Var, Tx, Stress) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Var = case_when(
      Var == "ch" ~ "Chardonnay",
      Var == "cs" ~ "Cabernet Sauvignon",
      TRUE ~ Var
    ),
    Tx = case_when(
      Tx == "NaN" ~ "Control",
      Tx == "2" ~ "2L Emitter",
      Tx == "4" ~ "4L Emitter",
      TRUE ~ Tx
    )
  )

# Display the sample sizes
print(sample_check)

# 1. First let's look at the overall distribution of samples
sample_distribution <- licor_data %>%
  mutate(Tx = as.character(Tx)) %>%
  filter(Tleaf > 37) %>%
  group_by(Var, Tx) %>%
  summarise(
    count = n(),
    mean_A = mean(A, na.rm = TRUE),
    sd_A = sd(A, na.rm = TRUE),
    .groups = 'drop'
  )

print(sample_distribution)

# 2. Let's perform a power analysis to determine if we have enough samples

library(pwr)

# Calculate effect size for main groups
group_stats <- licor_data %>%
  mutate(Tx = as.character(Tx)) %>%
  filter(Tleaf > 37) %>%
  group_by(Tx) %>%
  summarise(
    n = n(),
    mean = mean(A, na.rm = TRUE),
    sd = sd(A, na.rm = TRUE)
  )

# Calculate Cohen's d effect size between treatments
if(nrow(group_stats) >= 2) {
  d <- (max(group_stats$mean) - min(group_stats$mean)) / 
    sqrt((group_stats$sd[1]^2 + group_stats$sd[2]^2) / 2)
  
  # Power analysis
  power_test <- pwr.t.test(d = d,
                           sig.level = 0.05,
                           power = 0.8,
                           type = "two.sample")
  
  print("Required sample size per group for 80% power:")
  print(ceiling(power_test$n))
}

# 3. Check normality assumption
shapiro_results <- licor_data %>%
  mutate(Tx = as.character(Tx)) %>%
  filter(Tleaf > 37) %>%
  group_by(Tx) %>%
  summarise(
    shapiro_p = shapiro.test(A)$p.value,
    .groups = 'drop'
  )

print("Shapiro-Wilk normality test results:")
print(shapiro_results)

# Kruskal-Wallis test
kruskal.test(A ~ Tx, data = subset(licor_data, Tleaf > 37))

# If significant, follow with Dunn's test
library(FSA)
dunnTest(A ~ Tx, data = subset(licor_data, Tleaf > 37), method="bonferroni")

# Example for comparing 2L vs 4L treatments
wilcox.test(A ~ Tx, data = subset(licor_data, Tleaf > 37 & Tx %in% c("2", "4")))

# First for Cabernet Sauvignon only (since it has more data)
wilcox.test(A ~ Tx, 
            data = subset(licor_data, Tleaf > 37 & Tx %in% c("2", "4") & Var == "cs"))

# Then separate by stress level for CS
# Non-stressed
wilcox.test(A ~ Tx, 
            data = subset(licor_data, Tleaf > 37 & Tx %in% c("2", "4") & Var == "cs" & Stress == "NS"))

# Stressed
wilcox.test(A ~ Tx, 
            data = subset(licor_data, Tleaf > 37 & Tx %in% c("2", "4") & Var == "cs" & Stress == "S"))

# Function to run Wilcox tests for multiple parameters
phys_params_test <- function(data, var, temp_thresh) {
  # For gs (stomatal conductance)
  gs_test <- wilcox.test(gsw ~ Tx, data = subset(data, Tleaf > temp_thresh & Tx %in% c("2", "4") & Var == "cs"))
  
  # For E (transpiration)
  e_test <- wilcox.test(E ~ Tx, data = subset(data, Tleaf > temp_thresh & Tx %in% c("2", "4") & Var == "cs"))
  
  # For WUEi (water use efficiency)
  wuei_test <- wilcox.test(A/gsw ~ Tx, data = subset(data, Tleaf > temp_thresh & Tx %in% c("2", "4") & Var == "cs"))
  
  return(list(gs = gs_test, e = e_test, wuei = wuei_test))
}

# Run tests at 37°C
results_37 <- phys_params_test(licor_data, "cs", 37)

# Try lower thresholds to find optimal sample size
temps <- c(35, 36, 37, 38)
sample_sizes <- sapply(temps, function(t) {
  nrow(subset(licor_data, Tleaf > t & Tx %in% c("2", "4") & Var == "cs"))
})

print("Sample sizes at different temperature thresholds:")
data.frame(Threshold = temps, Samples = sample_sizes)

# Temporal patterns - group by time of day
temporal_summary <- licor_data %>%
  filter(Tleaf > 37) %>%
  group_by(Time_id, Tx) %>%
  summarise(
    n = n(),
    A_mean = mean(A, na.rm = TRUE),
    gs_mean = mean(gsw, na.rm = TRUE),
    E_mean = mean(E, na.rm = TRUE),
    WUEi_mean = mean(A/gsw, na.rm = TRUE),
    .groups = 'drop'
  )
print(temporal_summary)
