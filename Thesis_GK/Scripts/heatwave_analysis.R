
colnames(NSCs_1st_heatwave_August_17_2022_compiled)


# ========================================================================
# Comprehensive Analysis of Grapevine Response to Heat Stress
# ========================================================================
# This script analyzes how grapevines respond to different irrigation treatments
# (2L vs 4L) and stress conditions (Stressed vs Non-Stressed) during three
# distinct heatwave periods across 2022 and 2023. We examine multiple physiological
# responses including leaf temperature regulation, water relations, and gas exchange.

# Clear workspace and load saved data environment
rm(list = ls())
load("heatwave_set.RData")

# ========================================================================
# Required Libraries and Setup
# ========================================================================
library(tidyverse)  
library(readxl)     
library(lubridate)  
library(nlme)       
library(emmeans)    

# Function to print dataset structure
inspect_dataset <- function(data, dataset_name) {
  cat("\n=== Structure for", dataset_name, "===\n")
  cat("\nColumn names:\n")
  print(names(data))
  
  cat("\nFirst few rows:\n")
  print(head(data, 3))
  
  cat("\nData structure:\n")
  str(data)
}

# Inspect each dataset
inspect_dataset(NSCs_1st_heatwave_August_17_2022_compiled, "NSCs_1st_heatwave_August_17_2022")
inspect_dataset(NSCs_2nd_heatwave_September_6_2022_compiled, "NSCs_2nd_heatwave_September_6_2022")
inspect_dataset(X2023_comb_Licor, "X2023_comb_Licor")

# ========================================================================
# Treatment Assignment and Validation Functions
# ========================================================================

# Treatment assignment function
assign_treatments <- function(data) {
  data %>%
    filter(!is.na(Block), !is.na(Row_ID), !is.na(Vine_ID)) %>%  # Filter out NAs
    mutate(
      Row_Num = as.numeric(Row_ID),
      Vine_Num = as.numeric(Vine_ID),
      water_amount = case_when(
        Block == "A" & Vine_Num <= 8 ~ "2L",
        Block == "A" & Vine_Num > 8 & Vine_Num <= 16 ~ "4L",
        Block == "A" & Vine_Num > 16 ~ "2L",
        Block == "B" & Vine_Num <= 8 ~ "4L",
        Block == "B" & Vine_Num > 8 & Vine_Num <= 16 ~ "2L",
        Block == "B" & Vine_Num > 16 ~ "4L",
        TRUE ~ NA_character_
      ),
      stress_status = case_when(
        Block == "A" & Row_Num %in% c(2,3,10,11) ~ "NS",
        Block == "A" & Row_Num %in% c(6,7) ~ "S",
        Block == "B" & Row_Num %in% c(2,3,10,11) ~ "S",
        Block == "B" & Row_Num %in% c(6,7) ~ "NS",
        Row_Num %in% c(1,4,5,8,9,12) ~ "Buffer",
        TRUE ~ NA_character_
      ),
      treatment = case_when(
        stress_status == "Buffer" ~ "Buffer",
        TRUE ~ paste0(water_amount, "_", stress_status)
      )
    ) %>%
    select(-Row_Num, -Vine_Num)
}


# Treatment validation function
validate_treatment_assignment <- function(data) {
  cat("\n=== Validating Treatment Assignment ===\n")
  
  # Check treatment distribution
  cat("\nTreatment Distribution:\n")
  print(table(data$treatment, useNA = "ifany"))
  
  # Check Block/Row/Treatment combinations
  cat("\nBlock/Row/Treatment combinations:\n")
  if("Tx" %in% names(data)) {
    # For water potential data
    print(unique(data[c("Block", "Row", "Vine", "Tx")]))
  } else if("treatment" %in% names(data)) {
    # For combined data
    print(unique(data[c("Block", "Row_ID", "Vine_ID", "treatment")]))
  }
  
  # Check data coverage by block and row
  cat("\nBlock and Row coverage:\n")
  if("Row" %in% names(data)) {
    print(table(all_hw_data$Block, all_hw_data$Row))
  } else if("Row_ID" %in% names(data)) {
    print(table(all_hw_data$Block, all_hw_data$Row))
  }
  
  # Additional checks
  if("WP" %in% names(data)) {
    cat("\nWater potential summary statistics:\n")
    print(summary(data$WP))
  }
  
  if("time" %in% names(data)) {
    cat("\nMeasurement timing distribution:\n")
    print(table(data$time))
  }
  
  invisible(data)
}




# Initial inspection of raw data
cat("\nInitial Data Structure:")
wp_2022_initial <- Tyree_2022_Cleaned %>%
  filter(!is.na(Tx)) %>%
  select(
    Block = Block_ID,
    Row = Row_ID, 
    Vine = Vine_ID, 
    Treatment = Tx,
    Water_Potential = PSI,
    Month, 
    Day
  ) %>%
  arrange(Block, Row, Vine)

cat("\nSample of Initial 2022 Water Potential Data:")
print(head(wp_2022_initial, 10))

cat("\nUnique Treatments in 2022:")
print(unique(wp_2022_initial$Treatment))


# Now we can continue with the rest of the script...
# Import 2022 water potential data
wp_2022 <- Tyree_2022_Cleaned %>%
  mutate(
    heatwave_period = case_when(
      Month == 8 & Day == 17 ~ "HW1_2022",
      Month == 9 & Day == 6 ~ "HW2_2022",
      TRUE ~ NA_character_
    ),
    # First make the proper treatment format
    treatment = case_when(
      Tx == 2 ~ "2L_S",
      Tx == 4 ~ "4L_NS",
      TRUE ~ NA_character_
    ),
    # Then extract components
    water_amount = str_extract(treatment, "\\d+L"),
    stress_status = str_extract(treatment, "[NS]S")
  ) %>%
  filter(!is.na(heatwave_period)) %>%
  select(
    Block = Block_ID,
    Row = Row_ID,
    Vine = Vine_ID,
    treatment,
    water_amount,
    stress_status,
    WP = PSI,
    Month,
    Day,
    heatwave_period
  )





# Shift column names down by one row and rename the first column
process_hw1_2022 <- function(data) {
  # Promote first row to column names
  colnames(data) <- make.names(as.character(unlist(data[1, ])), unique = TRUE)
  data <- data[-1, ]  # Remove the first row after promoting it to column names
  
  # Rename 'block' to 'Block' to ensure consistency
  colnames(data)[colnames(data) == "block"] <- "Block"
  
  cat("\nProcessing HW1 2022 data...")
  cat("\nNumber of input rows:", nrow(data))
  
  # Continue with data processing
  data <- data %>%
    mutate(
      datetime = as.POSIXct("2022-08-17 12:00:00"),
      heatwave_period = "HW1_2022",
      Row_ID = as.character(row),
      Vine_ID = as.character(vine),
      Block = as.character(Block),
      Tleaf = as.numeric(TleafEB),
      Tair = as.numeric(Tair),
      A = as.numeric(A),
      E = as.numeric(E),
      gsw = as.numeric(gsw),
      VPDleaf = as.numeric(VPDleaf)
    ) %>%
    filter(!is.na(Block), !is.na(Row_ID), !is.na(Vine_ID)) %>%
    assign_treatments() %>%
    select(datetime, heatwave_period, Block, Row_ID, Vine_ID,
           treatment, water_amount, stress_status,
           Tleaf, Tair, A, E, gsw, VPDleaf) %>%
    mutate(
      cooling_capacity = Tleaf - Tair
    ) %>%
    filter(!if_all(c(Tleaf, Tair, A, E, gsw, VPDleaf), is.na))
  
  cat("\nFinal number of processed rows:", nrow(data))
  return(data)
}


# HW2 2022: Directly use existing column names without promoting row 1
process_hw2_2022 <- function(data) {
  # Directly use existing column names
  colnames(data)[1] <- "time_id"  # Ensure first column is named correctly
  
  cat("\nProcessing HW2 2022 data...")
  cat("\nNumber of input rows:", nrow(data))
  
  data <- data %>%
    mutate(
      datetime = as.POSIXct("2022-09-06 12:00:00"),
      heatwave_period = "HW2_2022",
      Row_ID = as.character(row),
      Vine_ID = as.character(vine),
      Block = as.character(Block),
      Tleaf = as.numeric(TleafEB),
      Tair = as.numeric(Tair),
      A = as.numeric(A),
      E = as.numeric(E),
      gsw = as.numeric(gsw),
      VPDleaf = as.numeric(VPDleaf)
    ) %>%
    filter(!is.na(Block), !is.na(Row_ID), !is.na(Vine_ID)) %>%
    assign_treatments() %>%
    select(datetime, heatwave_period, Block, Row_ID, Vine_ID,
           treatment, water_amount, stress_status,
           Tleaf, Tair, A, E, gsw, VPDleaf) %>%
    mutate(
      cooling_capacity = Tleaf - Tair
    ) %>%
    filter(!if_all(c(Tleaf, Tair, A, E, gsw, VPDleaf), is.na))
  
  cat("\nFinal number of processed rows:", nrow(data))
  return(data)
}

# HW 2023: Directly use existing column names without promoting row 1
process_hw_2023 <- function(data) {
  # Directly use existing column names
  colnames(data)[1] <- "time_id"  # Ensure first column is named correctly
  
  cat("\nProcessing HW 2023 data...")
  cat("\nNumber of input rows:", nrow(data))
  
  data <- data %>%
    mutate(
      datetime = as.POSIXct(paste(date, Time_ID), format = "%Y%m%d %I%p"),
      heatwave_period = "HW_2023",
      Row_ID = as.character(Row),
      Vine_ID = as.character(Vine),
      Block = as.character(Block),
      Tleaf = as.numeric(TleafEB),
      Tair = as.numeric(Tair),
      A = as.numeric(A),
      E = as.numeric(E),
      gsw = as.numeric(gsw),
      VPDleaf = as.numeric(VPDleaf)
    ) %>%
    filter(!is.na(Block), !is.na(Row_ID), !is.na(Vine_ID)) %>%
    assign_treatments() %>%
    select(datetime, heatwave_period, Block, Row_ID, Vine_ID,
           treatment, water_amount, stress_status,
           Tleaf, Tair, A, E, gsw, VPDleaf) %>%
    mutate(
      cooling_capacity = Tleaf - Tair
    ) %>%
    filter(!if_all(c(Tleaf, Tair, A, E, gsw, VPDleaf), is.na))
  
  cat("\nFinal number of processed rows:", nrow(data))
  return(data)
}

# ========================================================================
# Process All Datasets
# ========================================================================
cat("\nProcessing all datasets...")
hw1_2022 <- process_hw1_2022(NSCs_1st_heatwave_August_17_2022_compiled)
hw2_2022 <- process_hw2_2022(NSCs_2nd_heatwave_September_6_2022_compiled)
hw_2023 <- process_hw_2023(X2023_comb_Licor)

# Combine with validation
all_hw_data <- bind_rows(hw1_2022, hw2_2022, hw_2023) %>%
  mutate(
    gsw = ifelse(gsw < 0, 0, gsw),  # Clamp minimum
    gsw = ifelse(gsw > 1, 1, gsw)   # Clamp maximum
  ) %>%
  arrange(datetime) %>%
  filter(!if_all(c(Tleaf, Tair, A, E, gsw, VPDleaf), is.na))

# Print validation summary
cat("\n\nData Processing Summary:")
cat("\nHW1 2022 rows:", nrow(hw1_2022))
cat("\nHW2 2022 rows:", nrow(hw2_2022))
cat("\nHW 2023 rows:", nrow(hw_2023))
cat("\nTotal combined rows:", nrow(all_hw_data))
cat("\n")

# Statistical Analysis Functions

# Function to run mixed effects model for physiological responses using nlme
analyze_treatment_effects <- function(data, response_var) {
  tryCatch({
    # Convert Block and heatwave_period to factors if they aren't already
    data$Block <- as.factor(data$Block)
    data$heatwave_period <- as.factor(data$heatwave_period)
    data$water_amount <- as.factor(data$water_amount)
    data$stress_status <- as.factor(data$stress_status)
    
    # Create the model formula for nlme
    formula <- as.formula(paste(response_var, 
                                "~ water_amount * stress_status"))
    
    # Fit the mixed effects model with nlme
    model <- lme(formula, 
                 random = ~ 1 | Block/heatwave_period,
                 data = data,
                 na.action = na.omit)
    
    # Get ANOVA results using nlme's anova function
    anova_results <- anova(model, type = "marginal")
    
    # Calculate estimated marginal means
    # Note: emmeans works with nlme models too
    emm <- emmeans(model, ~ water_amount * stress_status)
    
    list(model = model, anova = anova_results, emm = emm)
  }, error = function(e) {
    warning(paste("Error analyzing", response_var, ":", e$message))
    return(NULL)
  })
}

# Run analyses for key variables
tleaf_analysis <- analyze_treatment_effects(all_hw_data, "Tleaf")
cooling_analysis <- analyze_treatment_effects(all_hw_data, "cooling_capacity")
gsw_analysis <- analyze_treatment_effects(all_hw_data, "gsw")

# If you want to extract fixed effects specifically, you can do so for each analysis:
tleaf_fixed_effects <- summary(tleaf_analysis$model)$tTable
cooling_fixed_effects <- summary(cooling_analysis$model)$tTable
gsw_fixed_effects <- summary(gsw_analysis$model)$tTable

# Print the fixed effects if needed
print("Leaf Temperature Fixed Effects:")
print(tleaf_fixed_effects)

print("Cooling Capacity Fixed Effects:")
print(cooling_fixed_effects)

print("Stomatal Conductance Fixed Effects:")
print(gsw_fixed_effects)

# Create summary statistics
summary_stats <- all_hw_data %>%
  group_by(heatwave_period, treatment) %>%
  summarise(
    across(c(Tleaf, cooling_capacity, gsw, VPDleaf), 
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE),
             n = ~sum(!is.na(.))
           ),
           .names = "{.col}_{.fn}")
  )

# Function to extract and format model results
format_model_results <- function(model_analysis, var_name) {
  if(is.null(model_analysis)) {
    return(paste("Analysis failed for", var_name))
  }
  
  # Extract fixed effects inside the function
  fixed_effects <- summary(model_analysis$model)$tTable
  
  # Extract random effects
  rand_effects <- VarCorr(model_analysis$model)
  
  # Format results
  cat("\n=== Analysis Results for", var_name, "===\n")
  cat("\nFixed Effects:\n")
  print(fixed_effects)
  cat("\nRandom Effects:\n")
  print(rand_effects)
  cat("\nANOVA Results:\n")
  print(model_analysis$anova)
  cat("\nEstimated Marginal Means:\n")
  print(summary(model_analysis$emm))
}

# Now you can use the function as before:
format_model_results(tleaf_analysis, "Leaf Temperature")
format_model_results(cooling_analysis, "Cooling Capacity")
format_model_results(gsw_analysis, "Stomatal Conductance")

print("Summary statistics:")
print(summary_stats)

# Visualization Functions

# Create custom theme for consistent plotting
theme_custom <- theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "bottom"
  )

# Function to create treatment response plots
plot_treatment_response <- function(data, y_var, y_lab, title) {
  ggplot(data, aes(x = water_amount, y = .data[[y_var]], 
                   color = stress_status, fill = stress_status)) +
    geom_boxplot(alpha = 0.3) +
    facet_wrap(~heatwave_period) +
    labs(
      title = title,
      x = "Irrigation Treatment",
      y = y_lab,
      color = "Stress Status",
      fill = "Stress Status"
    ) +
    theme_custom
}

# Create main visualization plots
leaf_temp_plot <- plot_treatment_response(
  all_hw_data, 
  "Tleaf",
  "Leaf Temperature (°C)",
  "Leaf Temperature Response to Treatments During Heatwaves"
)

cooling_plot <- plot_treatment_response(
  all_hw_data,
  "cooling_capacity",
  "Cooling Capacity (Tleaf - Tair, °C)",
  "Leaf Cooling Capacity Across Treatments During Heatwaves"
)

gsw_plot <- plot_treatment_response(
  all_hw_data,
  "gsw",
  "Stomatal Conductance (mol H₂O m⁻² s⁻¹)",
  "Stomatal Conductance Response to Treatments During Heatwaves"
)

# Create a plot showing the relationship between cooling and stomatal conductance
cooling_gsw_relationship <- ggplot(all_hw_data, 
                                   aes(x = gsw, y = cooling_capacity, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~heatwave_period) +
  labs(
    title = "Relationship Between Cooling Capacity and Stomatal Conductance",
    x = "Stomatal Conductance (mol H₂O m⁻² s⁻¹)",
    y = "Cooling Capacity (Tleaf - Tair, °C)"
  ) +
  theme_custom

# Display plots and statistical summaries
print(leaf_temp_plot)
print(cooling_plot)
print(gsw_plot)
print(cooling_gsw_relationship)

# Summarize statistical results
summary(tleaf_analysis$model)
summary(cooling_analysis$model)
summary(gsw_analysis$model)


  # Create directories if they don't exist
  dir.create("figures", showWarnings = FALSE)
  dir.create("tables", showWarnings = FALSE)
  
  # Function to create water potential visualization
  create_wp_plot <- function(wp_2022, wp_2023) {
    # Combine datasets
    wp_combined <- bind_rows(
      wp_2022 %>% mutate(year = "2022"),
      wp_2023 %>% mutate(year = "2023")
    )
    
    # Create time of day plot
    wp_time_plot <- ggplot(wp_2023, 
                           aes(x = time, y = WP, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      labs(
        title = "Diurnal Water Potential Patterns",
        x = "Time of Day",
        y = "Water Potential (MPa)",
        color = "Treatment"
      ) +
      theme_custom +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Create treatment comparison plot
    wp_treatment_plot <- ggplot(wp_combined, 
                                aes(x = water_amount, y = WP, 
                                    color = stress_status, fill = stress_status)) +
      geom_boxplot(alpha = 0.3) +
      facet_wrap(~heatwave_period) +
      labs(
        title = "Water Potential Response to Treatments",
        x = "Irrigation Treatment",
        y = "Water Potential (MPa)",
        color = "Stress Status",
        fill = "Stress Status"
      ) +
      theme_custom
    
    list(time_plot = wp_time_plot, treatment_plot = wp_treatment_plot)
  }
  
  # Function to export all figures
  save_all_figures <- function() {
    if (exists("wp_2023")) {
      wp_plots <- create_wp_plot(wp_2022, wp_2023)
      
      ggsave(
        "figures/water_potential_diurnal.png",
        plot = wp_plots$time_plot,
        width = 10, height = 6,
        dpi = 300
      )
      
      ggsave(
        "figures/water_potential_treatment.png",
        plot = wp_plots$treatment_plot,
        width = 10, height = 7,
        dpi = 300
      )
    } else {
      cat("\nSkipping water potential plots. wp_2023 not found.")
    }
    
    # Save gas exchange plots regardless
    ggsave(
      "figures/leaf_temperature_response.png",
      plot = leaf_temp_plot,
      width = 10, height = 7,
      dpi = 300
    )
    
    ggsave(
      "figures/cooling_capacity_response.png",
      plot = cooling_plot,
      width = 10, height = 7,
      dpi = 300
    )
    
    ggsave(
      "figures/stomatal_conductance_response.png",
      plot = gsw_plot,
      width = 10, height = 7,
      dpi = 300
    )
    
    ggsave(
      "figures/cooling_gsw_relationship.png",
      plot = cooling_gsw_relationship,
      width = 12, height = 7,
      dpi = 300
    )
  }
  
  save_all_tables <- function() {
    # Save gas exchange summary statistics
    write.csv(
      summary_stats,
      "tables/gas_exchange_summary.csv",
      row.names = FALSE
    )
    write.table(
      summary_stats,
      "tables/gas_exchange_summary.txt",
      row.names = FALSE,
      sep = "\t",
      quote = FALSE
    )
    
    # Save fixed effects tables
    write.csv(
      tleaf_fixed_effects,
      "tables/leaf_temperature_fixed_effects.csv",
      row.names = TRUE
    )
    write.table(
      tleaf_fixed_effects,
      "tables/leaf_temperature_fixed_effects.txt",
      row.names = TRUE,
      sep = "\t",
      quote = FALSE
    )
    
    write.csv(
      cooling_fixed_effects,
      "tables/cooling_capacity_fixed_effects.csv",
      row.names = TRUE
    )
    write.table(
      cooling_fixed_effects,
      "tables/cooling_capacity_fixed_effects.txt",
      row.names = TRUE,
      sep = "\t",
      quote = FALSE
    )
    
    write.csv(
      gsw_fixed_effects,
      "tables/stomatal_conductance_fixed_effects.csv",
      row.names = TRUE
    )
    write.table(
      gsw_fixed_effects,
      "tables/stomatal_conductance_fixed_effects.txt",
      row.names = TRUE,
      sep = "\t",
      quote = FALSE
    )
    
    # Save ANOVA results
    write.csv(
      data.frame(
        tleaf_analysis$anova,
        row.names = rownames(tleaf_analysis$anova)
      ),
      "tables/leaf_temperature_anova.csv"
    )
    write.table(
      data.frame(
        tleaf_analysis$anova,
        row.names = rownames(tleaf_analysis$anova)
      ),
      "tables/leaf_temperature_anova.txt",
      sep = "\t",
      quote = FALSE
    )
    
    write.csv(
      data.frame(
        cooling_analysis$anova,
        row.names = rownames(cooling_analysis$anova)
      ),
      "tables/cooling_capacity_anova.csv"
    )
    write.table(
      data.frame(
        cooling_analysis$anova,
        row.names = rownames(cooling_analysis$anova)
      ),
      "tables/cooling_capacity_anova.txt",
      sep = "\t",
      quote = FALSE
    )
    
    write.csv(
      data.frame(
        gsw_analysis$anova,
        row.names = rownames(gsw_analysis$anova)
      ),
      "tables/stomatal_conductance_anova.csv"
    )
    write.table(
      data.frame(
        gsw_analysis$anova,
        row.names = rownames(gsw_analysis$anova)
      ),
      "tables/stomatal_conductance_anova.txt",
      sep = "\t",
      quote = FALSE
    )
    
    # Save EMM results
    write.csv(
      data.frame(summary(tleaf_analysis$emm)),
      "tables/leaf_temperature_emmeans.csv",
      row.names = FALSE
    )
    write.table(
      data.frame(summary(tleaf_analysis$emm)),
      "tables/leaf_temperature_emmeans.txt",
      row.names = FALSE,
      sep = "\t",
      quote = FALSE
    )
    
    write.csv(
      data.frame(summary(cooling_analysis$emm)),
      "tables/cooling_capacity_emmeans.csv",
      row.names = FALSE
    )
    write.table(
      data.frame(summary(cooling_analysis$emm)),
      "tables/cooling_capacity_emmeans.txt",
      row.names = FALSE,
      sep = "\t",
      quote = FALSE
    )
    
    write.csv(
      data.frame(summary(gsw_analysis$emm)),
      "tables/stomatal_conductance_emmeans.csv",
      row.names = FALSE
    )
    write.table(
      data.frame(summary(gsw_analysis$emm)),
      "tables/stomatal_conductance_emmeans.txt",
      row.names = FALSE,
      sep = "\t",
      quote = FALSE
    )
    
    # Save water potential summary statistics
    if (exists("wp_2023")) {
      wp_summary <- bind_rows(
        wp_2022 %>% 
          group_by(heatwave_period, water_amount, stress_status) %>%
          summarise(
            wp_mean = mean(WP, na.rm = TRUE),
            wp_sd = sd(WP, na.rm = TRUE),
            wp_n = sum(!is.na(WP))
          ),
        wp_2023 %>%
          group_by(heatwave_period, water_amount, stress_status, time) %>%
          summarise(
            wp_mean = mean(WP, na.rm = TRUE),
            wp_sd = sd(WP, na.rm = TRUE),
            wp_n = sum(!is.na(WP))
          )
      )
      
      write.csv(
        wp_summary,
        "tables/water_potential_summary.csv",
        row.names = FALSE
      )
      write.table(
        wp_summary,
        "tables/water_potential_summary.txt",
        row.names = FALSE,
        sep = "\t",
        quote = FALSE
      )
    } else {
      cat("\nSkipping water potential tables. wp_2023 not found.")
    }
  }
  
  
  
  # Execute export functions
  save_all_figures()
  save_all_tables()
  
  # Print confirmation
  cat("\nExport complete!")
  cat("\nFigures saved to 'figures' directory")
  cat("\nTables saved to 'tables' directory")