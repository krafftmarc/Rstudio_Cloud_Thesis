# Add this at the start of functions.R
message("Sourcing functions.R...")
# Part 1: Libraries and Data Loading Functions ----------------------------------------

# Load required libraries
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
library(corrplot)
library(gt)
library(ggpubr)
library(car)  # For type III ANOVA
library(multcomp)  # For multiple comparisons
library(performance)  # For model diagnostics
library(MuMIn)  # For R-squared calculations
library(grid)  # For table export
library(gtable)  # For table export
# Additional libraries needed
library(webshot2)  # for PNG export
library(gt)        # for table formatting

# Create output directories function
create_output_dirs <- function() {
  # Create figures directory if it doesn't exist
  if (!dir.exists("figures")) {
    dir.create("figures")
  }
  
  # Create tables directory if it doesn't exist
  if (!dir.exists("tables")) {
    dir.create("tables")
  }
}

# Load and validate Water Potential 2022 data
load_wp_2022 <- function() {
  wp_2022 <- Tyree_2022_Cleaned %>%
    dplyr::mutate(
      Date = as.Date(paste(Year, Month, Day, sep = "-")),
      Season = "2022",
      Variety = factor(Variety, levels = c("CH", "CS")),
      Time_of_Day = case_when(
        Time_id == "1PM" ~ "Midday",
        Time_id == "5AM" ~ "Pre-dawn",
        TRUE ~ Time_id
      ),
      Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday")),
      Block_ID = as.factor(Block_ID),
      Vine_ID = as.factor(Vine_ID),
      # Handle Tx values
      Tx = case_when(
        is.nan(as.numeric(as.character(Tx))) ~ "Baseline",
        is.na(Tx) ~ "Baseline",
        TRUE ~ as.character(Tx)
      ),
      Stress = NA_character_
    ) %>%
    dplyr::filter(!is.na(PSI), !is.na(Variety))
  
  return(wp_2022)
}

# Load and validate Water Potential 2023 data
load_wp_2023 <- function() {
  wp_2023 <- Clean_Tyree_Water_Potentials_2023 %>%
    dplyr::mutate(
      Date = date,
      Season = "2023",
      Variety = factor(variety, levels = c("CH", "CS")),
      Time_of_Day = case_when(
        time_block == 500 ~ "Pre-dawn",
        time_block == 1300 ~ "Midday",
        TRUE ~ as.character(time_block)
      ),
      Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday")),
      Block_ID = as.factor(block),
      Vine_ID = as.factor(vine_id),
      PSI = `Ψ`,
      Tx = ifelse(is.na(Tx), "Baseline", as.character(Tx)),
      pot_type = "Leaf"
    ) %>%
    dplyr::filter(!is.na(PSI), !is.na(Variety))
  
  # Create a new dataframe with only the columns we want
  wp_2023_selected <- wp_2023 %>%
    dplyr::transmute(
      Date = Date,
      Season = Season,
      Variety = Variety,
      Time_of_Day = Time_of_Day,
      Block_ID = Block_ID,
      Vine_ID = Vine_ID,
      Tx = Tx,
      PSI = PSI,
      pot_type = pot_type,
      Stress = Stress
    )
  
  return(wp_2023_selected)
}

# Process water potential data with comprehensive treatment standardization
process_water_potential <- function(data_2022, data_2023) {
  # Define common columns
  common_cols <- c("Date", "Season", "Variety", "Time_of_Day", 
                   "Block_ID", "Vine_ID", "Tx", "PSI", 
                   "pot_type", "Stress")
  
  # Process 2022 data
  data_2022_processed <- data_2022 %>%
    dplyr::select(all_of(common_cols))
  
  # Process 2023 data
  data_2023_processed <- data_2023 %>%
    dplyr::select(all_of(common_cols))
  
  # Combine datasets and standardize treatment labels
  combined_data <- bind_rows(data_2022_processed, data_2023_processed) %>%
    dplyr::mutate(
      Season = factor(Season),
      # Updated Tx standardization logic
      Tx = case_when(
        Tx %in% c("2", "2L", 2) ~ "2L",  # Handle "2" and its variations
        Tx %in% c("4", "4L", 4) ~ "4L",  # Handle "4" and its variations
        Tx %in% c("Baseline", NA, "") ~ "Baseline",  # Handle empty or NA values
        TRUE ~ as.character(Tx)  # Convert all other cases to characters
      ),
      # Ensure Tx is a factor with consistent levels
      Tx = factor(Tx, levels = c("Baseline", "2L", "4L")),
      # Create Treatment combining Tx and Stress
      Treatment = case_when(
        is.na(Stress) ~ as.character(Tx),
        TRUE ~ paste(Tx, Stress, sep = "_")
      )
    )
  
  # Optional: Check for unexpected Tx values and raise a warning
  unexpected_tx <- unique(combined_data$Tx[is.na(combined_data$Tx)])
  if (length(unexpected_tx) > 0) {
    warning("Unexpected Tx values detected: ", paste(unexpected_tx, collapse = ", "))
  }
  
  return(combined_data)
}


# Updated CIMIS loading functions with careful column handling
load_cimis_2022 <- function() {
  # First verify the raw data structure
  message("Initial 2022 CIMIS columns:")
  print(colnames(CIMIS_growing_season_2022))
  
  data_2022 <- CIMIS_growing_season_2022 %>%
    # First ensure we have the humidity column
    rename_with(
      ~ "avg_rel_hum", 
      matches("Avg Rel Hum \\(%\\)")  # Explicit regex for the column name
    ) %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = "2022",
      # Convert columns to numeric explicitly
      temp = as.numeric(`Avg Air Temp (F)`),
      rh = as.numeric(avg_rel_hum)
    ) %>%
    filter(!is.na(temp), !is.na(rh))
  
  # Debug output
  message("\nProcessed 2022 CIMIS data structure:")
  print(str(data_2022))
  
  return(data_2022)
}

load_cimis_2023 <- function() {
  # First verify the raw data structure
  message("Initial 2023 CIMIS columns:")
  print(colnames(CIMIS_2023))
  
  data_2023 <- CIMIS_2023 %>%
    # First ensure we have the humidity column
    rename_with(
      ~ "avg_rel_hum", 
      matches("Avg Rel Hum \\(%\\)")  # Explicit regex for the column name
    ) %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = "2023",
      # Convert columns to numeric explicitly
      temp = as.numeric(`Avg Air Temp (F)`),
      rh = as.numeric(avg_rel_hum)
    ) %>%
    filter(!is.na(temp), !is.na(rh))
  
  # Debug output
  message("\nProcessed 2023 CIMIS data structure:")
  print(str(data_2023))
  
  return(data_2023)
}

# Debug function for CIMIS data
debug_cimis_data <- function(data, year) {
  message("\nDebugging CIMIS ", year, " data:")
  message("Column names:")
  print(names(data))
  message("\nFirst few rows of humidity columns:")
  print("Raw avg_rel_hum:")
  print(head(data$avg_rel_hum))
  print("\nRaw Avg Rel Hum (%):")
  print(head(data[,"Avg Rel Hum (%)"]))
}

# Updated CIMIS processing function
process_cimis <- function(cimis_2022, cimis_2023) {
  message("\nStarting CIMIS processing...")
  data_2022 <- cimis_2022 %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = factor("2022"),
      Temp_C = (as.numeric(`Avg Air Temp (F)`) - 32) * 5/9,
      RH = as.numeric(avg_rel_hum)
    ) %>%
    dplyr::transmute(
      Date = Date,
      Season = Season,
      Temp_C = Temp_C,
      RH = RH,
      VPD = calculate_vpd(Temp_C, RH)
    )
  
  data_2023 <- cimis_2023 %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = factor("2023"),
      Temp_C = (as.numeric(`Avg Air Temp (F)`) - 32) * 5/9,
      RH = as.numeric(avg_rel_hum)
    ) %>%
    dplyr::transmute(
      Date = Date,
      Season = Season,
      Temp_C = Temp_C,
      RH = RH,
      VPD = calculate_vpd(Temp_C, RH)
    )
  
  combined_cimis <- dplyr::bind_rows(data_2022, data_2023) %>%
    dplyr::arrange(Date)
  
  # Debug combined_cimis
  message("\nFirst few rows of combined_cimis:")
  print(head(combined_cimis))
  
  return(combined_cimis)
}



# Keep calculate_vpd function unchanged
calculate_vpd <- function(temp_C, RH) {
  # Calculate saturation vapor pressure (es) using Tetens equation
  es <- 0.611 * exp((17.27 * temp_C) / (temp_C + 237.3))
  # Calculate actual vapor pressure (ea)
  ea <- es * (RH / 100)
  # Calculate VPD in kPa
  vpd <- es - ea
  return(vpd)
}





# Function for advanced statistical analysis
# Location: functions.R
# Find the existing perform_statistical_analysis function and replace it with:
message("Columns in cimis_data:")
print(colnames(cimis_data))

perform_statistical_analysis <- function(combined_data, cimis_data) {
  # Merge water potential and environmental data
  analysis_data <- combined_data %>%
    left_join(cimis_data %>% dplyr::select(Date, Temp_C, VPD), by = "Date")
  
  # Basic Analysis with VPD
  basic_stats <- analysis_data %>%
    group_by(Season, Variety, Tx, Time_of_Day) %>%
    summarise(
      n = n(),
      mean_psi = mean(PSI, na.rm = TRUE),
      sd_psi = sd(PSI, na.rm = TRUE),
      mean_vpd = mean(VPD, na.rm = TRUE),
      sd_vpd = sd(VPD, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Correlation analysis
  correlations <- analysis_data %>%
    group_by(Season, Variety, Time_of_Day) %>%
    summarise(
      temp_cor = cor(PSI, Temp_C, use = "complete.obs"),
      vpd_cor = cor(PSI, VPD, use = "complete.obs"),
      .groups = 'drop'
    )
  
  # Mixed effects models including VPD
  models <- list()
  for(time in c("Pre-dawn", "Midday")) {
    subset_data <- analysis_data %>%
      filter(Time_of_Day == time) %>%
      mutate(across(c(Tx, Season, Variety), factor))
    
    # Model with VPD
    model <- lmer(PSI ~ Tx + Variety + VPD + (1|Block_ID),
                  data = subset_data,
                  control = lmerControl(check.nobs.vs.nlev = "ignore",
                                        check.nobs.vs.rankZ = "ignore",
                                        check.nobs.vs.nRE = "ignore"))
    models[[time]] <- model
  }
  
  # Save results
  sink("tables/statistical_analysis_with_vpd.txt")
  cat("STATISTICAL ANALYSIS RESULTS INCLUDING VPD\n")
  cat("=========================================\n\n")
  cat("1. CORRELATIONS\n")
  print(correlations)
  cat("\n\n")
  cat("2. MIXED EFFECTS MODEL RESULTS\n")
  for(time in c("Pre-dawn", "Midday")) {
    cat(sprintf("\n%s Analysis:\n", time))
    print(summary(models[[time]]))
    cat("\n")
  }
  sink()
  
  return(list(
    basic_stats = basic_stats,
    correlations = correlations,
    models = models
  ))
}


# Part 3: Visualization Functions ------------------------------------------------

# Create publication plots with temperature and VPD
create_comparison_plots <- function(combined_data, cimis_data) {
  plots <- list()
  
  for (var in c("CH", "CS")) {
    for (year in c("2022", "2023")) {
      # Subset water potential data
      subset_wp <- combined_data %>%
        filter(
          Variety == var,
          pot_type == "Stem",
          Season == year
        ) %>%
        filter(!is.na(Date))
      
      # Subset matching CIMIS data
      subset_cimis <- cimis_data %>%
        filter(Date %in% subset_wp$Date)
      
      if (nrow(subset_wp) > 0 && nrow(subset_cimis) > 0) {
        # Create plot
        p1 <- ggplot() +
          geom_point(data = subset_wp, 
                     aes(x = Date, y = PSI, color = Time_of_Day, shape = Tx),
                     alpha = 0.7, size = 3) +
          geom_smooth(data = subset_wp,
                      aes(x = Date, y = PSI, color = Time_of_Day),
                      method = "lm", se = TRUE, alpha = 0.2) +
          geom_line(data = subset_cimis,
                    aes(x = Date, y = Temp_C / 10, linetype = "Temperature"),
                    color = "red", linewidth = 1) +
          geom_line(data = subset_cimis,
                    aes(x = Date, y = VPD / 2, linetype = "VPD"),
                    color = "blue", linewidth = 1) +
          scale_y_continuous(
            name = "Water Potential (MPa)",
            sec.axis = sec_axis(~ . * 10, name = "Temperature (°C)")
          ) +
          scale_linetype_manual(
            name = "Environmental Variables",
            values = c("Temperature" = "solid", "VPD" = "dashed")
          ) +
          scale_color_brewer(palette = "Set1") +
          labs(
            title = paste(var, year, "- Stem Water Potential with Temperature and VPD"),
            x = "Date",
            color = "Time of Day",
            shape = "Treatment"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            legend.box = "vertical",
            plot.title = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 11),
            panel.grid.major = element_line(color = "grey90")
          ) +
          scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d")
        
        # Add plot to the list
        plot_key <- paste(var, year, "stem_comparison", sep = "_")
        plots[[plot_key]] <- p1
        message("Created comparison plot for: ", plot_key)
      }
    }
  }
  
  return(plots)
}


# Save plots to specified directory
save_plots <- function(plots, output_dir = "figures") {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Loop through all plots in the list and save them
  lapply(names(plots), function(plot_name) {
    file_path <- file.path(output_dir, paste0(plot_name, ".png"))
    ggsave(
      filename = file_path,
      plot = plots[[plot_name]],
      width = 10,  # Adjust dimensions as needed
      height = 8
    )
    message("Saved plot: ", file_path)
  })
}




# Part 4: Table Creation and Summary Functions ------------------------------------------

# Create output directories function
create_output_dirs <- function() {
  if (!dir.exists("figures")) dir.create("figures")
  if (!dir.exists("tables")) dir.create("tables")
}

# Updated table creation function with NA handling
create_summary_tables <- function(data) {
  # Remove NA Time_of_Day entries before summarizing
  summary_stats <- data %>%
    filter(!is.na(Time_of_Day)) %>%  # Remove NA entries
    group_by(Time_of_Day, Variety, Treatment) %>%
    summarise(
      Mean_PSI = mean(PSI, na.rm = TRUE),
      SD_PSI = sd(PSI, na.rm = TRUE),
      SE_PSI = SD_PSI / sqrt(sum(!is.na(PSI))),
      Min_PSI = min(PSI, na.rm = TRUE),
      Max_PSI = max(PSI, na.rm = TRUE),
      n = sum(!is.na(PSI)),
      .groups = "drop"  # Explicitly drop grouping
    ) %>%
    filter(!is.infinite(Min_PSI))  # Remove any infinite values
  
  # Create heatwave summary separately
  heatwave_stats <- data %>%
    filter(is.na(Time_of_Day)) %>%
    group_by(Variety, Treatment) %>%
    summarise(
      Time_of_Day = "Heatwave",
      Mean_PSI = mean(PSI, na.rm = TRUE),
      SD_PSI = sd(PSI, na.rm = TRUE),
      SE_PSI = SD_PSI / sqrt(sum(!is.na(PSI))),
      Min_PSI = min(PSI, na.rm = TRUE),
      Max_PSI = max(PSI, na.rm = TRUE),
      n = sum(!is.na(PSI)),
      .groups = "drop"
    ) %>%
    filter(!is.infinite(Min_PSI))
  
  # Combine regular and heatwave stats
  all_stats <- bind_rows(summary_stats, heatwave_stats)
  
  # Format table using gt
  gt_table <- all_stats %>%
    gt() %>%
    fmt_number(
      columns = c(Mean_PSI, SD_PSI, SE_PSI, Min_PSI, Max_PSI),
      decimals = 2
    ) %>%
    tab_header(
      title = "Water Potential Summary Statistics",
      subtitle = "By Variety, Treatment, and Time of Day"
    ) %>%
    tab_options(
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = 2,
      table_body.border.top.style = "solid",
      table_body.border.top.width = 1
    )
  
  return(list(
    raw_stats = all_stats,
    formatted_table = gt_table
  ))
}

# Updated table export function that doesn't rely on gtable
export_tables <- function(tables, filename_prefix = "water_potential_summary") {
  # Create directories if they don't exist
  create_output_dirs()
  
  # Export raw statistics to CSV
  write.csv(tables$raw_stats, 
            file = file.path("tables", paste0(filename_prefix, "_raw.csv")),
            row.names = FALSE)
  
  # Export formatted table as HTML
  gtsave(tables$formatted_table,
         filename = file.path("tables", paste0(filename_prefix, ".html")))
  
  # Export formatted table as PNG (alternative to gtable_render)
  filepath <- file.path("tables", paste0(filename_prefix, ".png"))
  gt::gtsave(
    tables$formatted_table,
    filename = filepath,
    expand = 10
  )
}

# Function to save statistical results
save_statistical_results <- function(models) {
  # Create directories if they don't exist
  create_output_dirs()
  
  # Save to text file
  sink("tables/statistical_results.txt")
  
  cat("Statistical Analysis Results\n\n")
  cat("=============================\n\n")
  
  # Print Pre-dawn results
  cat("\nPRE-DAWN RESULTS\n")
  cat("----------------\n")
  summary_prewritten <- capture.output(summary(models$`Pre-dawn`))
  cat(summary_prewritten, sep = "\n")
  
  cat("\nType III ANOVA:\n")
  print(Anova(models$`Pre-dawn`, type = 3))
  
  # Print Midday results
  cat("\nMIDDAY RESULTS\n")
  cat("--------------\n")
  print(summary(models$Midday))
  cat("\nType III ANOVA:\n")
  print(Anova(models$Midday, type = 3))
  
  # Create separate tables for coefficients
  cat("\nModel Coefficients\n")
  cat("----------------\n")
  print(coef(summary(models$`Pre-dawn`)))
  print(coef(summary(models$Midday)))
  
  sink()
}

# Function to print output locations
print_output_locations <- function() {
  message("\nAnalysis complete! Files have been saved to:")
  message("\nFigures:")
  message("- figures/seasonal_predawn.png")
  message("- figures/seasonal_midday.png")
  message("- figures/temperature_predawn.png")
  message("- figures/temperature_midday.png")
  message("- figures/treatment_boxplot.png")
  message("- figures/heatwave_boxplot.png")
  message("\nTables:")
  message("- tables/water_potential_summary.png")
  message("- tables/statistical_results.txt")
}

