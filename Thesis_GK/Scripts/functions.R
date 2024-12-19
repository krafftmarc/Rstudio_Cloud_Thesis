# Part 1: Libraries and Data Loading Functions ----------------------------------------

# Load required libraries
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
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
    mutate(
      Date = as.Date(paste(Year, Month, Day, sep = "-")),
      Variety = factor(Variety, levels = c("CH", "CS")),
      Time_of_Day = case_when(
        Time_id == "1PM" ~ "Midday",
        Time_id == "5AM" ~ "Pre-dawn",
        TRUE ~ Time_id
      ),
      Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday")),
      Block_ID = as.factor(Block_ID),
      Vine_ID = as.factor(Vine_ID),
      Tx = if_else(is.na(Tx), "2L", as.character(Tx)),  # Convert NaN to "2L"
      Stress = "Baseline"  # 2022 is baseline year
    ) %>%
    # Add data validation
    filter(!is.na(PSI), !is.na(Variety)) %>%
    arrange(Date)
  
  return(wp_2022)
}

# Load and validate Water Potential 2023 data
load_wp_2023 <- function() {
  wp_2023 <- Clean_Tyree_Water_Potentials_2023 %>%
    mutate(
      Date = date,  # already in correct format
      Variety = factor(variety, levels = c("CH", "CS")),
      Time_of_Day = case_when(
        time_block == 500 ~ "Pre-dawn",
        time_block == 1300 ~ "Midday",
        TRUE ~ as.character(time_block)
      ),
      Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday")),
      Block_ID = as.factor(block),
      Vine_ID = as.factor(vine_id),
      PSI = Ψ  # rename water potential column
    ) %>%
    # Add data validation
    filter(!is.na(PSI), !is.na(Variety), !is.na(Tx)) %>%
    arrange(Date)
  
  return(wp_2023)
}

# Load and validate CIMIS 2022 data
load_cimis_2022 <- function() {
  cimis_2022 <- CIMIS_growing_season_2022 %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = "2022"
    ) %>%
    # Add data validation
    filter(!is.na(`Avg Air Temp (F)`)) %>%
    arrange(Date)
  
  return(cimis_2022)
}

# Load and validate CIMIS 2023 data
load_cimis_2023 <- function() {
  cimis_2023 <- CIMIS_2023 %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = "2023"
    ) %>%
    # Add data validation
    filter(!is.na(`Avg Air Temp (F)`)) %>%
    arrange(Date)
  
  return(cimis_2023)
}

# Part 2: Data Consistency and Processing Functions ------------------------------------

# Function to check data consistency
check_data_consistency <- function(wp_2022, wp_2023, cimis_2022, cimis_2023) {
  # Check date ranges
  message("Checking date ranges...")
  print(paste("WP 2022 date range:", min(wp_2022$Date), "to", max(wp_2022$Date)))
  print(paste("WP 2023 date range:", min(wp_2023$Date), "to", max(wp_2023$Date)))
  print(paste("CIMIS 2022 date range:", min(cimis_2022$Date), "to", max(cimis_2022$Date)))
  print(paste("CIMIS 2023 date range:", min(cimis_2023$Date), "to", max(cimis_2023$Date)))
  
  # Check for missing values
  message("\nChecking for missing values...")
  print("WP 2022 missing values:")
  print(colSums(is.na(wp_2022)))
  print("WP 2023 missing values:")
  print(colSums(is.na(wp_2023)))
  
  # Check treatment levels
  message("\nChecking treatment levels...")
  print("2022 treatments:")
  print(table(wp_2022$Tx, wp_2022$Stress))
  print("2023 treatments:")
  print(table(wp_2023$Tx, wp_2023$Stress))
}

# Function to process water potential data
process_water_potential <- function(data_2022, data_2023) {
  # Process 2022 data
  wp_2022 <- data_2022 %>%
    mutate(
      Tx = as.character(Tx),
      Stress = ifelse(is.na(Stress), "Baseline", Stress),
      PSI = ifelse(is.na(PSI), -10, PSI),
      Season = "2022",
      Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday"))
    )
  
  # Process 2023 data
  wp_2023 <- data_2023 %>%
    mutate(
      Season = "2023",
      Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday"))
    ) %>%
    filter(!is.na(PSI), !is.na(Tx))  # Ensure no missing values for key columns
  
  # Combine datasets
  bind_rows(wp_2022, wp_2023) %>%
    mutate(
      Date = as.Date(Date),
      Season = as.factor(Season),
      Treatment = paste(Tx, Stress, sep = "_")  # Combined treatment variable
    )
}


# Function to process CIMIS data
process_cimis <- function(cimis_2022, cimis_2023) {
  bind_rows(
    cimis_2022 %>% mutate(Season = "2022"),
    cimis_2023 %>% mutate(Season = "2023")
  ) %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Season = as.factor(Season)
    )
}

# Function for advanced statistical analysis
perform_statistical_analysis <- function(data) {
  # Separate models for pre-dawn and midday
  models <- list()
  
  for(time in c("Pre-dawn", "Midday")) {
    subset_data <- filter(data, Time_of_Day == time)
    
    tryCatch({
      # Fit mixed model
      model <- lmer(PSI ~ Variety * Tx * Stress + 
                      `Avg Air Temp (F)` + 
                      (1 | Block_ID/Vine_ID),
                    data = subset_data)
      
      # Store model
      models[[time]] <- model
      
      # Print diagnostics
      cat("\nModel Diagnostics for", time, ":\n")
      print(check_model(model))
      
      # Type III ANOVA
      cat("\nType III ANOVA for", time, ":\n")
      print(Anova(model, type = 3))
      
      # EMM comparisons
      cat("\nEstimated Marginal Means for", time, ":\n")
      emm <- emmeans(model, ~ Variety * Tx * Stress)
      print(pairs(emm))
    }, error = function(e) {
      message("Error in ", time, " model: ", e$message)
    }, warning = function(w) {
      message("Warning in ", time, " model: ", w$message)
    })
  }
  
  return(models)
}

# Part 3: Visualization Functions ------------------------------------------------

# Updated visualization function with better smoothing parameters
create_publication_plots <- function(data) {
  # Theme settings
  publication_theme <- theme_classic() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      plot.background = element_blank()
    )
  
  # Split seasonal trends into Pre-dawn and Midday with robust smoothing
  seasonal_plot_predawn <- data %>%
    filter(Time_of_Day == "Pre-dawn") %>%
    ggplot(aes(x = Date, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7, size = 2) +
    stat_smooth(
      method = "loess",
      span = 0.5,  # Adjusted span
      se = TRUE,
      formula = y ~ x
    ) +
    facet_wrap(~Treatment, ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Pre-dawn Water Potential Trends",
      x = "Date",
      y = "Water Potential (MPa)"
    ) +
    publication_theme
  
  seasonal_plot_midday <- data %>%
    filter(Time_of_Day == "Midday") %>%
    ggplot(aes(x = Date, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7, size = 2) +
    stat_smooth(
      method = "loess",
      span = 0.5,  # Adjusted span
      se = TRUE,
      formula = y ~ x
    ) +
    facet_wrap(~Treatment, ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Midday Water Potential Trends",
      x = "Date",
      y = "Water Potential (MPa)"
    ) +
    publication_theme
  
  # Temperature response plots with linear smoothing
  temp_response_predawn <- data %>%
    filter(Time_of_Day == "Pre-dawn") %>%
    ggplot(aes(x = `Avg Air Temp (F)`, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE
    ) +
    facet_wrap(~Treatment, ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Pre-dawn Water Potential Response to Temperature",
      x = "Average Air Temperature (°F)",
      y = "Water Potential (MPa)"
    ) +
    publication_theme
  
  temp_response_midday <- data %>%
    filter(Time_of_Day == "Midday") %>%
    ggplot(aes(x = `Avg Air Temp (F)`, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE
    ) +
    facet_wrap(~Treatment, ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Midday Water Potential Response to Temperature",
      x = "Average Air Temperature (°F)",
      y = "Water Potential (MPa)"
    ) +
    publication_theme
  
  # Box plots with explicit NA handling
  treatment_boxplot <- data %>%
    filter(!is.na(Time_of_Day)) %>%
    ggplot(aes(x = Treatment, y = PSI, fill = Variety)) +
    geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75)) +
    facet_wrap(~Time_of_Day) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Water Potential Distribution by Treatment",
      x = "Treatment",
      y = "Water Potential (MPa)"
    ) +
    publication_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Heatwave boxplot
  heatwave_boxplot <- data %>%
    filter(is.na(Time_of_Day)) %>%
    ggplot(aes(x = Treatment, y = PSI, fill = Variety)) +
    geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.75)) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Heatwave Water Potential Distribution by Treatment",
      x = "Treatment",
      y = "Water Potential (MPa)"
    ) +
    publication_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    seasonal_predawn = seasonal_plot_predawn,
    seasonal_midday = seasonal_plot_midday,
    temperature_predawn = temp_response_predawn,
    temperature_midday = temp_response_midday,
    treatment = treatment_boxplot,
    heatwave = heatwave_boxplot
  ))
}
# Function to save all plots
save_plots <- function(plots, width = 12, height = 8) {
  # Create directories if they don't exist
  create_output_dirs()
  
  # Save seasonal plots
  ggsave("figures/seasonal_predawn.png", plots$seasonal_predawn, 
         width = width, height = height, dpi = 300)
  ggsave("figures/seasonal_midday.png", plots$seasonal_midday, 
         width = width, height = height, dpi = 300)
  
  # Save temperature response plots
  ggsave("figures/temperature_predawn.png", plots$temperature_predawn, 
         width = width, height = height, dpi = 300)
  ggsave("figures/temperature_midday.png", plots$temperature_midday, 
         width = width, height = height, dpi = 300)
  
  # Save boxplots
  ggsave("figures/treatment_boxplot.png", plots$treatment, 
         width = 10, height = 6, dpi = 300)
  ggsave("figures/heatwave_boxplot.png", plots$heatwave, 
         width = 10, height = 6, dpi = 300)
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
  print(summary(models$`Pre-dawn`))
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

