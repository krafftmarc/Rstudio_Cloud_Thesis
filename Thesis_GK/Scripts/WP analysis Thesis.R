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
    )
  
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
  }
  
  return(models)
}

# Update the create_publication_plots function

create_publication_plots <- function(data) {
  # Theme settings remain the same
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
  
  # Split seasonal trends into Pre-dawn and Midday
  seasonal_plot_predawn <- data %>%
    filter(Time_of_Day == "Pre-dawn") %>%
    ggplot(aes(x = Date, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "loess", se = TRUE) +
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
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(~Treatment, ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Midday Water Potential Trends",
      x = "Date",
      y = "Water Potential (MPa)"
    ) +
    publication_theme
  
  # Split temperature response into Pre-dawn and Midday
  temp_response_predawn <- data %>%
    filter(Time_of_Day == "Pre-dawn") %>%
    ggplot(aes(x = `Avg Air Temp (F)`, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
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
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~Treatment, ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Midday Water Potential Response to Temperature",
      x = "Average Air Temperature (°F)",
      y = "Water Potential (MPa)"
    ) +
    publication_theme
  
  # Updated boxplot separating by time period
  treatment_boxplot <- data %>%
    filter(!is.na(Time_of_Day)) %>%  # Remove NA time values
    ggplot(aes(x = Treatment, y = PSI, fill = Variety)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~Time_of_Day) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Water Potential Distribution by Treatment",
      x = "Treatment",
      y = "Water Potential (MPa)"
    ) +
    publication_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # New boxplot for heatwave measurements
  heatwave_boxplot <- data %>%
    filter(is.na(Time_of_Day)) %>%  # Only NA time values
    ggplot(aes(x = Treatment, y = PSI, fill = Variety)) +
    geom_boxplot(alpha = 0.7) +
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

# Function to export tables as PNG
export_tables <- function(tables, filename_prefix = "water_potential_summary") {
  # Convert gt table to grob
  table_grob <- gtable::gtable_render(tables$formatted_table)
  
  # Save as PNG with white background
  png(paste0(filename_prefix, ".png"), 
      width = 10, height = 6, units = "in", res = 300)
  grid::grid.draw(table_grob)
  dev.off()
}

# Function to create output directories
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
  
  # Save boxplots (slightly different dimensions)
  ggsave("figures/treatment_boxplot.png", plots$treatment, 
         width = 10, height = 6, dpi = 300)
  ggsave("figures/heatwave_boxplot.png", plots$heatwave, 
         width = 10, height = 6, dpi = 300)
}

# Updated function to export tables
export_tables <- function(tables, filename_prefix = "water_potential_summary") {
  # Create directories if they don't exist
  create_output_dirs()
  
  # Full path for the table
  filepath <- file.path("tables", paste0(filename_prefix, ".png"))
  
  # Convert gt table to grob
  table_grob <- gtable::gtable_render(tables$formatted_table)
  
  # Save as PNG with white background
  png(filepath, width = 10, height = 6, units = "in", res = 300)
  grid::grid.draw(table_grob)
  dev.off()
}

# Example usage:
# After running your analysis:
 results <- main_analysis(wp_2022, wp_2023, cimis_2022, cimis_2023)

#Save all outputs
save_plots(results$plots)
export_tables(results$tables)

# Print the locations where files were saved
print_output_locations <- function() {
  cat("\nFiles have been saved to the following locations:\n")
  cat("\nFigures:\n")
  cat("- figures/seasonal_predawn.png\n")
  cat("- figures/seasonal_midday.png\n")
  cat("- figures/temperature_predawn.png\n")
  cat("- figures/temperature_midday.png\n")
  cat("- figures/treatment_boxplot.png\n")
  cat("- figures/heatwave_boxplot.png\n")
  cat("\nTables:\n")
  cat("- tables/water_potential_summary.png\n")
}