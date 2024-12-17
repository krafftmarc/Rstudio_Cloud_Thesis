# Required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readxl)
library(stringr)

# Treatment Mapping Table
treatment_map <- data.frame(
  block_prefix = c("BR02", "BR06", "BR11"),
  irrigation = c("2L", "4L", "4L"),
  stress = c("S", "S", "NS")
)

# Function to Assign Treatments
get_treatment_info <- function(block_id) {
  prefix <- str_extract(block_id, "^BR[0-9]+")
  match <- treatment_map %>% filter(block_prefix == prefix)
  if (nrow(match) == 1) {
    return(list(irrigation = match$irrigation, stress = match$stress))
  }
  return(list(irrigation = NA, stress = NA))
}

# Load Licor Data
load_licor_data <- function(data) {
  licor_data <- data %>%
    select(date, time = hhmmss...6, photo = A, cond = gsw, vpd = VPDleaf, Tx, block) %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%y"),
      datetime = as.POSIXct(paste(date, time)),
      hour = hour(datetime),
      treatment_info = map(block, get_treatment_info),
      irrigation = map_chr(treatment_info, "irrigation"),
      stress = map_chr(treatment_info, "stress")
    ) %>%
    filter(date %in% as.Date(c("2022-09-05", "2022-09-06"))) %>%
    select(-treatment_info)
  return(licor_data)
}

# Load Water Potential Data
load_wp_data <- function(data) {
  wp_data <- data %>%
    select(Month, Day, Year, Time, water_potential = PSI, block = Block_ID) %>%
    mutate(
      datetime = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00", 
                                    Year, Month, Day, 
                                    Time %/% 100, Time %% 100)),
      treatment_info = map(block, get_treatment_info),
      irrigation = map_chr(treatment_info, "irrigation"),
      stress = map_chr(treatment_info, "stress")
    ) %>%
    filter(Month == 9, Day %in% c(5, 6), Year == 2022) %>%
    select(-treatment_info)
  return(wp_data)
}

# Combine Data Efficiently
combine_data <- function(licor_data, wp_data, time_threshold = 15) {
  combined <- wp_data %>%
    left_join(licor_data, by = c("irrigation", "stress"), suffix = c("_wp", "_licor"))
  
  # Check column names
  print("Column names after join:")
  print(names(combined))
  
  combined <- combined %>%
    filter(abs(difftime(datetime_wp, datetime_licor, units = "mins")) <= time_threshold) %>%
    select(
      datetime = datetime_wp,
      hour = hour_licor,
      water_potential,
      cond,
      photo,
      vpd,
      irrigation,
      stress
    )
  
  return(combined)
}

# Analysis Function
analyze_treatment <- function(data) {
  summary <- data %>%
    group_by(irrigation, stress) %>%
    summarize(
      mean_gs = mean(cond, na.rm = TRUE),
      mean_photo = mean(photo, na.rm = TRUE),
      mean_wp = mean(water_potential, na.rm = TRUE),
      .groups = "drop"
    )
  return(summary)
}

# Plot Function
plot_data <- function(data) {
  ggplot(data, aes(x = water_potential, y = cond, color = as.factor(hour))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    facet_grid(stress ~ irrigation) +
    labs(
      title = "Stomatal Conductance vs. Water Potential",
      x = "Water Potential (MPa)",
      y = "Stomatal Conductance (mol m⁻² s⁻¹)",
      color = "Hour"
    ) +
    theme_bw()
}

# Run Workflow
# Assuming licor_2022 and Tyree_2022_WaterPotentials are loaded datasets
licor_data <- load_licor_data(licor_2022)
wp_data <- load_wp_data(Tyree_2022_WaterPotentials)

# Combine the datasets
combined_data <- combine_data(licor_data, wp_data)

# Verify Combined Data
print("Combined Data Preview:")
print(head(combined_data))

# Analyze Treatment Differences
treatment_summary <- analyze_treatment(combined_data)
print("Treatment Summary:")
print(treatment_summary)

# Plot Data
treatment_plot <- plot_data(combined_data)
ggsave("treatment_plot.png", treatment_plot, width = 10, height = 6)

# Save Summary
write.csv(treatment_summary, "treatment_summary.csv", row.names = FALSE)
