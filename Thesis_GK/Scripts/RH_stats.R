# Load required libraries
library(tidyverse)
library(stats)

# Calculate statistical measures for each year
calc_humidity_stats <- function(data, year) {
  # Calculate daily ranges
  daily_stats <- data %>%
    group_by(Date) %>%
    summarise(
      daily_range = max(RH) - min(RH),
      daily_mean = mean(RH),
      daily_sd = sd(RH)
    )
  
  # Calculate overall statistics
  stats <- list(
    mean_range = mean(daily_stats$daily_range),
    sd_range = sd(daily_stats$daily_range),
    overall_mean = mean(daily_stats$daily_mean),
    overall_sd = sd(daily_stats$daily_mean),
    cv = (sd(daily_stats$daily_mean) / mean(daily_stats$daily_mean)) * 100
  )
  
  return(stats)
}

# Perform F-test for variance comparison
compare_variances <- function(data_2022, data_2023) {
  var.test(data_2022$RH, data_2023$RH)
}

# Calculate summary statistics for each humidity metric
calc_metric_stats <- function(data, metric) {
  stats <- data %>%
    summarise(
      mean = mean({{metric}}, na.rm = TRUE),
      sd = sd({{metric}}, na.rm = TRUE),
      cv = (sd/mean) * 100
    )
  return(stats)
}

# Example usage:
stats_2022 <- calc_humidity_stats(humidity_2022, "2022")
stats_2023 <- calc_humidity_stats(humidity_2023, "2023")

# Compare variances between years
variance_test <- compare_variances(humidity_2022, humidity_2023)

# Calculate metrics for max, min, and avg humidity
max_stats_2022 <- calc_metric_stats(humidity_2022, max_RH)
min_stats_2022 <- calc_metric_stats(humidity_2022, min_RH)
avg_stats_2022 <- calc_metric_stats(humidity_2022, avg_RH)