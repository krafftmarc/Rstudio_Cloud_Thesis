# Load required libraries
library(tidyverse)
library(stats)

# Function to analyze temperature exceedances
analyze_temp_exceedance <- function(data, threshold = 100) {
  exceedance_stats <- data %>%
    summarise(
      days_exceeded = sum(max_temp >= threshold),
      max_exceeded_temp = max(max_temp),
      mean_exceeded_temp = mean(max_temp[max_temp >= threshold])
    )
  return(exceedance_stats)
}

# Calculate statistical measures for each year
calc_temp_stats <- function(data) {
  # Daily statistics
  daily_stats <- data %>%
    summarise(
      mean_max = mean(max_temp),
      sd_max = sd(max_temp),
      cv_max = sd(max_temp)/mean(max_temp) * 100,
      mean_min = mean(min_temp),
      sd_min = sd(min_temp),
      cv_min = sd(min_temp)/mean(min_temp) * 100,
      mean_avg = mean(avg_temp),
      sd_avg = sd(avg_temp),
      cv_avg = sd(avg_temp)/mean(avg_temp) * 100,
      mean_diurnal = mean(max_temp - min_temp),
      sd_diurnal = sd(max_temp - min_temp)
    )
  return(daily_stats)
}

# Perform statistical comparisons between years
compare_years <- function(data_2022, data_2023) {
  # t-test for mean differences
  max_test <- t.test(data_2022$max_temp, data_2023$max_temp)
  min_test <- t.test(data_2022$min_temp, data_2023$min_temp)
  avg_test <- t.test(data_2022$avg_temp, data_2023$avg_temp)
  
  return(list(max_test = max_test, 
              min_test = min_test, 
              avg_test = avg_test))
}