library(dplyr)
library(readr)
library(lubridate)
library(purrr)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockA/2022/6_48/")
file_list <- list.files(pattern = ".csv")
output_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockA/2022/6_48/6_48_avg/"
dir.create(output_folder, showWarnings = FALSE)

for (file in file_list) {
  data <- read_csv(file)  # Read the CSV file
  
  # Parse the "Date (PST)" column and extract date-time components
  data <- data %>%
    mutate(`Date (PST)` = mdy_hm(`Date (PST)`)) %>%
    mutate(Year = year(`Date (PST)`),
           Month = month(`Date (PST)`),
           Day = day(`Date (PST)`),
           Hour = hour(`Date (PST)`),
           Interval = floor(Hour / 3) * 3)
  
  # Group by the specified columns and calculate averages
  interval_averages <- data %>%
    group_by(Year, Month, Day, Interval, Block, Row, Zone, Tx, Stress) %>%
    summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)), .groups = 'drop')
  
  # Create a new file name for the output
  output_file <- file.path(output_folder, paste0("3hour_avg_", file))
  
  # Export the interval averages to a new CSV file
  write_csv(interval_averages, output_file)
}
