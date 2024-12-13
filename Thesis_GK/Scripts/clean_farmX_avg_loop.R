library(dplyr)
library(readr)
library(lubridate)
library(purrr)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockB/SM/2022/RZ/")
file_list <- list.files(pattern = ".csv")
output_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockB/SM/2022/RZ_avg/"
# Create a folder to save the output files
dir.create(output_folder, showWarnings = FALSE)

for (file in file_list) {
  data <- read_csv(file)  # Read the CSV file
  
  # Convert the "Date (PST)" column to a datetime format
  data <- data %>% mutate(`Date (PST)` = as.Date(`Date (PST)`, format = "%m/%d/%y")) %>% mutate(Year = year(`Date (PST)`), Month = month(`Date (PST)`), Day = day(`Date (PST)`))
  
  # Calculate daily averages for each column
  daily_averages <- data %>%
    group_by(Year, Month, Day) %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  # Create a new file name for the output
  output_file <- file.path(output_folder, paste0("average_", file))
  
  # Export the daily averages to a new CSV file
  write_csv(daily_averages, output_file)
}
