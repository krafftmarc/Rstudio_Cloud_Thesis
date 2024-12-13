# Load necessary libraries
library(dplyr)
library(lubridate)
library(readxl)

# Read the datasets
data <- readxl::read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/water_pot copy/2023_pdwp.xlsx")
data_1 <- readxl::read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/water_pot copy/2023_1300wp.xlsx")

# Filter each dataset for the specific time blocks
data_500 <- filter(data, time_block == "500") %>%
  select(date, block, row, vine_id, water_potential) %>%
  rename(water_potential_500 = water_potential)

data_1300 <- filter(data_1, time_block == "1300") %>%
  select(date, block, row, vine_id, water_potential) %>%
  rename(water_potential_1300 = water_potential)

# Join the datasets. Use 'full_join' for all records or 'left_join' to keep only 1300 records
matched_data <- full_join(data_1300, data_500, by = c("date", "block", "row", "vine_id"))

# Calculate the water potential difference
matched_data$water_potential_diff <- matched_data$water_potential_1300 - matched_data$water_potential_500

# Optionally, write the result to a new CSV file
write.csv(matched_data, "water_potential_difference.csv", row.names = FALSE)
