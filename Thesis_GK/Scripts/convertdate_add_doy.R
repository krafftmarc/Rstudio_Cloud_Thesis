library(lubridate)

# Load the CSV file
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/clean_licor_2023_extract.csv")

# Convert the date column to a standard date format
data$date <- ymd_hms(data$date)


# Convert the date column to a date format
data$date <- as.Date(data$date)

# Extract the day of the year from the date column
data$day_of_year <- yday(data$date)

# Write the updated data frame to a new CSV file
write.csv(data, "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/extracted_licor_2023_doy.csv", row.names = FALSE)
