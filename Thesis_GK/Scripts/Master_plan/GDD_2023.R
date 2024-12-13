# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Load the data from the new CSV file for 2022
data <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022/Tyree LiCor Data 2022/Master_plan/CIMIS_2022_apr_sep.csv")  # Update the path accordingly

# Ensure the date column is properly formatted
data$Date <- as.Date(data$Date, format = "%m/%d/%y")

# Filter data for the growing season from April 1st to September 15th, 2022
data <- filter(data, Date >= as.Date("2022-04-01") & Date <= as.Date("2022-09-15"))

# Calculate daily Growing Degree Days (assuming base temp of 50°F)
data$GDD <- with(data, (`Max Air Temp (F)` + `Min Air Temp (F)`)/2 - 50)
data$GDD[data$GDD < 0] <- 0  # Set negative GDD values to 0

# Aggregate GDD by week
data <- data %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarize(Weekly_GDD = sum(GDD))

# Calculate cumulative GDD by week
data$Cumulative_GDD <- cumsum(data$Weekly_GDD)

# Plot cumulative weekly GDD as a bar graph
ggplot(data, aes(x = Week, y = Cumulative_GDD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cumulative Weekly Growing Degree Days (GDD) from April 1st to September 15th, 2022",
       x = "Week",
       y = "Cumulative Growing Degree Days") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
