# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Load the data
data <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022/Tyree LiCor Data 2022/Master_plan/CIMIS_2022_apr_sep.csv")

# Convert the 'Date' column to Date type
data$Date <- as.Date(data$Date, format = "%m/%d/%y")

# Filter data from April 1st to September 15th, 2022
data <- filter(data, Date >= as.Date("2022-04-01") & Date <= as.Date("2022-09-15"))

# Define Kc values for different periods
data <- data %>%
  mutate(Kc = case_when(
    Date < as.Date("2022-05-01") ~ 0.3,
    Date < as.Date("2022-06-15") ~ 0.54,
    Date < as.Date("2022-08-15") ~ 0.75,
    TRUE ~ 0.65
  ))

# Calculate ETc in inches
data <- mutate(data, ETc = ETo * Kc)

# Convert ETc from inches to gallons per acre
data <- mutate(data, ETc_gallons = ETc * 27154 * 1.6)

# Sum ETc by week
data <- data %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarize(Weekly_ETc_gallons = sum(ETc_gallons))

# Calculate cumulative ETc gallons
data$cumulative_ETc_gallons <- cumsum(data$Weekly_ETc_gallons)

# Plot cumulative ETc gallons as a bar graph with a smooth trendline and y-axis in increments of 50000
cumulative_plot <- ggplot(data, aes(x = Week, y = cumulative_ETc_gallons)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.5) + # Use semi-transparent bars for clarity
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "skyblue") + # Add a smooth trendline
  labs(title = "Cumulative ETc Gallons per Acre from April 1st to September 15th, 2022",
       x = "Week",
       y = "Cumulative Gallons per Acre") +
  scale_y_continuous(breaks = seq(0, max(data$cumulative_ETc_gallons), by = 50000)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Print the plot
print(cumulative_plot)
