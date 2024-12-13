library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the data, skipping the first row
data_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022/Tyree LiCor Data 2022/Master_plan/licor_comb_2022.xlsx"  # Update this path
data <- read_excel(data_path)

# Assuming you've confirmed the correct column name for the Unix timestamp
# For this example, let's assume the column is named 'timestamp'
data <- data %>%
  mutate(datetime = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))
data$datetime
# Create the plot
p <- ggplot(data, aes(x = datetime, y = gsw, color = Variety)) +
  geom_point() +  # Plot data points
  geom_smooth(method = "loess", se = TRUE, aes(group = Variety)) +  # Add smooth trend line with CI
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue", size = 1) +  # Horizontal line at 0.05
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red", size = 1) +  # Horizontal line at 0.15
  scale_y_continuous(breaks = seq(min(data$gsw, na.rm = TRUE), max(data$gsw, na.rm = TRUE), by = 0.05)) +  # Y-axis in 0.05 increments
  labs(title = "Plot of GSW against Date-Time by Variety",
       x = "Date-Time",
       y = "GSW") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) +  # Remove minor grid lines
  scale_color_brewer(palette = "Set1")  # Color palette for better distinction

# Print the plot
print(p)
