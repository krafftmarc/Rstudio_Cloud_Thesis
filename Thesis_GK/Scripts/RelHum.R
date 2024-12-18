# Load necessary libraries
library(tidyverse)
library(lubridate)

# Step 1: Load the CIMIS dataset
# Replace with your actual dataset
cimis_data <- CIMIS_growing_season_2022

# Step 2: Preprocess the data
cleaned_data <- cimis_data %>%
  mutate(
    Date = mdy(Date),  # Convert Date to Date format
    Max_Humidity = as.numeric(`Max Rel Hum (%)`),  # Max Relative Humidity
    Min_Humidity = as.numeric(`Min Rel Hum (%)`)   # Min Relative Humidity
  ) %>%
  filter(month(Date) >= 4 & month(Date) <= 10)  # Filter for growing season (Apr-Oct)

# Step 3: Calculate Average Relative Humidity
humidity_data <- cleaned_data %>%
  mutate(
    Avg_Humidity = (Max_Humidity + Min_Humidity) / 2  # Average Relative Humidity
  )

# Step 4: Plot Relative Humidity Trends with Legend
humidity_plot <- ggplot(humidity_data, aes(x = Date)) +
  geom_line(aes(y = Max_Humidity, color = "Max Humidity"), size = 1) +  # Max RH
  geom_line(aes(y = Min_Humidity, color = "Min Humidity"), size = 1, linetype = "dashed") + # Min RH
  geom_line(aes(y = Avg_Humidity, color = "Avg Humidity"), size = 1, linetype = "dotdash") + # Avg RH
  scale_color_manual(
    values = c("Max Humidity" = "blue", "Min Humidity" = "red", "Avg Humidity" = "green"),
    name = "Legend"
  ) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    name = "Relative Humidity (%)",
    limits = c(0, 100),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title = "Daily Relative Humidity Trends - 2022 Season",
    subtitle = "Growing Season Data (April–October)",
    x = "Date",
    y = "Relative Humidity (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),  # Remove gridlines
    axis.line = element_line(color = "black", linewidth = 0.5),  # Add axis lines
    legend.position = "top",  # Place legend at the top
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# Step 5: Ensure 'figures' Folder Exists
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Step 6: Save the Plot in the 'figures' Folder
ggsave("figures/relative_humidity_trends_with_legend_2022.png", plot = humidity_plot, width = 8, height = 6, dpi = 600)

# Display the plot
print(humidity_plot)
