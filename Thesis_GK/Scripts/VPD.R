# Load necessary libraries
library(tidyverse)
library(lubridate)

# Step 1: Load the CIMIS dataset
# Replace with your actual dataset
cimis_data <- CIMIS_2023

# Step 2: Preprocess the data
cleaned_data <- cimis_data %>%
  mutate(
    Date = mdy(Date),  # Convert Date to Date format
    Max_Temp_C = (as.numeric(`Max Air Temp (F)`) - 32) * (5/9),  # Convert F to C
    Min_Temp_C = (as.numeric(`Min Air Temp (F)`) - 32) * (5/9)   # Convert F to C
  ) %>%
  filter(month(Date) >= 4 & month(Date) <= 10)  # Filter for growing season (Apr-Oct)

# Step 3: Calculate SVP and VPD
vpd_data <- cleaned_data %>%
  mutate(
    Avg_Temp_C = (Max_Temp_C + Min_Temp_C) / 2,  # Average temperature in C
    SVP = 6.1078 * 10^(7.5 * Avg_Temp_C / (Avg_Temp_C + 237.3)),  # SVP in mBars
    VPD = SVP - as.numeric(`Avg Vap Pres (mBars)`)  # VPD = SVP - VP
  )

# Step 4: Plot VPD Trends
vpd_plot <- ggplot(vpd_data, aes(x = Date, y = VPD)) +
  geom_line(color = "blue", size = 1) +  # Line plot for VPD
  geom_point(color = "darkblue", size = 0.8, alpha = 0.7) +  # Points for individual days
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    name = "Vapor Pressure Deficit (mBars)",
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title = "Daily Vapor Pressure Deficit (VPD) - 2023 Season",
    subtitle = "Growing Season Data (April–October)",
    x = "Date",
    y = "VPD (mBars)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),  # Remove gridlines
    axis.line = element_line(color = "black", linewidth = 0.5)  # Add axis lines
  )

# Step 5: Ensure 'figures' Folder Exists
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Step 6: Save the Plot in the 'figures' Folder
ggsave("figures/vpd_trends_2023.png", plot = vpd_plot, width = 8, height = 6, dpi = 600)

# Display the plot
print(vpd_plot)
