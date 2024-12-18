# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)

# Step 1: Load and Clean Data
# Assuming your data is already loaded and cleaned
cleaned_data <- CIMIS_2023 %>%
  mutate(
    Date = mdy(Date),                   # Convert Date to Date format
    ETo = `ETo (in)`                    # Rename column for simplicity
  ) %>%
  select(Date, ETo) %>%
  filter(!is.na(ETo)) %>%
  filter(month(Date) >= 4 & month(Date) <= 10)  # Filter growing season (Apr–Oct)

# Step 2: Create the Plot with No Gridlines and Rotated X-axis Labels
eto_plot <- ggplot(cleaned_data, aes(x = Date, y = ETo)) +
  geom_line(color = "darkblue", linewidth = 1) +          # Line plot
  geom_point(color = "blue", alpha = 0.6, size = 1.5) +   # Data points
  scale_x_date(
    date_breaks = "1 week",            # Weekly ticks
    date_labels = "%b %d"              # Format: Month Day
  ) +
  scale_y_continuous(
    name = "Daily ETo (inches)",
    limits = c(0, NA),
    breaks = pretty_breaks(n = 6)
  ) +
  labs(
    title = "Daily Reference Evapotranspiration (ETo) for 2023",
    subtitle = "Growing Season Data (April–October)",
    x = "Date",
    y = "ETo (inches)"
  ) +
  theme(
    panel.background = element_blank(),          # Remove background
    panel.grid = element_blank(),                # Remove all gridlines
    axis.line = element_line(color = "black"),   # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    text = element_text(size = 16, family = "Arial"),   # Font adjustments
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

# Step 3: Ensure 'figures' Folder Exists
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Step 4: Save High-Quality Plot in 'figures' Folder
ggsave("figures/daily_eto_2023_clean.png", plot = eto_plot, width = 8, height = 6, dpi = 600, units = "in")

# Step 5: Display the Plot
print(eto_plot)
