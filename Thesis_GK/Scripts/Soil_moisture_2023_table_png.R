# Load required libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(scales)
library(gt)
library(webshot2)

# Step 1: Load the 2022 Data
soil_data <- sm_output_cleaned_2022

# Step 2: Create figures directory if needed
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Step 3: Reshape and Clean Data for 2022 (7/20 to 9/23)
soil_data_long <- soil_data %>%
  pivot_longer(
    cols = contains("Soil Moisture"),
    names_to = "Depth",
    values_to = "VWC"
  ) %>%
  mutate(
    Date = as.Date(`Date (PST)`, format = "%m/%d/%y"),
    Depth = factor(Depth, 
                   levels = paste0("Soil Moisture ", 
                                   c("6in", "12in", "18in", "24in", "30in", "36in", "42in", "48in"), 
                                   " (%)")),
    Depth_num = as.numeric(gsub("[^0-9]", "", Depth)),
    Treatment = paste(Tx, Stress, sep = "_")
  ) %>%
  filter(Date >= as.Date("2022-07-20") & Date <= as.Date("2022-09-23")) %>% # Date range filter
  filter(VWC >= 0 & VWC <= 1) %>%  # Filter valid VWC values
  drop_na(VWC)

# Step 4: Define Plotting Functions

# Treatment Comparison Plot
treatment_comparison_plot <- function(data) {
  ggplot(data, aes(x = Treatment, y = VWC, fill = Stress)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
    facet_wrap(~Depth, scales = "free_y", ncol = 2) +
    scale_fill_viridis_d(end = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 10)
    ) +
    labs(
      x = "Treatment",
      y = "Volumetric Water Content",
      title = "Soil Moisture (VWC) Distribution by Treatment and Depth (2022)",
      subtitle = "Measurements from July 20 to September 23"
    )
}

# Temporal Patterns Plot
temporal_patterns_plot <- function(data) {
  ggplot(data %>% filter(Depth_num <= 24),
         aes(x = Date, y = VWC, color = Depth)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1, alpha = 0.5) +
    facet_wrap(~Treatment, scales = "free_y", ncol = 2) +
    scale_color_viridis_d(end = 0.8) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      x = "Date",
      y = "Volumetric Water Content",
      color = "Soil Depth",
      title = "Temporal Patterns in Soil Moisture (VWC) by Treatment (2022)",
      subtitle = "Showing top 24 inches of soil profile"
    )
}

# Depth Profile Plot
depth_profile_plot <- function(data) {
  ggplot(data, aes(x = Date, y = Depth_num, fill = VWC)) +
    geom_tile() +
    scale_fill_viridis(option = "magma", 
                       labels = scales::percent_format(accuracy = 0.1)) +
    scale_y_continuous(
      breaks = c(6, 12, 18, 24, 30, 36, 42, 48),
      labels = c("6", "12", "18", "24", "30", "36", "42", "48")
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text = element_text(size = 10),
      panel.grid = element_blank(),
      legend.position = "right"
    ) +
    labs(
      x = "Date",
      y = "Soil Depth (inches)",
      fill = "VWC",
      title = "Soil Moisture (VWC) Distribution by Depth (2022)",
      subtitle = "Measurements from July 20 to September 23"
    )
}

# Step 5: Generate and Save Plots
p1 <- treatment_comparison_plot(soil_data_long)
p2 <- temporal_patterns_plot(soil_data_long)
p3 <- depth_profile_plot(soil_data_long)

ggsave("figures/soil_vwc_treatment_comparison_2022.png", p1, width = 10, height = 8, dpi = 300)
ggsave("figures/soil_vwc_temporal_patterns_2022.png", p2, width = 12, height = 8, dpi = 300)
ggsave("figures/soil_vwc_depth_profile_2022.png", p3, width = 10, height = 8, dpi = 300)

# Step 6: Generate Summary Statistics Table
summary_stats <- soil_data_long %>%
  mutate(
    Depth_Order = as.numeric(gsub("[^0-9]", "", Depth)),
    Treatment = case_when(
      Stress == "NS" ~ paste0(Tx, " (Non-Stressed)"),
      TRUE ~ paste0(Tx, " (Stressed)")
    )
  ) %>%
  group_by(Treatment, Depth, Depth_Order) %>%
  summarise(
    `Mean VWC (%)` = sprintf("%.2f", mean(VWC * 100, na.rm = TRUE)),
    `SD (%)` = sprintf("%.2f", sd(VWC * 100, na.rm = TRUE)),
    `Min VWC (%)` = sprintf("%.2f", min(VWC * 100, na.rm = TRUE)),
    `Max VWC (%)` = sprintf("%.2f", max(VWC * 100, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Treatment, Depth_Order) %>%
  select(-Depth_Order)

# Create and Save Table as PNG
png_table <- summary_stats %>%
  gt() %>%
  tab_header(
    title = "Soil Moisture (VWC) Statistics by Treatment and Depth",
    subtitle = "July 20 - September 23, 2022"
  ) %>%
  opt_row_striping()

gtsave(png_table, "figures/soil_vwc_summary_table_2022.png", expand = 10)
