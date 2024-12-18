# Load required libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(scales)
library(gt)
library(webshot2)

# Step 1: Load the Data
soil_data <- sm_output_cleaned_2023

# Create figures directory if needed
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Reshape and Clean Data for Plotting
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
  # Filter out nonsensical VWC values
  filter(VWC >= 0 & VWC <= 1) %>%
  drop_na(VWC)

# Define plotting functions
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
      panel.grid = element_blank(), # Remove all gridlines
      axis.line = element_line(color = "black"),
      strip.text = element_text(size = 10)
    ) +
    labs(
      x = "Treatment",
      y = "Volumetric Water Content",
      title = "Soil Moisture (VWC) Distribution by Treatment and Depth (2023)",
      subtitle = "Measurements from April 1 to September 23"
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
      panel.grid = element_blank(), # Remove all gridlines
      axis.line = element_line(color = "black"),
      legend.position = "bottom"
    ) +
    labs(
      x = "Date",
      y = "Volumetric Water Content",
      color = "Soil Depth",
      title = "Temporal Patterns in Soil Moisture (VWC) by Treatment (2023)",
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
      panel.grid = element_blank(), # Remove all gridlines
      axis.line = element_line(color = "black"),
      legend.position = "right"
    ) +
    labs(
      x = "Date",
      y = "Soil Depth (inches)",
      fill = "VWC",
      title = "Soil Moisture (VWC) Distribution by Depth (2023)",
      subtitle = "Measurements from April 1 to September 23"
    )
}

# Generate and save plots
p1 <- treatment_comparison_plot(soil_data_long)
p2 <- temporal_patterns_plot(soil_data_long)
p3 <- depth_profile_plot(soil_data_long)

# Save plots
ggsave("figures/soil_vwc_treatment_comparison_2023.png", p1, width = 10, height = 8, dpi = 300)
ggsave("figures/soil_vwc_temporal_patterns_2023.png", p2, width = 12, height = 8, dpi = 300)
ggsave("figures/soil_vwc_depth_profile_2023.png", p3, width = 10, height = 8, dpi = 300)

# Generate VWC summary statistics with cleaned data
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
    `Mean VWC` = sprintf("%.3f", mean(VWC, na.rm = TRUE)),
    `SD` = sprintf("%.3f", sd(VWC, na.rm = TRUE)),
    `Min VWC` = sprintf("%.3f", min(VWC, na.rm = TRUE)),
    `Max VWC` = sprintf("%.3f", max(VWC, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Depth = gsub("Soil Moisture ", "", Depth),
    Depth = gsub(" \\(%\\)", "", Depth)
  ) %>%
  arrange(Treatment, Depth_Order) %>%
  select(-Depth_Order)

# Create table using gt
png_table <- summary_stats %>%
  gt() %>%
  # Add title and subtitle
  tab_header(
    title = md("**Soil Moisture (VWC) Statistics by Treatment and Depth**"),
    subtitle = "April 01 - September 23, 2023"
  ) %>%
  # Format numeric columns to display as percentages
  fmt_percent(
    columns = c(`Mean VWC`, `Min VWC`, `Max VWC`),
    decimals = 1
  ) %>%
  fmt_number(
    columns = SD,
    decimals = 3
  ) %>%
  # Style the columns
  cols_align(
    align = "center",
    columns = c(Depth, `Mean VWC`, SD, `Min VWC`, `Max VWC`)
  ) %>%
  cols_align(
    align = "left",
    columns = Treatment
  ) %>%
  # Add borders
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "black",
        weight = px(1)
      )
    ),
    locations = cells_body()
  ) %>%
  # Style header
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom"),
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  # Add source note
  tab_source_note(
    source_note = md("*Note:* VWC = Volumetric Water Content. NS = Non-stressed, S = Stressed conditions. Values filtered to valid VWC range (0-1).")
  ) %>%
  # Customize appearance
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(14),
    source_notes.font.size = px(10),
    column_labels.font.weight = "bold",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2)
  )

# Save table as PNG
# First save as HTML
gtsave(png_table, "figures/temp_table.html")

# Then convert to PNG using webshot2
webshot2::webshot(
  url = "figures/temp_table.html",
  file = "figures/soil_moisture_table.png",
  delay = 0.1,
  vwidth = 1000,
  vheight = 800
)

# Clean up temporary HTML file
file.remove("figures/temp_table.html")
