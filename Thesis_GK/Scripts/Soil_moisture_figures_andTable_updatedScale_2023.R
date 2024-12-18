# Load required libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(scales)
library(kableExtra)

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
      panel.grid.minor = element_blank(),
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
    geom_tile(interpolate = TRUE) +
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


# Generate VWC summary statistics
summary_stats <- soil_data_long %>%
  mutate(
    Depth_Order = as.numeric(gsub("[^0-9]", "", Depth)),  # Extract numeric depth for ordering
    Treatment = case_when(
      Stress == "NS" ~ paste0(Tx, " (Non-Stressed)"),
      TRUE ~ paste0(Tx, " (Stressed)")
    )
  ) %>%
  group_by(Treatment, Depth, Depth_Order) %>%
  summarise(
    `Mean VWC (%)` = sprintf("%.2f", mean(VWC * 100, na.rm = TRUE)),  # Convert to %
    `SD (%)` = sprintf("%.2f", sd(VWC * 100, na.rm = TRUE)),          # Standard deviation
    `Min VWC (%)` = sprintf("%.2f", min(VWC * 100, na.rm = TRUE)),    # Minimum
    `Max VWC (%)` = sprintf("%.2f", max(VWC * 100, na.rm = TRUE)),    # Maximum
    .groups = "drop"
  ) %>%
  arrange(Treatment, Depth_Order) %>%
  select(-Depth_Order)  # Remove depth order column

# Create a formatted table for publication
summary_table <- summary_stats %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary of Soil Moisture (VWC) Statistics by Treatment and Depth (Apr 01 - Sep 23, 2023)",
    label = "tab:vwc_summary",
    col.names = c("Treatment", "Depth", "Mean VWC (%)", "SD (%)", "Min VWC (%)", "Max VWC (%)"),
    align = c("l", "c", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("striped", "repeat_header", "hold_position", "scale_down"),
    font_size = 9,
    position = "center",
    full_width = FALSE
  ) %>%
  pack_rows(index = table(summary_stats$Treatment)) %>%  # Group by Treatment
  footnote(
    general = "VWC = Volumetric Water Content. NS = Non-stressed, S = Stressed conditions.",
    general_title = "Note:"
  )

# Save the table to a LaTeX file
writeLines(summary_table, "figures/soil_vwc_summary_table_2023.tex")

# Optional: Display table in R console
print(summary_table)
