

# Load required libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(scales)
library(kableExtra)

# Step 1: Load the Data
# Replace 'your_file.csv' with your actual data file
soil_data <- sm_output_cleaned_2022

# Create figures directory if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Reshape and filter data for the specific date range
soil_data_long <- soil_data %>%
  pivot_longer(
    cols = contains("Soil Moisture"),
    names_to = "Depth",
    values_to = "Moisture"
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
  # Filter for the specified date range
  filter(Date >= as.Date("2022-07-20") & Date <= as.Date("2022-09-23"))

# 1. Depth Profile Plot
depth_profile_plot <- function(data) {
  ggplot(data, aes(x = Date, y = Depth_num, fill = Moisture)) +
    geom_tile() +
    scale_fill_viridis(option = "magma") +
    scale_y_continuous(
      breaks = c(6, 12, 18, 24, 30, 36, 42, 48),
      labels = c("6", "12", "18", "24", "30", "36", "42", "48")
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "right"
    ) +
    labs(
      x = "Date",
      y = "Soil Depth (inches)",
      fill = "Soil Moisture (%)",
      title = "Soil Moisture Distribution by Depth (Jul 20 - Sep 23, 2022)"
    )
}

# 2. Treatment Comparison Plot
treatment_comparison_plot <- function(data) {
  ggplot(data, aes(x = Treatment, y = Moisture, fill = Stress)) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA) +
    facet_wrap(~Depth, scales = "free_y") +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      strip.text = element_text(size = 10)
    ) +
    labs(
      x = "Treatment",
      y = "Soil Moisture (%)",
      title = "Soil Moisture Distribution by Treatment and Depth (Jul 20 - Sep 23, 2022)"
    )
}

# 3. Temporal Patterns Plot
temporal_patterns_plot <- function(data) {
  ggplot(data %>% filter(Depth_num <= 24),
         aes(x = Date, y = Moisture, color = Depth)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~Treatment, scales = "free_y") +
    scale_color_viridis_d() +
    theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "bottom"
    ) +
    labs(
      x = "Date",
      y = "Soil Moisture (%)",
      color = "Depth",
      title = "Temporal Patterns in Soil Moisture (Jul 20 - Sep 23, 2022)"
    )
}

# Generate and save plots
p1 <- depth_profile_plot(soil_data_long)
p2 <- treatment_comparison_plot(soil_data_long)
p3 <- temporal_patterns_plot(soil_data_long)

# Save plots with high resolution
ggsave("figures/soil_depth_profile_2022_filtered.png", p1, width = 8, height = 6, dpi = 600)
ggsave("figures/soil_treatment_comparison_2022_filtered.png", p2, width = 8, height = 6, dpi = 600)
ggsave("figures/soil_temporal_patterns_2022_filtered.png", p3, width = 8, height = 6, dpi = 600)

# Generate summary statistics
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
    `Mean (%)` = sprintf("%.2f", mean(Moisture, na.rm = TRUE)),
    `SD (%)` = sprintf("%.2f", sd(Moisture, na.rm = TRUE)),
    `Min (%)` = sprintf("%.2f", min(Moisture, na.rm = TRUE)),
    `Max (%)` = sprintf("%.2f", max(Moisture, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Depth = gsub("Soil Moisture ", "", Depth),
    Depth = gsub(" \\(%\\)", "", Depth)
  ) %>%
  arrange(Treatment, Depth_Order) %>%
  select(-Depth_Order)

# Create and save the table
summary_table <- summary_stats %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Soil moisture statistics by treatment and depth (July 20 - September 23, 2022).",
    label = "tab:soil_moisture_summary",
    col.names = c("Treatment", "Depth", "Mean (%)", "SD (%)", "Min (%)", "Max (%)"),
    align = c("l", "c", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 9,
    position = "center",
    full_width = FALSE
  ) %>%
  pack_rows(
    index = table(summary_stats$Treatment),
    indent = FALSE,
    hline_after = TRUE
  ) %>%
  footnote(
    general = "NS = Non-stressed, S = Stressed conditions.",
    threeparttable = TRUE,
    general_title = "Note:"
  ) %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(2, width = "1.5cm") %>%
  column_spec(3:6, width = "1.8cm")

# Save the table
writeLines(summary_table, "figures/soil_moisture_summary_table.tex")
