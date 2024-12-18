# Load required libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(scales)
library(kableExtra)

# Step 1: Load the Data
# Replace 'your_file.csv' with your actual data file
soil_data <- sm_output_cleaned_2022

# Step 2: Create Figures Directory
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Step 3: Reshape Data for Plotting
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
  )

# Step 4: Generate Figures
# 1. Depth Profile Plot
depth_profile_plot <- function(data) {
  ggplot(data, aes(x = Date, y = Depth_num, fill = Moisture)) +
    geom_tile() +  # Use geom_tile for uneven dates
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
      title = "Soil Moisture Distribution by Depth Over Time - 2022 Season"
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
      title = "Soil Moisture Distribution by Treatment and Depth - 2022 Season"
    )
}

# 3. Temporal Patterns Plot
temporal_patterns_plot <- function(data) {
  ggplot(data %>% filter(Depth_num <= 24),  # Focus on upper soil layers
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
      title = "Temporal Patterns in Soil Moisture by Treatment - 2022 Season"
    )
}

# Step 5: Generate Plots and Save Them
p1 <- depth_profile_plot(soil_data_long)
ggsave("figures/soil_depth_profile_2022.png", plot = p1, width = 8, height = 6, dpi = 600)

p2 <- treatment_comparison_plot(soil_data_long)
ggsave("figures/soil_treatment_comparison_2022.png", plot = p2, width = 8, height = 6, dpi = 600)

p3 <- temporal_patterns_plot(soil_data_long)
ggsave("figures/soil_temporal_patterns_2022.png", plot = p3, width = 8, height = 6, dpi = 600)

# Improved table generation code
# Improved table generation code with thesis-ready LaTeX features
summary_stats <- soil_data_long %>%
  # Extract numeric depth value and create treatment labels
  mutate(
    Depth_Order = as.numeric(gsub("[^0-9]", "", Depth)),
    Treatment = case_when(
      Stress == "NS" ~ paste0(Tx, " (Non-Stressed)"),
      TRUE ~ paste0(Tx, " (Stressed)")
    )
  ) %>%
  # Group and calculate statistics
  group_by(Treatment, Depth, Depth_Order) %>%
  summarise(
    `Mean (%)` = sprintf("%.2f", mean(Moisture, na.rm = TRUE)),
    `SD (%)` = sprintf("%.2f", sd(Moisture, na.rm = TRUE)),
    `Min (%)` = sprintf("%.2f", min(Moisture, na.rm = TRUE)),
    `Max (%)` = sprintf("%.2f", max(Moisture, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Clean up depth labels
  mutate(
    Depth = gsub("Soil Moisture ", "", Depth),
    Depth = gsub(" \\(%\\)", "", Depth)
  ) %>%
  # Arrange by treatment and depth
  arrange(Treatment, Depth_Order) %>%
  select(-Depth_Order)

# Create LaTeX header with necessary packages
latex_header <- c(
  "% Required packages for this table",
  "% \\usepackage{booktabs}",
  "% \\usepackage{longtable}",
  "% \\usepackage{threeparttable}",
  "% \\usepackage{array}",
  ""
)

# Create a publication-quality table with thesis-specific features
summary_table <- summary_stats %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Soil moisture statistics by treatment and depth during the 2022 growing season.",
    label = "tab:soil_moisture_summary",  # Add label for cross-referencing
    col.names = c("Treatment", "Depth", "Mean (%)", "SD (%)", "Min (%)", "Max (%)"),
    align = c("l", "c", "r", "r", "r", "r"),
    escape = FALSE  # Allow LaTeX commands in the table
  ) %>%
  kable_styling(
    latex_options = c("repeat_header", "striped"),
    font_size = 9,
    position = "center",
    full_width = FALSE
  ) %>%
  # Add treatment grouping
  pack_rows(
    index = table(summary_stats$Treatment),
    indent = FALSE,
    hline_after = TRUE
  ) %>%
  # Add footnote with details
  footnote(
    general = paste(
      "Measurements taken from June to September 2022.",
      "NS = Non-stressed, S = Stressed conditions."
    ),
    threeparttable = TRUE,
    general_title = "Note:",
    escape = FALSE
  ) %>%
  # Add column spacing
  column_spec(1, width = "2.5cm") %>%
  column_spec(2, width = "1.5cm") %>%
  column_spec(3:6, width = "1.8cm")

# Combine header and table content
full_table <- c(
  latex_header,
  "% Table generated by R on ",
  paste0("% ", Sys.Date()),
  "",
  summary_table
)

# Save the table with header comments
writeLines(full_table, "figures/soil_moisture_summary_table.tex")

# Create a separate usage instruction file
usage_instructions <- c(
  "To include this table in your Scrivener thesis:",
  "",
  "1. Make sure these packages are in your LaTeX preamble:",
  "   \\usepackage{booktabs}",
  "   \\usepackage{longtable}",
  "   \\usepackage{threeparttable}",
  "   \\usepackage{array}",
  "",
  "2. Include the table using either:",
  "   \\input{figures/soil_moisture_summary_table.tex}",
  "   OR",
  "   {<<include figures/soil_moisture_summary_table.tex>>}",
  "",
  "3. Reference the table in your text using:",
  "   Table~\\ref{tab:soil_moisture_summary}",
  "",
  "Note: The table is labeled as 'tab:soil_moisture_summary'"
)

# Save usage instructions
writeLines(usage_instructions, "figures/table_usage_instructions.txt")
