wp_data <- Tyree_PSI_2022_Cleaned_W_ETo_T
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(broom)
library(lubridate)
library(kableExtra)
library(nortest)

# Check initial data distribution
print("Original data distribution:")
print(table(wp_data$Variety, wp_data$Tx, useNA = "ifany"))

# Data processing with CS filter
wp_data <- wp_data %>%
  filter(Variety == "CS") %>%
  mutate(
    Date = as.Date(paste(Year, Month, Day, sep="-"), format="%Y-%m-%d"),
    Time_id = case_when(
      Time == 500 ~ "5AM",
      Time == 1300 ~ "1PM",
      TRUE ~ as.character(Time)
    ),
    Treatment = case_when(
      Tx == 2 ~ "2L",
      Tx == 4 ~ "4L",
      is.nan(Tx) ~ "Control",
      TRUE ~ NA_character_
    ),
    Treatment = factor(Treatment)
  )

# Filter for main measurement times
wp_filtered <- wp_data %>%
  filter(Time_id %in% c("1PM", "5AM"))

# Calculate summary statistics
summary_stats <- wp_filtered %>%
  group_by(Date, Time_id, Treatment) %>%
  summarise(
    Mean = mean(PSI, na.rm = TRUE),
    SD = sd(PSI, na.rm = TRUE),
    n = n(),
    SE = SD/sqrt(n),
    .groups = 'drop'
  ) %>%
  mutate(Date = as.Date(Date))

# Create base theme
base_theme <- theme_classic() +
  theme(
    panel.background = element_rect(fill = "beige", color = "black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Create plot
p <- ggplot(summary_stats, aes(x = Date, y = Mean, color = Treatment, group = Treatment)) +
  annotate("rect", 
           xmin = min(summary_stats$Date), 
           xmax = max(summary_stats$Date),
           ymin = -Inf, ymax = Inf,
           fill = "beige",
           alpha = 0.2) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 1, 
                size = 0.5) +
  scale_color_manual(values = c("2L" = "blue", "4L" = "red", "Control" = "black")) +
  facet_wrap(~Time_id) +
  labs(
    title = "Cabernet Sauvignon Stem Water Potential",
    x = "Date",
    y = "Mean (Bar)",
    color = "Treatment"
  ) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(
    limits = c(-20, 0),
    breaks = seq(-20, 0, by = 5)
  ) +
  base_theme +
  theme(legend.position = "top")

# Add rest wave lines
rest_wave_dates <- as.Date(c("2022-09-01", "2022-09-15"))
for(date in rest_wave_dates) {
  p <- p + geom_vline(xintercept = date, 
                      color = "grey50", 
                      alpha = 0.5,
                      size = 1)
}

# Statistical analyses
# ANOVA for treatment and time effects
model_anova <- aov(PSI ~ Treatment + Time_id, data = wp_filtered)
anova_summary <- summary(model_anova)

# Tukey's HSD for treatment comparisons
tukey_results <- TukeyHSD(model_anova, "Treatment")

# Separate analyses for morning and afternoon
am_data <- wp_filtered %>% filter(Time_id == "5AM")
pm_data <- wp_filtered %>% filter(Time_id == "1PM")

am_anova <- aov(PSI ~ Treatment, data = am_data)
pm_anova <- aov(PSI ~ Treatment, data = pm_data)

# Create summary table
summary_table <- wp_filtered %>%
  group_by(Treatment, Time_id) %>%
  summarise(
    Mean_PSI = mean(PSI, na.rm = TRUE),
    SD = sd(PSI, na.rm = TRUE),
    n = n(),
    SE = SD/sqrt(n),
    .groups = 'drop'
  )

# Save results
sink("CS_statistical_analysis_results.txt")
cat("Cabernet Sauvignon Water Potential Analysis Results\n")
cat("===============================================\n\n")

cat("1. Sample Sizes\n")
cat("--------------\n")
print(table(wp_filtered$Treatment, wp_filtered$Time_id))

cat("\n2. Overall ANOVA Results\n")
cat("----------------------\n")
print(anova_summary)

cat("\n3. Treatment Comparisons (Tukey HSD)\n")
cat("----------------------------------\n")
print(tukey_results)

cat("\n4. Time-specific Analyses\n")
cat("------------------------\n")
cat("\nMorning (5AM):\n")
print(summary(am_anova))
cat("\nAfternoon (1PM):\n")
print(summary(pm_anova))

cat("\n5. Summary Statistics by Treatment and Time\n")
cat("----------------------------------------\n")
print(summary_table)
sink()

# Save plot
ggsave("CS_stem_water_potential.pdf", p, width = 10, height = 6, dpi = 300)

# Display results
print(p)
cat("\nSample sizes by treatment and time:\n")
print(table(wp_filtered$Treatment, wp_filtered$Time_id))
