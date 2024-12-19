# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Create figures directory if it doesn't exist
dir.create("figures", showWarnings = FALSE)

# Read and process data
irrigation_data <- Tyree_irrigation_w_missing_dates
irrigation_data$Date <- dmy(paste0(irrigation_data$Date, "-2022"))

# Calculate separate cumulative totals for each treatment
irrigation_split <- irrigation_data %>%
  # First create rows for each treatment
  mutate(
    `2L_daily` = case_when(
      Date <= as.Date("2022-08-12") ~ Total/2,  # Split baseline period
      TRUE ~ `2L`
    ),
    `4L_daily` = case_when(
      Date <= as.Date("2022-08-12") ~ Total/2,  # Split baseline period
      TRUE ~ `4L`
    )
  ) %>%
  # Calculate cumulative amounts for each treatment
  mutate(
    `2L_cumulative` = cumsum(`2L_daily`),
    `4L_cumulative` = cumsum(`4L_daily`)
  )

# Create the dual-axis plot
p <- ggplot(irrigation_split) +
  # Daily irrigation lines
  geom_line(aes(x = Date, y = `2L_daily`, color = "2L Treatment"), size = 1) +
  geom_line(aes(x = Date, y = `4L_daily`, color = "4L Treatment"), size = 1) +
  geom_point(aes(x = Date, y = `2L_daily`, color = "2L Treatment"), size = 2) +
  geom_point(aes(x = Date, y = `4L_daily`, color = "4L Treatment"), size = 2) +
  
  # Cumulative lines (scaled to secondary axis)
  geom_line(aes(x = Date, y = `2L_cumulative`/3, linetype = "2L Cumulative"), 
            color = "#1B9E77", size = 1) +
  geom_line(aes(x = Date, y = `4L_cumulative`/3, linetype = "4L Cumulative"), 
            color = "#D95F02", size = 1) +
  
  # Add vertical line for treatment transition
  geom_vline(xintercept = as.Date("2022-08-12"), 
             linetype = "dashed", 
             color = "gray50", 
             size = 0.5) +
  
  # Add heatwave period annotation
  annotate("rect", 
           xmin = as.Date("2022-09-05"), 
           xmax = as.Date("2022-09-06"), 
           ymin = -Inf, 
           ymax = Inf, 
           alpha = 0.2, 
           fill = "orange") +
  
  # Customize theme
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.y.right = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  
  # Set colors and linetypes
  scale_color_manual("Daily Irrigation",
                     values = c("2L Treatment" = "#1B9E77",
                                "4L Treatment" = "#D95F02")) +
  scale_linetype_manual("Cumulative Irrigation",
                        values = c("2L Cumulative" = "dotted",
                                   "4L Cumulative" = "dotted")) +
  
  # Updated title and labels
  labs(
    title = "2022 Season Irrigation",
    subtitle = "Daily irrigation (solid lines) and cumulative totals (dotted lines)",
    x = "Date",
    y = "Daily Irrigation Volume (L)",
    color = "Daily Irrigation",
    linetype = "Cumulative Irrigation"
  ) +
  
  # Add second y-axis for cumulative values
  scale_y_continuous(
    name = "Daily Irrigation Volume (L)",
    sec.axis = sec_axis(~.*3, name = "Cumulative Irrigation Volume (L)")
  )

# Save plot
ggsave("figures/irrigation_dual_axis_plot.png", plot = p,
       width = 12, 
       height = 8, 
       dpi = 600,
       bg = "white")