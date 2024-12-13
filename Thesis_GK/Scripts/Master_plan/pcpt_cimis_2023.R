library(readr)
library(dplyr)
library(ggplot2)

# Reading the data
data <- read_csv("~/Downloads/monthly (1).csv")

# Define correct levels based on observed Month_Year format
month_levels <- c("22-Sep", "22-Oct", "22-Nov", "22-Dec", "23-Jan", "23-Feb", "23-Mar", "23-Apr")

# Convert Month_Year to a factor with specified levels
data <- data %>%
  mutate(Month_Year = factor(Month_Year, levels = month_levels)) %>%
  filter(!is.na(Pcpt), Month_Year %in% month_levels) %>%
  mutate(
    Pcpt_clean = ifelse(is.na(Pcpt), 0, Pcpt),
    gallons_per_acre = Pcpt_clean * 27154 * 1.6,  # Convert to gallons per acre
    cumulative_gallons_per_acre = cumsum(gallons_per_acre)  # Calculate the cumulative total
  )

# Ensure there's data to plot after processing
if (nrow(data) > 0) {
  # Create plots for precipitation and cumulative gallons per acre
  p1 <- ggplot(data, aes(x = Month_Year, y = Pcpt_clean, fill = "Total Precipitation")) +
    geom_bar(stat = "identity") +
    scale_y_continuous("Precipitation (inches)", breaks = seq(0, 15, 1)) +
    labs(title = "Total Precipitation by Month and Year", x = "Month and Year", y = "Precipitation (inches)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c("Total Precipitation" = "steelblue"))
  
  p2 <- ggplot(data, aes(x = Month_Year, y = cumulative_gallons_per_acre, fill = "Cumulative Gallons per Acre")) +
    geom_bar(stat = "identity", color = "firebrick3", fill = "firebrick4") +
    scale_y_continuous("Cumulative gallons", breaks = seq(0, max(data$cumulative_gallons_per_acre, na.rm = TRUE), by = 50000)) +
    labs(title = "Cumulative Gallons for Experimental Vineyard by Month and Year", x = "Month and Year", y = "Cumulative gallons") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c("Cumulative Gallons per Acre" = "red"))
  
  # Print the plots
  print(p1)
  print(p2)
} else {
  print("No data available for plotting after filtering.")
}
