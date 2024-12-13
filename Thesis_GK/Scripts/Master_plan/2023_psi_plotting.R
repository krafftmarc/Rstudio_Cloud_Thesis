library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)  # Make sure purrr is loaded

# Load the data from an Excel file
data <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/water_pot copy/Clean_Tyree Water Potentials 2023.xlsx")

# Assuming the date is in MM/DD/YYYY format in the Excel file
data$date <- as.Date(data$date, format = "%m/%d/%Y")

# Separate the data by time_block and pot_type, then plot Ψ by date by variety
data %>%
  group_by(time_block, pot_type) %>%
  group_split() %>%
  walk(~{
    plot_data <- .x
    p <- plot_data %>%
      group_by(variety) %>%
      ggplot(aes(x = date, y = `Ψ`, color = variety, group = variety)) +
      geom_point() +  # Plot data points
      geom_smooth(method = "loess", se = TRUE) +  # Add smooth trend lines with confidence intervals
      geom_hline(yintercept = -12.0, linetype = "dashed", color = "blue", size = 1) +  # Horizontal line at -12.0
      geom_hline(yintercept = -15.0, linetype = "dashed", color = "red", size = 1) +  # Horizontal line at -15.0
      labs(title = paste("Ψ by Date for", unique(plot_data$time_block), "and", unique(plot_data$pot_type)),
           x = "Date",
           y = "Ψ") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_labels = "%m/%d/%Y") +
      guides(color = guide_legend(title = "Variety"))
    
    print(p)  # This will display the plot in the R environment
    ggsave(paste("plot_", unique(plot_data$time_block), "_", unique(plot_data$pot_type), ".png", sep = ""), plot = p)
  })
