library(ggplot2)
data <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022/CIMIS_2022/CIMIS_growing_season_2022.csv")

# Convert 'Date' column to Date format
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data$date
Tmax <- data$`Max Air Temp (F)`
Tmin <- data$`Min Air Temp (F)`
Tavg <- data$`Avg Air Temp (F)`
# Create the plot

  ggplot(data, aes(x = Date)) +
    geom_point(aes(y = Tmax), color = "red") +
    geom_point(aes(y = Tmin), color = "blue") +
    geom_point(aes(y = Tavg), color = "green") +  geom_smooth(aes(y = Tmax), color = "red", se = TRUE) +
    geom_smooth(aes(y = Tmin), color = "blue", se = TRUE) +
    geom_smooth(aes(y = Tavg), color = "green", se = TRUE) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
    labs(x = "Date", y = "Temperature (F)") +
    ggtitle("Temperature Variation") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  