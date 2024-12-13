library(dplyr)
library(ggplot2)

# Read the CSV file
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/extracted_licor_2023_doy.csv")

# Filter out rows with negative Tleaf values and only keep rows with Var = "CS"
data$date <- as.Date(data$date)
data <- data %>% filter(Tleaf >= 24, Var == "CS")

# Calculate R-squared and p-value
r_squared <- summary(model)$r.squared
p_value <- summary(model)$coefficients[3, 4]  # Use row index 3 instead of 4
r_squared
p_value




ggplot(data, aes(x = gsw, y = A, fill = Tx, color = Tx)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "gsw", y = "A", fill = "Tx", color = "Tx") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = min(data$gsw), y = max(data$A), 
           label = paste("R-squared =", round(r_squared, 2), "\n",
                         "p-value =", round(p_value, 2)), 
           hjust = 0, vjust = 1)
