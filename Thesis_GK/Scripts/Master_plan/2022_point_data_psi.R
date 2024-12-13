library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the data
data <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_2022_Cleaned.xlsx")

# Convert Month, Day, and Year to Date and convert Ψ to numeric
data <- data %>%
  mutate(Date = make_date(Year, Month, Day),
         Ψ = as.numeric(Ψ)) %>%
  # Remove rows where Date or Ψ is NA
  filter(!is.na(Date), !is.na(Ψ))

# Plotting Ψ against Date, using Time and Stem_Leaf for separate plots, and Variety for color
p <- ggplot(data, aes(x = Date, y = Ψ, color = Variety)) +
  geom_point() + # Plot data points
  geom_smooth(method = "loess", se = TRUE) + # Include smooth trend lines with standard error shading
  geom_hline(yintercept = -12.0, linetype = "dashed", color = "blue", size = 1) + 
  geom_hline(yintercept = -15.0, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~ Time + Stem_Leaf, scales = "free_y") +
  theme_minimal() +
  labs(title = "Plot of Ψ against Date",
       x = "Date",
       y = "Water Potential (Ψ)",
       caption = "Data separated by Time and Stem_Leaf, Variety shown in colors")

print(p)
