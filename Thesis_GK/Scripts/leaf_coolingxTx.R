library(tidyverse)
library(car)

# Read in the CSV file
data <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022/Tyree LiCor Data 2022/licor_combined_2022.csv")

# Subtract Tleaf from Tair
data <- data %>% mutate(Tcooling = Tair - Tleaf)

data$Tcooling
# Test for normal distribution
shapiro.test(data$Tcooling)

# Conduct ANOVA with Type III error by Tx and show the response
anova_result <- Anova(lm(data$Tx ~ data$Tcooling, data = data), type = 3)
summary(anova_result)
plot(anova_result)
