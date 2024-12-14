# Required packages
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(ggplot2)
library(lubridate)
library(nlme)
library(readxl)



# Data import and preparation
licor_data <- clean_licor_2023_extract
wp_data <- Clean_Tyree_Water_Potentials_2023

# Let's examine the structure of your datasets
str(licor_data)
str(wp_data)

# And see what columns are available
names(licor_data)
names(wp_data)

# Convert date formats and create time periods
licor_data$date <- as.POSIXct(licor_data$date, format="%Y%m%d %H:%M:%S")
licor_data$doy <- yday(licor_data$date)

# Calculate WUEi
licor_data$WUEi <- licor_data$A/licor_data$gsw

# Basic ANOVA models for treatment effects
wp_model <- lmer(PSI ~ Block + Variety + Time + (1|Date), data = wp_data)
anova(wp_model)

# Analysis of container size effects
container_model <- lmer(A ~ Block + Variety + pot_type + Time + 
                          (1|Date), data = licor_data)
summary(container_model)

# Heatwave specific analysis
# Filter for heatwave dates
heatwave_data <- licor_data %>%
  filter(date >= as.POSIXct("2023-08-15") & 
           date <= as.POSIXct("2023-08-18"))

# Compare treatments during heatwave
heatwave_model <- lmer(A ~ Block + Variety + pot_type * Stress + 
                         (1|date), data = heatwave_data)
summary(heatwave_model)

# Temperature response curves
temp_model <- lm(A ~ poly(Tleaf, 2) * Variety * pot_type, 
                 data = licor_data)
summary(temp_model)

# Create visualization of temperature response
ggplot(licor_data, aes(x = Tleaf, y = A, color = Variety)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  facet_wrap(~pot_type) +
  theme_bw() +
  labs(x = "Leaf Temperature (°C)",
       y = "Photosynthetic Rate (µmol m⁻² s⁻¹)",
       title = "Temperature Response Curves by Variety and Container Size")

# Analysis of hydraulic conductance
licor_data$K <- licor_data$E/(-1) # Simplified for demonstration
k_model <- lmer(K ~ Block + Variety + pot_type + Tleaf + 
                  (1|Date), data = licor_data)
summary(k_model)

# Year comparison (assuming 2022 data is similarly structured)
year_comparison <- lmer(A ~ Year + Block + Variety + pot_type + 
                          (1|Date), data = combined_years)
summary(year_comparison)

# High temperature analysis (>30°C)
high_temp_data <- licor_data %>%
  filter(Tleaf > 30)

high_temp_model <- lmer(A ~ Block + Variety + pot_type * Stress + 
                          (1|Date), data = high_temp_data)
summary(high_temp_model)

# Post-hoc comparisons
emmeans(high_temp_model, pairwise ~ pot_type:Variety)

# Calculate treatment means and standard errors
treatment_summary <- high_temp_data %>%
  group_by(pot_type, Variety, Stress) %>%
  summarise(
    mean_A = mean(A, na.rm = TRUE),
    se_A = sd(A, na.rm = TRUE)/sqrt(n()),
    mean_gs = mean(gsw, na.rm = TRUE),
    se_gs = sd(gsw, na.rm = TRUE)/sqrt(n()),
    mean_Tleaf = mean(Tleaf, na.rm = TRUE),
    se_Tleaf = sd(Tleaf, na.rm = TRUE)/sqrt(n())
  )

# Export results
write.csv(treatment_summary, "treatment_summary.csv")

# Create publication-quality figures
ggplot(treatment_summary, 
       aes(x = pot_type, y = mean_A, fill = Variety)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_A - se_A, ymax = mean_A + se_A),
                position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~Stress) +
  theme_bw() +
  labs(x = "Container Size",
       y = "Photosynthetic Rate (µmol m⁻² s⁻¹)",
       title = "Treatment Effects on Photosynthesis During Heat Stress")

# Correlation analysis
cor_test <- cor.test(licor_data$A, licor_data$ETR)
print(cor_test)

# Mixed effects model for repeated measures
repeated_measures <- lme(A ~ pot_type * Variety * Stress,
                         random = ~1|Date/Block,
                         data = licor_data)
summary(repeated_measures)

# Test for normality and homoscedasticity
shapiro.test(resid(repeated_measures))
plot(repeated_measures)
