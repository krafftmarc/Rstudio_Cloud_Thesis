library(readxl)
library(dplyr)
library(purrr)
library(corrplot)
library(rstatix)
library(Hmisc)
library(psych)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(cowplot)
library(rlang)
library(gridExtra)
library(Rmisc)

############################################ Data preparation ----
data <- read_excel("E:/Actual - Edited-NSCs 5_16_2023/R-DAVIS-2022/r-davis-in-class-project-arodriguezurquidi/data/marc_hw_treatments.xlsx")
#View(marc_hw_treatments)
str(data)

data$block = as.factor(data$block)
data$treatment = as.factor(data$treatment)
data$hw_treatment = as.factor(data$hw_treatment)
data$id = as.factor(data$id)
data$row = as.factor(data$row)
data$time = as.factor(data$time)
str(data)
summary(data)

### data with all licor variables + soil & temp
data2 <- read_excel("E:/Actual - Edited-NSCs 5_16_2023/R-DAVIS-2022/r-davis-in-class-project-arodriguezurquidi/data/r-analysis.xlsx")
str(data2)

data2$id = as.factor(data2$id)
data2$block = as.factor(data2$block)
data2$row = as.factor(data2$row)
data2$time = as.factor(data2$time)
data2$vine = as.factor(data2$vine)
str(data2)
summary(data2)

### Data Analysis ###----
############################################ STARCH ############################################

# 2L to 4L t-test
result <- t.test(starch ~ treatment, data = data)
# Print the results
print(result)

# Perform ANOVA
result2 <- aov(starch ~ treatment + hw_treatment, data = data)
result3 <- aov(starch ~ treatment * hw_treatment, data = data)

# ANOVA time * treatments
result4 <- aov(nsc ~ time * treatment * hw_treatment, data = data)
result5 <- aov(nsc ~ time, data = data)
result6 <- aov(starch ~ time, data = data)

# Print the results
print(summary(result2))
print(summary(result3))
print(summary(result4))
print(summary(result5)) # The effect is more pronoounced in NSCs, p=2e^-16
print(summary(result6)) # not so much for starch, p = 0.113

# Perform Tukey's post hoc test
library(multcomp)
tukey_result <- glht(result5, linfct = mcp(time = "Tukey"))
# Summarize the Tukey's test results
summary(tukey_result)
# Plot the Tukey's test results
library(multcompView)
plot(tukey_result)

# Create a subset of data for the time intervals of interest (100 and 400)
subset_data <- data[data$time %in% c(100, 400), ]
# Perform pairwise t-tests for the time intervals
pairwise_result <- pairwise.t.test(subset_data$nsc, subset_data$time, p.adjust.method = "bonferroni")

# Print the pairwise comparison results
print(pairwise_result)

# Perform Tukey's HSD test
posthoc2 <- TukeyHSD(result2)
posthoc3 <- TukeyHSD(result3)

# Print the pairwise comparisons
print(posthoc2)
print(posthoc3)


# Perform pairwise t-tests with Bonferroni correction
posthoc_correctedp <- pairwise.t.test(data$starch, data$treatment, p.adjust.method = "bonferroni")

# Print the pairwise comparisons
print(posthoc_correctedp)



############################################ NSCs ##################################################

# 2L to 4L t-test
result <- t.test(nsc ~ treatment, data = data)
# Print the results
print(result)

# Perform ANOVA
result2 <- aov(nsc ~ treatment + hw_treatment, data = data)
result3 <- aov(nsc ~ treatment * hw_treatment, data = data)
# Print the results
print(summary(result2))
print(summary(result3))

# Perform Tukey's HSD test
posthoc2 <- TukeyHSD(result2)
posthoc3 <- TukeyHSD(result3)

# Print the pairwise comparisons
print(posthoc2)
print(posthoc3)


# Perform pairwise t-tests with Bonferroni correction
posthoc_correctedp <- pairwise.t.test(data$starch, data$treatment, p.adjust.method = "bonferroni")

# Print the pairwise comparisons
print(posthoc_correctedp)

############################################ CAN WE ELIMINATE 1 and 4 AM? #######
# Create a custom contrast matrix
contrast_matrix <- matrix(c(-1, 1, 0, 0, 0, 0, 0, 0,    # 700 - 1000
                            0, -1, 1, 0, 0, 0, 0, 0,    # 1000 - 1300
                            0, 0, -1, 1, 0, 0, 0, 0,    # 1300 - 1600
                            0, 0, 0, -1, 1, 0, 0, 0,    # 1600 - 1900
                            0, 0, 0, 0, -1, 1, 0, 0,    # 1900 - 2200
                            0, 0, 0, 0, 0, -1, 1, 0,    # 2200 - 100
                            -1, 0, 0, 0, 0, 0, 0, 1     # 100 - 400
), ncol = 8, byrow = TRUE)
# Perform pairwise t-tests with the custom contrast matrix
pairwise_result <- pairwise.t.test(data$nsc, data$time, p.adjust.method = "bonferroni", pool.sd = FALSE,
                                   paired = FALSE, alternative = "two.sided", contr = contrast_matrix)

# Print the pairwise comparison results
print(pairwise_result)

library(ggplot2)

# Create a matrix of p-values from the pairwise comparison results
p_values <- pairwise_result$p.value
row_names <- rownames(p_values)
col_names <- colnames(p_values)

# Create a data frame for the heatmap plot
heatmap_data <- expand.grid(row = row_names, col = col_names)
heatmap_data$p_value <- as.vector(p_values)

# Create the heatmap plot
ggplot(heatmap_data, aes(x = col, y = row, fill = p_value, label = sprintf("%.4f", p_value))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Time", y = "Time", fill = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_equal() +
  ggtitle("Pairwise Comparison - p-values") +
  geom_text(color = "black", size = 3)  # Add p-values as text labels
#
# 1000 to 400: p-value = 1.3e-10 (very highly significant) NOT IMPORTANT
# 1900 to 400: p-value = 7.2e-12 (very highly significant) NOT IMPORTANT
# 2200 to 400: p-value = 9.3e-12 (very highly significant) NOT IMPORTANT
# 1900 to 1000: p-value = 0.00029 (highly significant) NOT
# 2200 to 1000: p-value = 0.00029 (highly significant) NOT
# 700 to 400: p-value = 4.4e-05 (significant) <- WE CAN REMOVE 4 AM 
# 1600 to 1000: p-value = 0.14866 (not significant) NOT
# 1300 to 1000: p-value = 0.23577 (not significant) NOT
# 1600 to 400: p-value = 0.28210 (not significant) NOT
# 1000 to 700: p-value = 0.00104 (significant) NOT
# 1900 to 700: p-value = 3.9e-05 (significant) NOT
# 2200 to 700: p-value = 3.0e-05 (significant) 
# 1300 to 400: p-value = 6.3e-07 (very highly significant)
# 1600 to 700: p-value = 0.19863 (not significant)
# 1300 to 700: p-value = 0.73416 (not significant)



# Numerical Variables 
############################################ NUMERICAL VARIABLES, Linear regressions, Tukeys, t.test (data) ----

# Calculate the correlation coefficient
correlation <- cor(data$nsc, data$treatment_n)
print(correlation)

# Perform linear regression analysis
model <- lm(data$nsc ~ data$treatment_n)
summary(model)

####
# Coefficients:
# The coefficients section presents the estimates for the intercept and the coefficient associated with the data$treatment_n 
# variable. These estimates represent the expected change in the dependent variable for each unit increase in the independent 
# variable.
# 
# The estimate for the intercept is 73.1366, suggesting that when data$treatment_n is zero, the expected value 
# of data$nsc is 73.1366. The p-value associated with the (Intercept) represents the probability of observing such an 
# extreme or more extreme value for the intercept coefficient, assuming the null hypothesis that the true value of the 
# coefficient is zero.
# A small p-value (e.g., less than the conventional significance level of 0.05) suggests strong evidence against the null 
# hypothesis. Therefore, in this case, the small p-value for the (Intercept) term indicates that the intercept coefficient 
# is statistically significant, meaning that the y-intercept is significantly different from zero.


# The estimate for data$treatment_n is 0.5019, indicating that for each unit 
# increase in data$treatment_n, the expected change in data$nsc is 0.5019. However, note that the associated 
# p-value for this coefficient is 0.778, indicating that it is not statistically significant.


# Residual standard error:
# The residual standard error (RSE) is an estimate of the standard deviation of the residuals. In this case, the 
# RSE is 21.22, indicating the average amount by which the observed data$nsc values deviate from the predicted values.

# Multiple R-squared and Adjusted R-squared:
# The multiple R-squared value represents the proportion of variability in the dependent variable that can be explained 
# by the independent variable(s). In this case, the R-squared value is 0.0005602, suggesting that the data$treatment_n 
# variable explains a very small fraction of the variability in data$nsc. The adjusted R-squared value takes into account 
# the number of predictors in the model, and in this case, it is slightly negative (-0.006478), suggesting that the model does 
# not improve the prediction of data$nsc compared to a simple mean.
# 
# F-statistic and p-value:
# The F-statistic assesses the overall significance of the model by comparing the variability explained by the model to 
# the unexplained variability. In this example, the F-statistic is 0.07959, with a corresponding p-value of 0.7783, indicating 
# that the model is not statistically significant in explaining the variability in data$nsc.


# New data for prediction
data$treatment_one = data$treatment_n-1 

# Make predictions
predictions <- predict(model, data = data$treatment_one)
# Print the predictions
print(predictions)

# Obtain predictions with confidence intervals
pred_interval <- predict(model, data = data$treatment_one, interval = "confidence")
# Print the predictions with confidence intervals
print(pred_interval)


plot(data$nsc, data$treatment_n, pch = 16, xlab = "x", ylab = "y", main = "Linear Regression") # Plot the observed data points
abline(model, col = "blue") # Add the regression line
lines(data$nsc, pred_interval[, "lwr"], col = "red", lty = 2) # Add confidence intervals
lines(data$nsc, pred_interval[, "upr"], col = "red", lty = 2)
legend("topright", legend = c("Data", "Regression Line", "Confidence Intervals"),
       col = c("black", "blue", "red"), lty = c(NA, 1, 2), pch = c(16, NA, NA)) # Add legend


# Calculate the correlation coefficient
correlation2 <- cor(data$starch, data$treatment_n)
print(correlation2)

# Perform linear regression analysis
model2 <- lm(data$starch ~ data$treatment_n)
summary(model2)

# Make predictions
predictions <- predict(model2, data = data$treatment_one)
# Print the predictions
print(predictions)

# Obtain predictions with confidence intervals
pred_interval2 <- predict(model2, data = data$treatment_one, interval = "confidence")
# Print the predictions with confidence intervals
print(pred_interval2)

plot(data$starch, data$treatment_n, pch = 16, xlab = "x", ylab = "y", main = "Linear Regression") # Plot the observed data points
abline(model, col = "blue") # Add the regression line
lines(data$nsc, pred_interval[, "lwr"], col = "red", lty = 2) # Add confidence intervals
lines(data$nsc, pred_interval[, "upr"], col = "red", lty = 2)
legend("topright", legend = c("Data", "Regression Line", "Confidence Intervals"),
       col = c("black", "blue", "red"), lty = c(NA, 1, 2), pch = c(16, NA, NA)) # Add legend




############################################ Factor Correlations ----
#Data2#

#### Data preparation ###
# Subset the data frame without the factor variables
subset_data2 <- data2 %>% 
  select_if(negate(is.factor))

str(subset_data2)

cor_matrix <- cor(subset_data2)
# Create correlation plot
corrplot(cor_matrix, method = "circle", type = "full", tl.cex = 0.8)
# Extract correlations for the variables of interest (nsc and starch)
cor_subset <- cor_matrix[c("nsc", "starch"), ] #### Este es el que usas para plots solo de NSC y starch 
corrplot(cor_subset, method = "number", type = "full", tl.cex = 0.8)
# TleafEB, VPcham, SVPcham, airt, relhum, soiltemp <- most relevant correlations


#Correlation between two numerical variables, this should be performed after acquiring the most significant p values for the whole dataset
# ggstatsplot
## plot with statistical results
selected_variables <- c("nsc", "starch", "TleafEB", "VPcham", "SVPcham", "airt", "relhum", "soiltemp")
subset_data3 <- subset_data2[, selected_variables]

ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = soiltemp,
  bf.message = FALSE,
  marginal = FALSE # remove histograms
)
#TleafEB, SVPcham, relhum, soiltemp

############################################ Examples of plots ----
#Boxplot
chart <- ggplot(data, aes(x = time, y = nsc, fill = block)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(chart)

#Lines 
chart2 <- ggplot(data, aes(x = time, y = nsc, color = block)) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = mean_se, geom = "smooth", aes(group = block), method = "loess") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(chart2)

# Convert time to a factor with the desired order of levels
data$time <- factor(data$time, levels = c("700", "1000", "1300", "1600", "1900", "2200", "100", "400"), ordered = T) # Tiempos ordenados
summary_data <- summarySE(data, measurevar = "nsc", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)


############################################ NSCs plots (las buenas) ----

### NSCs by Block ###
chart3 <- ggplot(summary_data, aes(x = time, y = nsc, color = block)) +
  #geom_point(shape = 21, size = 3, fill = "white") +
  geom_smooth(aes(group = block), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) +
  labs(title = "NSCs by Block", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart3)


# NSCs by emitter treatment
chart5 <- ggplot(summary_data, aes(x = time, y = nsc, color = treatment)) +
  geom_smooth(aes(group = treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by Emitters (2L, 4L)", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart5)

# NSCs by hw_treatment
chart6 <- ggplot(summary_data, aes(x = time, y = nsc, color = hw_treatment)) +
  geom_smooth(aes(group = hw_treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by HW_Treatment", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart6)

# NSCs by row
chart7 <- ggplot(summary_data, aes(x = time, y = nsc, color = row)) +
  geom_smooth(aes(group = row), method = "loess", se = F, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by Row", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart7)

combined_plot = chart3 + chart5 + chart6 + chart7
print(combined_plot)


############################################ Starch plots ----

summary_data <- summarySE(data, measurevar = "starch", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)

### Starch by Block ###
chart3 <- ggplot(summary_data, aes(x = time, y = starch, color = block)) +
  #geom_point(shape = 21, size = 3, fill = "white") +
  geom_smooth(aes(group = block), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) +
  labs(title = "Starch by Block", x = "Time", y = "Starch (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart3)


# Starch by emitter treatment
chart5 <- ggplot(summary_data, aes(x = time, y = starch, color = treatment)) +
  geom_smooth(aes(group = treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "Starch by Treatment", x = "Time", y = "Starch (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart5)

# Starch by hw_treatment
chart6 <- ggplot(summary_data, aes(x = time, y = starch, color = hw_treatment)) +
  geom_smooth(aes(group = hw_treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "Starch by HW_Treatment", x = "Time", y = "Starch (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart6)

# Starch by row
chart7 <- ggplot(summary_data, aes(x = time, y = starch, color = row)) +
  geom_smooth(aes(group = row), method = "loess", se = F, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "Starch by Row", x = "Time", y = "Starch (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart7)

combined_plot = chart3 + chart5 + chart6 + chart7
print(combined_plot)



############################################ Extras #Additional correlation approaches (not used yet) ----
# correlation library ALL RELATIONSHIPS, WITH p-values
correlation(subset_data2, include_factors = TRUE, method = "auto")

# grouped correlations
data5 %>%
  group_by(time) %>%
  correlation()


# Correlation between two variables, with p-values displayed
ggpubr::ggscatter(subset_data2, x = "nsc", y = "TleafEB",
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "gray")
)+
  ggpubr::stat_cor(method = "pearson", label.x = 0.005, label.y = 0.005)  # Add correlation coefficient



#By block

ggpubr::ggscatter(data5, x = "nsc", y = "starch",
                  add = "reg.line",                         # Add regression line
                  conf.int = TRUE,                          # Add confidence interval
                  color = "block", palette = "jco",           # Color by groups "cyl"
                  shape = "block"                             # Change point shape by groups "cyl"
)+
  ggpubr::stat_cor(aes(color = block), label.x = 0.005)           # Add correlation coefficient



#2d density estimation
sp <- ggpubr::ggscatter(subset_data2, x = "nsc", y = "starch",
                        color = "gray")
sp + geom_density_2d()
# Gradient color
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Change gradient color: custom
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  ggpubr::gradient_fill(c("white", "steelblue"))
# Change the gradient color: RColorBrewer palette
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  ggpubr::gradient_fill("YlOrRd")


round(cor(subset_data2),
      digits = 2 # rounded to 2 decimals
)