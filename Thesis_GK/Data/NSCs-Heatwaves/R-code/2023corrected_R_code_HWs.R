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
library(ggsignif)
library(emmeans)
library(randomForest)
library(multcomp)
library(multcompView)
library(patchwork)
library(ggside)
library(ggstatsplot)
library(correlation)
library(forestplot)
library(caret)
library(stats)
library(pdp)
library(GGally)
library(car)
############################################ Data preparation ----
data <- read_excel("E:/NSCs-2023/BethMeeting/r2-hw2-2022.xlsx")
#View(marc_hw_treatments)
str(data)

data$block = as.factor(data$block)
data$treatment = as.factor(data$treatment)
data$hw_treatment = as.factor(data$hw_treatment)
data$id = as.factor(data$id)
data$row = as.factor(data$row)
data$time = as.factor(data$time)
data$time <- factor(data$time, levels = c("700", "1000", "1300", "1600", "1900", "2200", "100", "400"), ordered = T) # Tiempos ordenados
data$vine = as.factor(data$vine)
data$year = as.factor(data$year)
str(data)
summary(data)

############################################ Forest plots for highest values display----
# Order the data by descending nsc values
ordered_nsc <- data %>% arrange(desc(nsc))
ordered_starch <- data %>% arrange(desc(starch))
ordered_transpiration <- data %>% arrange(desc(E))

# Select the top 10 IDs with highest nsc values
top_10 <- head(ordered_nsc, 10)
top_10s <- head(ordered_starch, 10)
top_transpiration <- head(ordered_transpiration, 10)

top_10
top_10s
top_transpiration

### t.tests, ANOVAs, Wilcox & Tukeys, for treatment Comparison (NSCs & Starch) ###----

                                    ### NSCs ###
#First we need to look at the distribution of our data:
# histograms
hist(data$nsc, probability = TRUE, col = "lightblue", main = "NSC distribution", xlab = "NSCs values") #seems not normally distributed

#Easiest way to check, a Shapiro test 
shapiro.test(data$nsc) 
#??shapiro.test
#assess whether a sample of data comes from a normally distributed population. 
#based on the comparison between the observed sample data and the expected values under the assumption of a normal distribution.
#Null Hypothesis (H0): The null hypothesis of the test is that the data follows a normal distribution.
#Alternative Hypothesis (Ha): The alternative hypothesis is that the data does not follow a normal distribution.

#Test Statistic (W): The test statistic (W) is calculated from the ordered sample values. 
#Is a measure of how closely the sample distribution resembles a normal distribution. 
#The closer W is to 1, the more likely the data is normally distributed.
#The p-value associated with the test statistic is used to determine the significance of the result. 
#If the p-value is less than the significance level (commonly 0.05), null hypothesis is rejected, 
#indicating that the data is significantly different from a normal distribution.
#Which is usually corroborated by the use of a qqnorm and qqline test

# Q-Q Plots
qqnorm(data$nsc)
qqline(data$nsc)#not normal

# Density Plot
plot(density(data$nsc), main = "Density Plot") #nope, not normal 

                                                ### t.test

# 2L to 4L t-test
#Objective: Determine if the mean of a single group is significantly different from a hypothesized 
#population mean or the mean of another group.
nsc_result <- t.test(nsc ~ treatment, data = data, conf.level = 0.99)
nsc_result2 <- t.test(nsc ~ treatment_n, data = data, conf.level = 0.99) #numeric treatment, for after predictions
nsc_hw <- t.test(nsc ~ hw_treatment, data = data, conf.level = 0.99)
# Print the results
print(nsc_result)
print(nsc_result2) # No difference between treatments
print(nsc_hw)

# However, we assume that the variances between treatments will be different, therefore we need to run a Welch test as well (more)
welch_test <- t.test(nsc ~ treatment, data = data, var.equal = FALSE, conf.level = 0.99)
print(welch_test)

# Perform pairwise t-tests with Bonferroni AND holm correction. "holm" is better for type II errors
#Objective: Specifically designed for comparing means across multiple groups or treatments. However, in here I did it separately first

                    # Bonferroni corrections
# Adjusts the significance level by dividing it by the number of comparisons. 
# Is a more conservative approach, suitable for controlling the overall Type I error rate when conducting multiple tests.

posthoc_correctedp <- pairwise.t.test(data$nsc, data$treatment, p.adjust.method = "bonferroni")
posthoc_correctedp2 <- pairwise.t.test(data$nsc, data$hw_treatment, p.adjust.method = "bonferroni")

# Print the pairwise comparisons
print(posthoc_correctedp)
print(posthoc_correctedp2)

# Now the same, but accounting for the treatment interactions
data$combined_treatment <- interaction(data$treatment, data$hw_treatment) # Create a new factor representing the combination of treatments

# Perform pairwise t-test for the combined treatments
posthoc_combined <- pairwise.t.test(data$nsc, data$combined_treatment, p.adjust.method = "bonferroni")
print(posthoc_combined)

#The result matrix represents the results of pairwise comparisons using t tests with pooled standard deviation 
#on the variables (nsc & the combined_treatment). Each cell in the matrix indicates whether a pairwise comparison 
#is significant (<1) or not (>1).
# In this case :
#2L.NS vs. 4L.NS: Not significant difference (p-value = 1)
#2L.NS vs. 2L.S: Not Significant (1)
#4L.NS vs. 2L.S: Not significant
#2L.S vs. 4L.S: Not Significant

# The results make sense, as the Bonferroni correction is better suited for type 1 error, let's try holms

                          #holm
holm_correctedp <- pairwise.t.test(data$nsc, data$treatment, p.adjust.method = "holm")
holm_correctedp2 <- pairwise.t.test(data$nsc, data$hw_treatment, p.adjust.method = "holm")

# Print the pairwise comparisons
print(holm_correctedp)
print(holm_correctedp2)

# use data_combined_treatment again as the new factor of interactions 
holm_combined <- pairwise.t.test(data$nsc, data$combined_treatment, p.adjust.method = "holm")
print(holm_combined)
#Same results, following the expected, as the data distribution is not normal

                                                #Wilcox tests 

#### NOW, assuming NOT NORMAL distribution of the data
wilcox_test <- wilcox.test(nsc ~ treatment, data = data, conf.level = 0.99) # Works similar to a t.test, but for smaller sample sizes, often used to verify t.test results
print(wilcox_test)
#When we work with non normal data, it seems that the treatments have indeed a significant difference 

wilcox_stress <- wilcox.test(nsc ~ hw_treatment, data = data, conf.level = 0.99) 
print(wilcox_stress) #not so much when considering only the stress treatments

#However, we also need to take into consideration that there might be differences between treatments when thinking about time!
#The not efficient way (one by one)
wilcox_time <- wilcox.test(nsc ~ treatment, data = data[data$time == "700",])
print(wilcox_time) #not significant for the 7 am , makes sense, it's not hot!

#More efficient
time_levels <- levels(data$time) # Get all time levels

# Create an empty list to store the results
wilcox_results_list <- list()

# Iterate over each time level
for (time_level in time_levels) {
  # Perform Wilcoxon rank-sum test
  wilcox_result <- wilcox.test(nsc ~ treatment, data = data[data$time == time_level,])
  
  # Print the result
  print(paste("Time Level:", time_level))
  print(wilcox_result)
  
  # Store the result in the list
  wilcox_results_list[[time_level]] <- wilcox_result
}

# Access specific results from the list
wilcox_results_list[["700"]]
wilcox_results_list[["1000"]]
wilcox_results_list[["1300"]]
wilcox_results_list[["1600"]]#here the treatment has a significant difference
wilcox_results_list[["1900"]]#even more relevant at 7 pm
wilcox_results_list[["2200"]]# and it seems to have a really strong effect at the recovery hours
wilcox_results_list[["100"]]
wilcox_results_list[["400"]]

                                          # ANOVAs
nsc_result1 <- aov(nsc ~ treatment, data = data) # bornig, and the residuals are not notmally distributed
nsc_result2 <- aov(nsc ~ hw_treatment, data = data)#even more boring 
nsc_result3 <- aov(nsc ~ treatment + hw_treatment, data = data) 
nsc_result4 <- aov(nsc ~ treatment * hw_treatment, data = data)
nsc_result5 <- aov(nsc ~ time, data = data)
nsc_result6 <- aov(nsc ~ time + treatment + block + hw_treatment, data = data)
nsc_result7 <- aov(nsc ~ time * treatment * hw_treatment * block, data = data)

# Print the results
print(summary(nsc_result1))
print(summary(nsc_result2))
print(summary(nsc_result3))
print(summary(nsc_result4))
print(summary(nsc_result5))
print(summary(nsc_result6))
print(summary(nsc_result7))

shapiro.test(residuals(nsc_result7)) # the residuals seem fairly normal 
qqnorm(nsc_result7$residuals)
qqline(nsc_result7$residuals) #not quite huh? ↑

#Let's dive on why time has so much relevance by performing separate analyses for each level of time
time_contrasts <- contrasts(data$time) # Create contrasts for each level of the time factor
num_levels <- nlevels(data$time)
p_values <- numeric(num_levels) # Create an empty vector to store p-values

for (i in 1:num_levels) { #starts a loop that iterates over each level of the time factor.
  # Create a contrast matrix for the specific level
  contrast_matrix <- matrix(0, nrow = 1, ncol = num_levels) #matrix filled with 0s (one row and and #columns based on time levels)
  contrast_matrix[1, i] <- 1 # sets elements in the "i" column of the first row of the contrast matrix
  
# Perform a type-II ANOVA
anova_result <- Anova(nsc_result7, idata = data.frame(data$time), idesign = ~data$time, type = "II")
# Extract the p-value for the contrast
p_values[i] <- anova_result$"Pr(>F)"[i]
}
# Combine results into a data frame
results_df <- data.frame(Time_Level = levels(data$time), P_Value = p_values)
# Print or further analyze the results
print(results_df)


                                                          # Tukeys
# Perform Tukey's HSD test
posthoc1 <- TukeyHSD(nsc_result3, conf.level = 0.99)
posthoc2 <- TukeyHSD(nsc_result4, conf.level = 0.99)
posthoc3 <- TukeyHSD(nsc_result5, conf.level = 0.99)
posthoc4 <- TukeyHSD(nsc_result6, conf.level = 0.99)
posthoc7 <- TukeyHSD(nsc_result7, conf.level = 0.99)

# Print the pairwise comparisons
print(posthoc1)
print(posthoc2)
print(posthoc3)
print(posthoc4)
print(posthoc7)
posthoc7$`treatment:hw_treatment`
posthoc7$`time:treatment`# the relevance of time is expected

tukey_result <- glht(nsc_result7, linfct = mcp(block = "Tukey"))
# Summarize the Tukey's test results
summary(tukey_result)


                                 ### STARCH ###
### t. tests 
# 2L to 4L
starch_result <- t.test(starch ~ treatment, data = data, conf.level =0.99)
starch_result2 <- t.test(starch ~ treatment_n, data = data, conf.level =0.99) #numeric treatment, for alter predictions
# Print the results
print(starch_result)
print(starch_result2)

# Corroborate the results with a pairwise t-test with a Bonferroni correction in the p-values
posthoc_correctedp <- pairwise.t.test(data$starch, data$treatment, p.adjust.method = "bonferroni")
# Print the pairwise comparisons
print(posthoc_correctedp) # In starch levels, there are no significant changes between treatments

### ANOVAs
# Perform ANOVA to corroborate the results again 
starch_result3 <- aov(starch ~ treatment + hw_treatment, data = data)
starch_result4 <- aov(starch ~ treatment * hw_treatment, data = data)
starch_result5 <- aov(starch ~ time * treatment * hw_treatment, data = data)

# Print the results
print(summary(starch_result3))
print(summary(starch_result4))
print(summary(starch_result5)) # Not significant changes between treatments on Starch levels

### Tukeys
# Lastly Perform Tukey's HSD (Honest Significant Difference) test
# compare the means of multiple groups. It allows for pairwise comparisons between 
# all groups to determine if there are significant differences among them.
posthoc1 <- TukeyHSD(starch_result3)
posthoc2 <- TukeyHSD(starch_result4)
posthoc3 <- TukeyHSD(starch_result5)

# Print the pairwise comparisons
print(posthoc1)
print(posthoc2)
print(posthoc3) # No difference between any


posthoc3$`time:treatment:hw_treatment`
result<-data.frame(posthoc3$`time:treatment:hw_treatment`)
result["p.adj"]
result <- result[order(result$p.adj, decreasing = TRUE), ]
tail(result, 20) # Nothing really has a significant value wihtout accounting for weird interactions, like 4 pm to 4 am, which 
#of course it does not help a lot

### Difference and relevance between time slots ----

#1 Create a subset of data for the time intervals of interest (100 and 400)
subset_data <- data[data$time %in% c(1000, 400), ]
#2 Perform pairwise t-tests for the time intervals
pairwise_result <- pairwise.t.test(subset_data$nsc, subset_data$time, p.adjust.method = "bonferroni")
#3 Print the pairwise comparison results
print(pairwise_result) # p = 2.2e-13 ***


############################################ Which are the most relevant times for NSCs?
# Create a custom contrast matrix so the times appear ordered, not like in previous models (Specific order)
#he first row -1 1 0 0 0 0 0 0 represents a contrast that subtracts the mean at the first time point (700) from 
#the mean at the second time point (1000), effectively testing the difference between these two means.
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
# It seems like the most relevant times for NSCs are of course the recovery hours, altough there is an odd interaction
# from 4pm to 7 pm, can it be due to sudden change in temperature/sun exposure of the leaves? 


############################################ Irrigation treatment numerical (2-4L), Linear regressions, Tukeys, t.test (data) ----
#Play a bit with predictions based on the data (numeric values for irrigation)

                                    ### NSCs ###

# Calculate the correlation coefficients
correlation <- cor(data$nsc, data$treatment_n)
print(correlation)

# Perform linear regression analysis
model <- lm(data$nsc ~ data$treatment_n)
summary(model)


#### Model broken down
# Coefficients:
# The coefficients section presents the estimates for the intercept and the coefficient associated with the data$treatment_n 
# variable. These estimates represent the expected change in the dependent variable for each unit increase in the independent 
# variable.

# The estimate for the intercept is 79.3939, suggesting that when data$treatment_n (irrigation) is zero, the expected value 
# of NSCs is ~79.3939. The p-value associated with this (Intercept) is the probability of observing
# that value for the intercept coefficient, assuming the null hypothesis that the true value of the 
# coefficient is zero. WHICH IS NOT THE CASE FOR OUR TREATMENTS, a small p-value suggests strong evidence against the null 
# hypothesis. Therefore, in this case, the small p-value for the intercept indicates that the intercept coefficient 
# is statistically significant, meaning that the y-intercept is significantly different from zero.

# The estimate for data$treatment_n is 0.5776, indicating that for each unit increase in data$treatment_n, the 
# expected change in data$nsc is 0.5019. However, the associated p-value for this coefficient is 0.758 
# which is not statistically significant. Predictions might improve the more data we have

# Residual standard error:
# The residual standard error (RSE) is an estimate of the standard deviation of the residuals. In this case, the 
# RSE is 22.32, indicating the average amount by which the observed data$nsc values deviate from the predicted values
# is 22.32! A LOT!

# Multiple R-squared and Adjusted R-squared:
# The multiple R-squared value represents the proportion of variability in the dependent variable that can be explained 
# by the independent variable(s). In this case, the R-squared value is 0.0006702 suggesting that the data$treatment_n 
# variable explains a very small fraction of the variability in data$nsc. The adjusted R-squared value takes into account 
# the number of predictors in the model, and in this case, it is slightly negative (-0.006367), suggesting that the model does 
# not improve the prediction of data$nsc compared to a simple mean.

# F-statistic and p-value:
# The F-statistic assesses the overall significance of the model by comparing the variability explained by the model to 
# the unexplained variability. In this example, the F-statistic is 0.09523, with a corresponding p-value of 0.7581, indicating 
# that the model is not statistically significant in explaining the variability in data$nsc.
# that treatment have no significant effect on the NSCs levels


## THEREFORE, FOR THIS PARTICULAR HEATWAVE, IT IS NOT RELEVANT TO MAKE PREDICTIONS BASED ON WATER ##
# Let's do it just for the fun
# NSCs predictions with variable water treatments
# New data for prediction
data$treatment_one = data$treatment_n-1
data$treatment_one = data$treatment_n-1.5

# Make predictions
predictions <- predict(model, data = data$treatment_one)
# Print the predictions
print(predictions)

# Obtain predictions with confidence intervals
pred_interval <- predict(model, data = data$treatment_one, interval = "confidence")
# Print the predictions with confidence intervals
print(pred_interval)

                                    ### Starch ###

# Calculate the correlation coefficient
correlation2 <- cor(data$starch, data$treatment_n)
print(correlation2) # a negative correlation

# Perform linear regression analysis
model2 <- lm(data$starch ~ data$treatment_n)
summary(model2)

# Make predictions
predictions <- predict(model2, data = data2$treatment_one)
# Print the predictions
print(predictions)

# Obtain predictions with confidence intervals
pred_interval2 <- predict(model2, data = data2$treatment_one, interval = "confidence")
# Print the predictions with confidence intervals
print(pred_interval2)

#### Approach #2
lm_result <- lm(nsc ~ treatment_n, data = data)
# View the regression model summary
summary(lm_result)

# Create a new treatment_n variable with the desired values
new_treatment <- c(0.5, 1, 1.5, 2.5, 3, 3.5, 4.5, 5, 5.5, 6)

# Predict nsc for the new treatment values
predicted_nsc <- predict(lm_result, newdata = data.frame(treatment_n = new_treatment))

# View the predicted nsc values
predicted_nsc
## Again, this won't be a great help, because the amount of water seem to have very little effect on the NSCs or Starch levels

#BUT!
### If is not water, then what is it? Random Forest model for variable importance and predictions----

#Splitting data into training and test sets 
set.seed(666)  # Set a seed for reproducibility
train_indices <- createDataPartition(data$nsc, times = 1, p = 0.7, list = FALSE) #70% of the data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Random Forest model for nsc
model_nsc <- randomForest(nsc ~ ., data = train_data, ntree = 5000, importance = TRUE)

# Random Forest model for starch
model_starch <- randomForest(starch ~ ., data = train_data, ntree = 5000, importance = TRUE)
#(ntree) is the number of trees in the forest

#### Predict on test data
nsc_pred <- predict(model_nsc, newdata = test_data)
starch_pred <- predict(model_starch, newdata = test_data)

# Assess model performance
nsc_rmse <- sqrt(mean((nsc_pred - test_data$nsc)^2))
starch_rmse <- sqrt(mean((starch_pred - test_data$starch)^2))

# Print RMSE (Root Mean Squared Error) for each model, average difference between actual 
# values and predicted values. The lower, the better. 
print(paste("RMSE for nsc:", nsc_rmse)) # BAAAAAAAAAD
print(paste("RMSE for starch:", starch_rmse)) # NOT AS BAD

# Variable importance for nsc
varimp_nsc <- importance(model_nsc)
# print(varimp_nsc) not ordered results
varimp_nsc_ordered <- varimp_nsc[order(varimp_nsc[, 1]), ] # %IncMSE is ordered from smallest to largest
print(varimp_nsc_ordered)

# Variable importance for "starch"
varimp_starch <- importance(model_starch)
#print(varimp_starch)
varimp_starch_ordered <- varimp_starch[order(varimp_starch[, 1]), ] # %IncMSE is ordered from smallest to largest
print(varimp_starch_ordered)

##### Output of the models:
# "%IncMSE" (percentage increase in mean squared error) & 
# "IncNodePurity" (increase in node purity).
''' 
"%IncMSE": This metric measures the increase in mean squared error when a variable is 
randomly permuted or removed from the model. Higher values indicate greater importance of the 
variable in predicting the target variable (nscs). For example, variables like "sugar_perc" 
and "leafwp" have high "%IncMSE" values, suggesting they have a significant impact on 
predicting "nsc". THE HIGHER, THE MORE IMPORTANT ON PREDICTED VARIABLE

"IncNodePurity": This metric measures the increase in node purity (impurity reduction) 
associated with a variable. Variables with higher values contribute more to the overall 
purity of the nodes in the decision trees. In this case, variables like "VPcham" and "SVPleaf" 
have high "IncNodePurity" values, indicating their importance in predicting "nsc".

From the results, the variables displaying  anegative sign, negatively contribute to the 
accuracy of the forest models, and therefore we can remove them:

Which seems interesting, because this indicates that there is a possibility to predict the value of NSCs based on 
some variables in the Li-Cor readings (VPcham), gtw, SVPlead, Emm

The more data we feed into the model predictor, the more accurate wil be on which variables to pay attention to. 
With some exceptions, like sugar %, which is just a variable created from the NSC values itself. Which explains its 
high numbers on the predictions. 
Does it make any sense based on what the other variables are?
(VPcham) Vapor pressure in the chamber
(gtw) total conductance to water vapor
(SVPleaf) Saturation vapor pressure at leaf temp
(Emm) Transpiration Rate
(TleafEB) 
However, we have to keep in mind that these importance scores do not provide information about the direction or nature of the relationship. 
They only indicate the importance of each variable in terms of predictive accuracy. 
Further analysis: Maybe examining partial dependence plots (pdp) or variable interactions could provide more insights into how these 
variables influence the NSCs values.
Also, when interpreting variable importance, it is crucial to consider potential collinearity.

'''


''' Linear regression assumes a linear relationship between the predictors and the response variable 
and is suitable when certain assumptions, such as linearity, independence, homoscedasticity, and normality of residuals, are met.

'''

################################ MODELS ###########################################
# Based on this we can recreate the lm model and play with other approaches as well
#asses linearity

# Selecting only numeric variables
# Selecting only numeric variables
numeric_variables <- sapply(data, is.numeric)
numeric_data <- data[, numeric_variables]
numeric_data <- numeric_data[, !colnames(numeric_data) %in% c("Rabs", "TleafCnd", "TleafEB", "gtc","gtw", "Pca", "Pci", "RHcham", "VPcham", "SVPcham","LathFlux", "SenHFlux","NetTherm", "Qabs_fs")]

# Adding 'nsc' to the numeric data
numeric_data$nsc <- data$nsc

# Create a categorical variable based on quartiles
numeric_data$nsc_category <- cut(numeric_data$nsc, breaks = 4, labels = c("Q1", "Q2", "Q3", "Q4"))

# Creating a scatterplot matrix using GGally
ggpairs(numeric_data, columns = 1:ncol(numeric_data), aes(color = nsc_category))


full_model = lm(nsc ~ soiltemp + Ca + A + relhum + airt + TleafEB + Emm + gtw + SVPleaf + VPcham + treatment_n, data = data)
summary(full_model)

#Soil temperature linearity with nscs
plot(data$soiltemp, data$nsc, xlab = "Soil Temperature", ylab = "NSC", main = "Soil Temperature vs. NSC")
abline(lm(nsc ~ soiltemp, data = data), col = "red")

#relative humidity
plot(data$soiltemp, data$relhum, xlab = "Relative Humidity", ylab = "NSC", main = "Relative Humidity vs. NSC")
abline(lm(nsc ~ relhum, data = data), col = "red")



model2 = lm(nsc ~ relhum + airt + soiltemp + relhum*airt*soiltemp + soiltemp*airt*treatment_n, data = data)
summary(model2)

interaction_model <- lm(nsc ~ soiltemp * Ca * A * relhum * airt * TleafEB * Emm * gtw * SVPleaf * VPcham, data = data)
summary(interaction_model)

## And we can use more to test

graphics.off() # this rests the graphics device 
pdp_soiltemp <- partial(model_nsc, pred.var = "soiltemp", train_data)
ggplot(pdp_soiltemp, aes(x = soiltemp, y = yhat)) + #in this case yhat = predicted variable
  geom_line() +
  labs(title = "Partial Dependence Plot for 'soiltemp' on NSC")

# I can play with some of the correlations within the model predictions
cor_matrix <- cor(train_data[, c("nsc","leafwp", "gbw", "Ca", "A", "SVPcham", "soiltemp")])
print(cor_matrix)


#### THIS IS NOT STRICTLY NECESSARY, WHILE IMPROVES THE ACCURACY OF THE MODEL, THE DIFFERENCE 
# IS NOT SIGNIFICANT, LO HICISTE PARA VER SI AFECTABA< NO LO REPITAS CON EL STARCH!!

# Identify variables with non-negative %IncMSE values
positive_vars <- rownames(varimp_nsc[varimp_nsc[, "%IncMSE"] >= 0, , drop = FALSE])

# Subset the training and test data to include only the positive variables
train_data_filtered <- train_data[, c(positive_vars, "nsc")]
test_data_filtered <- test_data[, c(positive_vars, "nsc")]

# Fit a new Random Forest model using the filtered data
model_nsc_filtered <- randomForest(nsc ~ ., data = train_data_filtered, ntree = 5000, importance = TRUE)

nsc_pred <- predict(model_nsc_filtered, newdata = test_data)
nsc_rmse <- sqrt(mean((nsc_pred - test_data$nsc)^2))
print(paste("RMSE for nsc:", nsc_rmse))
# Variable importance for "nsc" filtered 
varimp_nsc <- importance(model_nsc_filtered)
# print(varimp_nsc) not ordered results
varimp_nsc_ordered <- varimp_nsc[order(varimp_nsc[, 1]), ] # %IncMSE is ordered from smallest to largest
print(varimp_nsc_ordered)

############################################ Factor Correlations & FUll variables and correlations with NSCs ----
#Data2#

#### Data preparation ###
# Subset the data frame without the factor variables
subset_data2 <- data %>% 
  select_if(negate(is.factor))

str(subset_data2)

cor_matrix <- cor(subset_data2)
# Create correlation plot
corrplot(cor_matrix, method = "circle", type = "full", tl.cex = 0.8)

# Extract correlations for the variables of interest (nsc and starch)
cor_subset <- cor_matrix[c("nsc", "starch"), ] #### Este es el que usas para plots solo de NSC y starch 
corrplot(cor_subset, method = "number", type = "full", tl.cex = 0.8)
# Relevant variables to NSCs and Starch:

#From Li-Cor gas exchange:
#TleafEB - in Â°C - Leaf temperature from energy balance
#VPcham - kPa - Vapor pressure in the chamber
#SVPcham - kPa - Saturation vapor pressure in the chamber

# From SIMIS data:
#airt, relhum, soiltemp

#Correlation between two numerical variables, this should be performed after acquiring the most significant p values for the whole dataset
# ggstatsplot
## plot with statistical results
selected_variables <- c("nsc", "starch", "TleafEB", "VPcham", "SVPcham", "airt", "relhum", "soiltemp")
subset_data3 <- subset_data2[, selected_variables]

### Correlation to numerical Variables (only plots) NSCs ###----
sp1 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = airt,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)
plot(sp1)
sp2 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = relhum,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
  )

sp3 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = soiltemp,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sp4 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = TleafEB,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sp5 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = VPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sp6 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = SVPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

# Combine the scatterplots into a single image
combined_plot <- sp1 + sp2 + sp3 + sp4 + sp5 + sp6

# Display the combined plot
combined_plot

### Correlation to numerical Variables (only plots) Starch ----

sps1 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = airt,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)
#plot(sp1)

sps2 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = relhum,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps3 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = soiltemp,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps4 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = TleafEB,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps5 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = VPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps6 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = SVPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

# Combine the scatterplots into a single image
combined_plot2 <- sps1 + sps2 + sps3 + sps4 + sps5 + sps6

# Display the combined plot
combined_plot2


############################################ Box Plots, by Block, treatment, hw_treatment & row ----

                                    ### NSCs ###
### Boxplots
# Block, treatment, hw_treatment, row
by_blocks <- ggplot(data, aes(x = time, y = nsc, fill = block)) +
  geom_boxplot() +
  labs(x = "Time", y = "NSC (Mean)") +
  ggtitle("NSC by Block") +
  theme_minimal()
print(by_blocks)

by_treatment <- ggplot(data, aes(x = time, y = nsc, fill = treatment)) +
  geom_boxplot() +
  labs(x = "Time", y = "NSC (Mean)") +
  ggtitle("NSC by Irrigation Treatment") +
  theme_minimal()
print(by_treatment)

by_hwtreatment <- ggplot(data, aes(x = time, y = nsc, fill = hw_treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal() + 
  scale_fill_manual(values = c("NS" = "light blue", "S" = "red"))
print(by_hwtreatment)

by_row <- ggplot(data, aes(x = time, y = nsc, fill = row, group =)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSCs (Mean)") +
  theme_minimal()
print(by_row)

# Combine the scatterplots into a single image
combined_plot3 <- by_blocks + by_treatment + by_hwtreatment + by_row +
  ggtitle("NScs")+
  theme(plot.title = element_text(size = 20),
        plot.title.position = "panel")
# Display the combined plot
combined_plot3

                                    ### Starch ###
### Boxplots
# Block, treatment, hw_treatment, row
sby_blocks <- ggplot(data, aes(x = time, y = starch, fill = block)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal()
print(sby_blocks)

sby_treatment <- ggplot(data, aes(x = time, y = starch, fill = treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal()
print(sby_treatment)

sby_hwtreatment <- ggplot(data, aes(x = time, y = starch, fill = hw_treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal() + 
  scale_fill_manual(values = c("NS" = "light blue", "S" = "red"))
print(sby_hwtreatment)

sby_row <- ggplot(data, aes(x = time, y = starch, fill = row)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal()
print(sby_row)

# Combine the scatterplots into a single image
combined_plot4 <- sby_blocks + sby_treatment + sby_hwtreatment + sby_row + 
  ggtitle("Starch")+
  theme(plot.title = element_text(size = 20),
        plot.title.position = "panel")

# Display the combined plot
combined_plot4

###2nd Plot Option, by Lines---- 
# Base code #
chart2 <- ggplot(data, aes(x = time, y = nsc, color = block)) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = mean_se, geom = "smooth", aes(group = block), method = "loess") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(chart2)

# Convert time to a factor with the desired order of levels
#data$time <- factor(data$time, levels = c("700", "1000", "1300", "1600", "1900", "2200", "100", "400"), ordered = T) # Tiempos ordenados
summary_data <- summarySE(data, measurevar = "starch", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)
summary_data2 <- summarySE(data, measurevar = "nsc", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)


                            ### NSCs by Block ###
chart3 <- ggplot(data2, aes(x = time, y = nsc, color = block)) +
  #geom_point(shape = 21, size = 3, fill = "white") +
  geom_smooth(aes(group = block), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) +
  labs(title = "NSCs by Block", x = "Time", y = "NSC (Mean) in Âµg/mg in dry sample") +
  theme_minimal()
print(chart3)

# NSCs by emitter treatment
chart5 <- ggplot(summary_data2, aes(x = time, y = nsc, color = treatment)) +
  geom_smooth(aes(group = treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by Emitters (2L, 4L)", x = "Time", y = "NSC (Mean) in Âµg/mg in dry sample") +
  theme_minimal()
print(chart5)

# NSCs by hw_treatment
chart6 <- ggplot(summary_data2, aes(x = time, y = nsc, color = hw_treatment)) +
  geom_smooth(aes(group = hw_treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by HW_Treatment", x = "Time", y = "NSC (Mean) in Âµg/mg in dry sample") +
  theme_minimal()
print(chart6)

# NSCs by row
chart7 <- ggplot(summary_data2, aes(x = time, y = nsc, color = row)) +
  geom_smooth(aes(group = row), method = "loess", se = F, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by Row", x = "Time", y = "NSC (Mean) in Âµg/mg in dry sample") +
  theme_minimal()
print(chart7)

combined_plot = chart3 + chart5 + chart6 + chart7
print(combined_plot)

summary_data11 <- summarySE(data, measurevar = "starch", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)
summary_data11

### Numerical comparisson ()





############################################ Extras #Additional correlation approaches (not used yet) ----
# correlation library ALL RELATIONSHIPS, WITH p-values
correlation(subset_data2, include_factors = TRUE, method = "auto")

# grouped correlations
data %>%
  group_by(time) %>%
  correlation()


# Correlation between two variables, with p-values displayed
ggpubr::ggscatter(subset_data2, x = "nsc", y = "prune_w",
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "gray")
)+
  ggpubr::stat_cor(method = "pearson", label.x = 0.005, label.y = 0.005)  # Add correlation coefficient



#By block

ggpubr::ggscatter(data, x = "nsc", y = "prune_w",
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
############################################ nsc & starch with predicted treatment values ----

#NSCs#
correlation <- cor(data$nsc, data$treatment_n)
correlation

# Fit a linear regression model
model <- lm(nsc ~ treatment_n, data = data)
summary(model)

# Predict nsc values for specific treatment_n levels
#new_treatment_n <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 3.5, 4.5, 5)
new_treatment_n <- seq(0.1,5, by = 0.1)
predictions <- predict(model, newdata = data.frame(treatment_n = new_treatment_n))

# Print the predicted nsc values
predictions

# Create a data frame with the predicted values and corresponding treatment_n levels
prediction_data <- data.frame(nsc = predictions, new_treatment_n = new_treatment_n)

# Plot the predictions
ggplot(prediction_data, aes(x = new_treatment_n, y = nsc)) +
  geom_point() +
  xlab("Water in Liters/hour") +
  ylab("nsc in Âµg/mg") +
  ggtitle("Predicted nsc values vs. new_treatment_n levels")


# Perform pairwise comparisons between predictions and treatment_n levels
comparison_results <- data.frame(new_treatment_n, predictions)

# Print the comparison results
print(comparison_results)

# Perform pairwise Wilcoxon signed-rank tests to see if the difference between the prediction value results are significant
pairwise_tests <- pairwise.wilcox.test(comparison_results$predictions, comparison_results$new_treatment_n, p.adjust.method = "bonferroni")

# Print the pairwise test results
print(pairwise_tests)

# The diagonal elements (e.g., 0.2, 0.3, 0.4, etc.) represent the comparison of a new_treatment_n level with itself, so the 
# p-value is always 1 (indicating no significant difference).
# 
# The upper and lower triangular elements represent the pairwise comparisons between the new_treatment_n levels. In this case, all 
# the p-values are 1, indicating no significant difference between the nsc levels for any pair of new_treatment_n values.
# 
# Since all the p-values are 1, there is no evidence to suggest a significant difference between any pair of nsc levels based on 
# the new_treatment_n values in the comparison_results data frame.


### Starch
correlation <- cor(data$starch, data$treatment_n)
correlation

# Fit a linear regression model
model <- lm(starch ~ treatment_n, data = data)
summary(model) # the results suggest that maybe we should not use a linear regression approach

# Predict nsc values for specific treatment_n levels
#new_treatment_n <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 3.5, 4.5, 5)
new_treatment_n <- seq(0.1,5, by = 0.1)
predictions <- predict(model, newdata = data.frame(treatment_n = new_treatment_n))

# Print the predicted nsc values
predictions

# Create a data frame with the predicted values and corresponding treatment_n levels
prediction_data <- data.frame(nsc = predictions, new_treatment_n = new_treatment_n)

# Plot the predictions
ggplot(prediction_data, aes(x = new_treatment_n, y = nsc)) +
  geom_point() +
  xlab("Water in Liters/hour") +
  ylab("Starch in Âµg/mg") +
  ggtitle("Predicted nsc values vs. new_treatment_n levels")


# Fit a GAM using the mgcv package
library(mgcv)
model <- gam(nsc ~ s(treatment_n), data = data)
summary(model)


##

# Filter data for each treatment
data_2L <- data[data$treatment == "2L", ]
data_4L <- data[data$treatment == "4L", ]

ggplot(data, aes(x = time)) +
  geom_point(aes(y = nsc, color = treatment), position = position_dodge(width = 0.8), size = 2) +
  geom_point(aes(y = Emm, color = treatment), position = position_dodge(width = 0.8), size = 2, shape = 2) +
  stat_summary(aes(y = nsc, group = treatment), fun = "mean", geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Emm, group = treatment), fun = "mean", geom = "line", linetype = "dashed", size = 1) +
  labs(title = "Average NSC and E over Time",
       x = "Time",
       y = "Value") +
  scale_color_manual(values = c("2L" = "blue", "4L" = "red")) +
  theme_minimal()
print(plot_E)

## secondary y-axis added (is on the same scale as the nsc one)
ggplot(data, aes(x = time)) +
  geom_point(aes(y = nsc, color = treatment), position = position_dodge(width = 0.8), size = 2) +
  geom_point(aes(y = Emm, color = treatment), position = position_dodge(width = 0.8), size = 2, shape = 2) +
  stat_summary(aes(y = nsc, group = treatment), fun = "mean", geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = Emm, group = treatment), fun = "mean", geom = "line", linetype = "dashed", size = 1) +
  labs(title = "Average NSC and E over Time",
       x = "Time",
       y = "NSC Value") +
  scale_color_manual(values = c("2L" = "blue", "4L" = "red")) +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red")) +  # Color of the secondary y-axis title
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Emm Value", labels = scales::comma))  # Setting up the secondary y-axis

## axis on the right adjusted to the Emm variable (still looks small)
ggplot(data, aes(x = time)) +
  geom_point(aes(y = nsc, color = treatment), position = position_dodge(width = 0.8), size = 2) +
  geom_point(aes(y = scale(Emm), color = treatment), position = position_dodge(width = 0.8), size = 2, shape = 2) +
  stat_summary(aes(y = nsc, group = treatment), fun = "mean", geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = scale(Emm), group = treatment), fun = "mean", geom = "line", linetype = "dashed", size = 1) +
  labs(title = "Average NSC and scaled Emm over Time",
       x = "Time",
       y = "NSC Value") +
  scale_color_manual(values = c("2L" = "blue", "4L" = "red")) +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red")) +  # Color of the secondary y-axis title
  scale_y_continuous(sec.axis = sec_axis(~scale(.) * sd(data$Emm) + mean(data$Emm), name = "Emm Value"))  # Scaling and setting up the secondary y-axis

## NSCs and Starch
ggplot(data, aes(x = time)) +
  geom_point(aes(y = nsc, color = treatment), position = position_dodge(width = 0.8), size = 2) +
  geom_point(aes(y = scale(starch), color = treatment), position = position_dodge(width = 0.8), size = 2, shape = 2) +
  stat_summary(aes(y = nsc, group = treatment), fun = "mean", geom = "line", linetype = "solid", size = 1) +
  stat_summary(aes(y = scale(starch), group = treatment), fun = "mean", geom = "line", linetype = "dashed", size = 1) +
  labs(title = "Average NSC and scaled Starch over Time",
       x = "Time",
       y = "NSC Value") +
  scale_color_manual(values = c("2L" = "light green", "4L" = "light blue")) +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red")) +  # Color of the secondary y-axis title
  scale_y_continuous(sec.axis = sec_axis(~scale(.) * sd(data$starch) + mean(data$starch), name = "Starch Value"))  # Scaling and setting up the secondary y-axis



### purrr approach
numeric_variables <- names(select_if(data, is.numeric))

# Create a list of plots for each numeric variable
plots <- map(numeric_variables, function(var) {
  ggplot(data, aes(x = time)) +
    geom_point(aes_string(y = "nsc", color = "treatment"), position = position_dodge(width = 0.8), size = 2) +
    geom_point(aes_string(y = paste0("scale(", var, ")"), color = "treatment"), position = position_dodge(width = 0.8), size = 2, shape = 2) +
    stat_summary(aes_string(y = "nsc", group = "treatment"), fun = "mean", geom = "line", linetype = "solid", size = 1) +
    stat_summary(aes_string(y = paste0("scale(", var, ")"), group = "treatment"), fun = "mean", geom = "line", linetype = "dashed", size = 1) +
    labs(title = paste("Average NSC and scaled", var, "over Time"),
         x = "Time",
         y = "NSC Value") +
    scale_color_manual(values = c("2L" = "light green", "4L" = "light blue")) +
    theme_minimal() +
    theme(axis.title.y.right = element_text(color = "red")) +
    scale_y_continuous(sec.axis = sec_axis(~scale(.) * sd(data[[var]]) + mean(data[[var]]), name = paste(var, "Value")))
})

# Arrange the plots in a grid
multiplot <- do.call(gridExtra::grid.arrange, plots)

# Display the multiplot
multiplot
# Each individual plot can be accesed through indexes 
# for example plot_variable3 <- multiplot[["variable3"]]
starch_plot <- multiplot[[2]]

# Now you can use the starch_plot as needed
print(starch_plot)


hist(data$nsc + data$starch)


