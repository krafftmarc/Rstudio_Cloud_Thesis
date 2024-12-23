# Load necessary libraries
library(emmeans)
library(ggplot2)
library(dplyr)

# Step 1: Use preloaded data directly
conductance <- emmeans_conductance
photosynthesis <- emmeans_photosynthesis
transpiration <- emmeans_transpiration

# Step 2: Check the structure of the data
glimpse(conductance)
glimpse(photosynthesis)
glimpse(transpiration)

# Step 3: Fit a linear model for each dataset
# Using 'emmean' as the response variable
model_conductance <- lm(emmean ~ treatment * stress_level * variety, data = conductance)
model_photosynthesis <- lm(emmean ~ treatment * stress_level * variety, data = photosynthesis)
model_transpiration <- lm(emmean ~ treatment * stress_level * variety, data = transpiration)

# Step 4: Calculate EMMs for interactions
emm_conductance <- emmeans(model_conductance, ~ treatment * stress_level * variety)
emm_photosynthesis <- emmeans(model_photosynthesis, ~ treatment * stress_level * variety)
emm_transpiration <- emmeans(model_transpiration, ~ treatment * stress_level * variety)

# Step 5: Perform pairwise comparisons
pairs_conductance <- pairs(emm_conductance, adjust = "tukey")
pairs_photosynthesis <- pairs(emm_photosynthesis, adjust = "tukey")
pairs_transpiration <- pairs(emm_transpiration, adjust = "tukey")

# Print pairwise comparison results
print(pairs_conductance)
print(pairs_photosynthesis)
print(pairs_transpiration)

# Step 6: Plot the interactions
plot_conductance <- plot(emm_conductance, comparisons = TRUE) +
  ggtitle("Conductance EMMs") +
  theme_minimal()

plot_photosynthesis <- plot(emm_photosynthesis, comparisons = TRUE) +
  ggtitle("Photosynthesis EMMs") +
  theme_minimal()

plot_transpiration <- plot(emm_transpiration, comparisons = TRUE) +
  ggtitle("Transpiration EMMs") +
  theme_minimal()

# Display plots
print(plot_conductance)
print(plot_photosynthesis)
print(plot_transpiration)

# Optional: Save plots to files
ggsave("conductance_emm_plot.png", plot = plot_conductance, width = 8, height = 6)
ggsave("photosynthesis_emm_plot.png", plot = plot_photosynthesis, width = 8, height = 6)
ggsave("transpiration_emm_plot.png", plot = plot_transpiration, width = 8, height = 6)
