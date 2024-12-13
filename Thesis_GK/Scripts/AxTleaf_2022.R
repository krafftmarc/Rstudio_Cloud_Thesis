library(dplyr)
library(ggplot2)
library(broom)
library(readxl)
library(purrr)  # Required for the map functions
library(tidyr)  # Required for unnest function

# Load the data
data <- readxl::read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022/Tyree LiCor Data 2022/Master_plan/licor_comb_2022.xlsx")

# Filter data for observations at 1 PM
data_1pm <- data %>%
  filter(Time_id == "1PM")

# Fit linear models by Variety and Tx, and compute summaries
results <- data_1pm %>%
  group_by(Variety, Tx) %>%
  summarize(
    model = list(lm(A ~ TleafEB, data = .)),
    .groups = 'drop'
  ) %>%
  mutate(
    tidied = map(model, tidy),
    glanced = map(model, glance),
    r_squared = map_dbl(glanced, ~ .x$r.squared),
    p_value = map_dbl(glanced, ~ .x$p.value)
  ) %>%
  unnest(tidied) %>%
  mutate(label = paste("R^2 = ", round(r_squared, 2), ", p = ", format.pval(p_value, digits = 3), sep = ""))

# Plotting
p <- ggplot(data_1pm, aes(x = TleafEB, y = A, color = Variety)) +
  geom_point(alpha = 0.6) +  # Add points
  geom_smooth(method = "lm", se = FALSE, aes(linetype = Tx)) +  # Add regression lines
  geom_text(data = results, aes(label = label, x = Inf, y = Inf), hjust = 1.1, vjust = 1, check_overlap = TRUE) +
  facet_wrap(~ Variety + Tx, scales = "free_x") +  # Create panels
  labs(
    title = "Relationship between Photosynthetic Rate (A) and Leaf Temperature (TleafEB) at 1PM",
    x = "Leaf Temperature (TleafEB)",
    y = "Photosynthetic Rate (A)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)
