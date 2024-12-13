library(dplyr)
library(ggplot2)
library(broom)
library(readxl)
library(purrr)  # For map functions
library(tidyr)  # For unnest function

# Load the data
data <- readxl::read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/licor_2023 copy/2023_comb_Licor.xlsx")

# Filter data for observations at 1 PM and for Tx 2L and 4L
data_filtered <- data %>%
  filter(Time_id == "1PM", Tx %in% c("2L", "4L"))

# Fit linear models by Variety and Tx, and compute summaries
results <- data_filtered %>%
  group_by(Variety, Tx) %>%
  summarize(
    model = list(tryCatch(lm(gsw ~ TleafEB, data = .), error = function(e) NULL)),
    .groups = 'drop'
  ) %>%
  filter(!is.null(model)) %>%
  mutate(
    tidied = map(model, tidy),
    glanced = map(model, glance),
    r_squared = map_dbl(glanced, ~ .x$r.squared, .default = NA),
    p_value = map_dbl(glanced, ~ .x$p.value, .default = NA)
  ) %>%
  unnest(tidied)

# Prepare labels for plotting
results <- results %>%
  mutate(label = ifelse(!is.na(p_value), 
                        paste("R^2 = ", round(r_squared, 2), ", p = ", format.pval(p_value, digits = 3), sep = ""), 
                        "Insufficient data"))

# Plotting
p <- ggplot(data_filtered, aes(x = TleafEB, y = gsw, color = Tx)) +
  geom_point(alpha = 0.6) +  # Add points
  geom_smooth(method = "loess", se = TRUE, aes(linetype = Tx), data = data_filtered %>% filter(!is.na(TleafEB) & !is.na(gsw))) +  # Add loess trend lines with standard error
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue", size = 1) +  # Horizontal line at 0.05
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red", size = 1) +  # Horizontal line at 0.15
  geom_text(data = results, aes(label = label, x = Inf, y = Inf), hjust = 1.1, vjust = 1, check_overlap = TRUE) +
  facet_wrap(~ Variety, scales = "free_x") +  # Create panels for each Variety
  labs(
    title = "Comparison of Stomatal Conductance (gsw) and Leaf Temperature (TleafEB) at 1PM between Tx 2L and 4L",
    x = "Leaf Temperature (TleafEB)",
    y = "Stomatal Conductance (gsw)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

# Print the plot
print(p)
