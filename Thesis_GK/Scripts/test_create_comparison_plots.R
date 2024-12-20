# test_create_comparison_plots.R

# Load functions
source("/cloud/project/Thesis_GK/Scripts/functions.R")

# Create test data
test_combined_data <- tibble(
  Date = as.Date('2022-04-01') + 0:5,
  Season = factor(rep("2022", 6)),
  Variety = rep("CH", 6),
  pot_type = rep("Stem", 6),
  PSI = runif(6, -1.5, -0.5),
  Time_of_Day = rep(c("Pre-dawn", "Midday"), 3),
  Tx = rep(c("Baseline", "2L", "4L"), 2)
)

test_cimis_data <- tibble(
  Date = as.Date('2022-04-01') + 0:5,
  Temp_C = runif(6, 10, 25),
  RH = runif(6, 30, 70),
  VPD = runif(6, 0.5, 2.0)
)

# Test the function
plots <- create_comparison_plots(test_combined_data, test_cimis_data)

# Print one plot to verify
print(plots[[1]])
