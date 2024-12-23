# 1_packages_setup.R
# Define CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of required packages with descriptions
required_packages <- c(
  "tidyverse",    # includes dplyr, ggplot2, tidyr, etc.
  "car",          # for Anova() function
  "emmeans",      # for estimated marginal means
  "patchwork",    # for combining plots
  "lubridate",    # for date handling
  "magrittr"      # for pipe operator
)

# Function to check if packages are available
check_packages <- function(packages) {
  missing_pkgs <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  return(missing_pkgs)
}

# Check which packages need to be installed
missing_packages <- check_packages(required_packages)
if(length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all required packages
for(pkg in required_packages) {
  library(pkg, character.only = TRUE)
}