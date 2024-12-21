# 1_packages_setup.R
# Script to load required packages and set up the environment

# Install required packages if not already installed
required_packages <- c(
  "tidyverse",
  "lme4",
  "emmeans",
  "car",
  "ggpubr",
  "lubridate",
  "readxl"
)

# Function to install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# Install any missing packages
install_missing_packages(required_packages)

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Set default theme for ggplot
theme_set(theme_bw())