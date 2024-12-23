# startup.R

# Source validation utilities first
source("validation_utils.R")

# Initialize logging
log_file <- initialize_logging()

# Source main analysis file separately
source("7_main_analysis.R")


# Run the analysis
results <- run_analysis(default_paths)

