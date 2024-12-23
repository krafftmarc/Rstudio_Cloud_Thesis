# startup.R

# Source scripts and run analysis
source("validation_utils.R")
log_file <- initialize_logging()
source("7_main_analysis.R")
results <- run_analysis(default_paths)

# Verify exports worked
list.files("output_tables")
list.files("output_figures")

