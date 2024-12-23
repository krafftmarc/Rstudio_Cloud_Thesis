# run_script.R

# First define logging functions
log_message <- function(msg, print_console = TRUE) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  message <- paste(timestamp, msg)
  if(print_console) cat(message, "\n")
  
  # If log file exists, write to it
  if(exists("log_file")) {
    write(message, log_file, append = TRUE)
  }
}

# Initialize logging
log_file <- file.path("logs", format(Sys.time(), "analysis_log_%Y%m%d_%H%M%S.txt"))
dir.create("logs", showWarnings = FALSE)

# Set working directory if needed
# setwd("/path/to/your/scripts")

# Source all required scripts in correct order
required_scripts <- c(
  "1_packages_setup.R",
  "2_data_loading.R",
  "3_data_processing.R",
  "4_statistical_analysis.R",
  "5_visualization.R",
  "6_export_functions.R",
  "validation_utils.R",
  "7_main_analysis.R"
)

# Source each script with error checking
log_message("Checking R environment...")
log_message(sprintf("R version: %s", getRversion()))
log_message("Sourcing required scripts...")

for(script in required_scripts) {
  tryCatch({
    source(script)
    log_message(sprintf("Successfully loaded: %s", script))
  }, error = function(e) {
    log_message(sprintf("ERROR loading %s: %s", script, e$message))
    stop(sprintf("Failed to load %s", script))
  })
}

# Create output directories
dir.create("output_tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output_figures", showWarnings = FALSE, recursive = TRUE)

# Clean output directories
unlink("output_tables/*")
unlink("output_figures/*")

# Run the analysis
log_message("Starting analysis...")
results <- main_analysis(default_paths)

# Verify outputs
log_message("\nChecking output directories:")
log_message("\nTable files:")
print(list.files("output_tables"))
log_message("\nFigure files:")
print(list.files("output_figures"))

# Save workspace
save.image("analysis_workspace.RData")
log_message("Analysis completed.")

# Return TRUE if successful
TRUE