# run_analysis.R
# Main script to execute the irrigation treatment analysis

# Install and load required packages
source("1_packages_setup.R")

# Initialize logging
log_file <- file.path("logs", format(Sys.time(), "analysis_log_%Y%m%d_%H%M%S.txt"))
dir.create("logs", showWarnings = FALSE)

# Function to log messages with timestamps
log_message <- function(msg, print_console = TRUE) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  message <- paste(timestamp, msg)
  write(message, log_file, append = TRUE)
  if (print_console) cat(message, "\n")
}

# Function to check R environment
check_environment <- function() {
  log_message("Checking R environment...")
  
  # Check R version
  r_version <- getRversion()
  log_message(paste("R version:", r_version))
  
  # Create results directories
  dirs <- c("output_tables", "output_figures")
  sapply(dirs, dir.create, showWarnings = FALSE)
}

# Function to source required scripts
source_scripts <- function() {
  required_scripts <- c(
    "1_packages_setup.R",
    "validation_utils.R",  # Moved earlier in sequence
    "2_data_loading.R",
    "3_data_processing.R",
    "4_statistical_analysis.R",
    "5_visualization.R",
    "6_export_functions.R",
    "7_main_analysis.R"    # Main analysis should be last
  )
  
  log_message("Sourcing required scripts...")
  
  for (script in required_scripts) {
    if (file.exists(script)) {
      tryCatch({
        source(script)
        log_message(paste("Successfully loaded:", script))
      }, error = function(e) {
        log_message(paste("ERROR loading", script, ":", e$message))
        stop(paste("Failed to load", script))
      })
    } else {
      log_message(paste("ERROR:", script, "not found"))
      stop(paste("Missing required script:", script))
    }
  }
}

# Function to validate file paths
validate_paths <- function(paths) {
  log_message("Validating pre-loaded dataframes...")
  
  missing_data <- sapply(names(paths), function(name) {
    if (!exists(paths[[name]], envir = .GlobalEnv)) {
      log_message(paste("ERROR: Required dataframe", paths[[name]], "is missing in the environment."))
      return(TRUE)
    }
    return(FALSE)
  })
  
  if (any(missing_data)) {
    stop("One or more required dataframes are missing. Check log for details.")
  }
  
  log_message("All required dataframes are present.")
}

# Main function to run analysis
run_analysis <- function(paths = default_paths) {
  results <- tryCatch({
    # Initialize environment
    check_environment()
    
    # Source all required scripts
    source_scripts()
    
    # Validate paths before running analysis
    validate_paths(paths)
    
    # Run the analysis
    log_message("Starting analysis...")
    results <- main_analysis(paths)
    
    # Print success message
    log_message("\nAnalysis completed successfully!")
    log_message("Results have been exported to:")
    log_message("  - output_tables/")
    log_message("  - output_figures/")
    
    # Save results object
    save(results, file = "analysis_results.RData")
    log_message("Results object saved to: analysis_results.RData")
    
    # Return results
    return(results)
    
  }, error = function(e) {
    log_message(paste("ERROR in analysis:", e$message))
    log_message("Analysis failed. Check log file for details.")
    stop(e)
  }, warning = function(w) {
    log_message(paste("WARNING:", w$message))
  })
  
  return(results)
}

# Define paths using the actual dataframe names in the environment
default_paths <- list(
  licor_2022 = "licor_comb_2022_final",
  licor_2023 = "X2023_comb_Licor",
  cimis_2022 = "CIMIS_growing_season_2022",
  cimis_2023 = "CIMIS_2023"
)

# If this script is being sourced, don't auto-execute
if (!exists("IS_SOURCED")) {
  IS_SOURCED <- TRUE
}