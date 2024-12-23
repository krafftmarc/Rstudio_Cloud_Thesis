# validation_utils.R 

#------------------------------------------------------------------------------
# Logging Functions
#------------------------------------------------------------------------------

# Function to initialize logging
initialize_logging <- function() {
  log_file_path <- file.path("logs", format(Sys.time(), "analysis_log_%Y%m%d_%H%M%S.txt"))
  dir.create("logs", showWarnings = FALSE)
  assign("log_file", log_file_path, envir = .GlobalEnv)  
  return(log_file_path)
}

# Function to log messages with timestamps
log_message <- function(msg, print_console = TRUE) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  message <- paste(timestamp, msg)
  write(message, log_file, append = TRUE)
  if (print_console) cat(message, "\n")
}

#------------------------------------------------------------------------------
# Validation Functions
#------------------------------------------------------------------------------

# Function to validate paths and data requirements
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

# Function to validate column names
validate_columns <- function(data, required_cols, dataset_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in", dataset_name, ":",
               paste(missing_cols, collapse = ", ")))
  }
  return(TRUE)
}

# Function to validate data types
validate_data_types <- function(data, expected_types) {
  type_mismatches <- lapply(names(expected_types), function(col) {
    if (!is(data[[col]], expected_types[[col]])) {
      return(paste("Column", col, "should be", expected_types[[col]],
                   "but is", class(data[[col]])[1]))
    }
    return(NULL)
  })
  
  # Remove NULL entries
  type_mismatches <- type_mismatches[!sapply(type_mismatches, is.null)]
  
  if (length(type_mismatches) > 0) {
    stop(paste("Data type mismatches found:",
               paste(unlist(type_mismatches), collapse = "; ")))
  }
  return(TRUE)
}

# Function to validate value ranges
validate_ranges <- function(data, range_specs) {
  range_violations <- lapply(names(range_specs), function(col) {
    range <- range_specs[[col]]
    values <- data[[col]]
    if (any(values < range[1] | values > range[2], na.rm = TRUE)) {
      return(paste("Column", col, "has values outside allowed range",
                   paste(range, collapse = " to ")))
    }
    return(NULL)
  })
  
  # Remove NULL entries
  range_violations <- range_violations[!sapply(range_violations, is.null)]
  
  if (length(range_violations) > 0) {
    warning(paste("Range violations found:",
                  paste(unlist(range_violations), collapse = "; ")))
  }
  return(TRUE)
}


# [existing validation functions remain the same...]

# Function to check data quality across all datasets
check_data_quality <- function(data) {
  log_message("Checking data quality...")
  
  # LICOR data quality checks
  validate_ranges(data$licor_2022, list(
    A = c(-50, 50),        # photosynthesis range
    gsw = c(-1, 2),        # stomatal conductance range
    VPDleaf = c(0, 10)     # vapor pressure deficit range
  ))
  validate_ranges(data$licor_2023, list(
    A = c(-50, 50),        # photosynthesis range
    gsw = c(-1, 2),        # stomatal conductance range - made consistent with 2022
    VPDleaf = c(0, 10)     # vapor pressure deficit range
  ))
  
  # CIMIS data quality checks
  validate_ranges(data$cimis_2022, list(
    "Max Air Temp (F)" = c(0, 130),
    "Avg Vap Pres (mBars)" = c(0, 100)
  ))
  validate_ranges(data$cimis_2023, list(
    "Max Air Temp (F)" = c(0, 130),
    "Avg Vap Pres (mBars)" = c(0, 100)
  ))
  
  log_message("Data quality checks completed")
  return(TRUE)
}

#------------------------------------------------------------------------------
# Environment Setup Functions
#------------------------------------------------------------------------------

# Function to check R environment and setup directories
check_environment <- function() {
  log_message("Checking R environment...")
  
  # Check R version
  r_version <- getRversion()
  log_message(paste("R version:", r_version))
  
  # Create results directories
  dirs <- c("output_tables", "output_figures", "logs")
  sapply(dirs, dir.create, showWarnings = FALSE)
}

# Source scripts in the correct order, avoiding validation_utils.R
source_scripts <- function() {
  required_scripts <- c(
    "1_packages_setup.R",
    "2_data_loading.R",
    "3_data_processing.R",
    "4_statistical_analysis.R",
    "5_visualization.R",
    "6_export_functions.R"
  )  # Note: main_analysis.R is not included here
  
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
#------------------------------------------------------------------------------
# Analysis Execution Functions
#------------------------------------------------------------------------------

# Function to initialize and run analysis
run_analysis <- function(paths = default_paths) {
  tryCatch({
    # Initialize environment
    check_environment()
    
    # Source required scripts before running analysis
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
    
    return(results)
    
  }, error = function(e) {
    log_message(paste("ERROR in analysis:", e$message))
    log_message("Analysis failed. Check log file for details.")
    stop(e)
  }, warning = function(w) {
    log_message(paste("WARNING:", w$message))
  })
}