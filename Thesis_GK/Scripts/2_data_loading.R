# 2_data_loading.R
# Function to load all required data files (use pre-loaded dataframes)
load_data <- function(paths = default_paths) {
  message("Using pre-loaded dataframes...")
  
  # Validate that data exists in the environment
  missing_data <- sapply(paths, function(df_name) !exists(df_name, envir = .GlobalEnv))
  
  if (any(missing_data)) {
    stop(paste("Missing required dataframes in environment:", 
               paste(names(paths)[missing_data], collapse = ", ")))
  }
  
  # Create a list to hold the data by referencing existing dataframes
  loaded_data <- list(
    licor_2022 = get(paths$licor_2022),
    licor_2023 = get(paths$licor_2023),
    cimis_2022 = get(paths$cimis_2022),
    cimis_2023 = get(paths$cimis_2023)
  )
  
  # Perform validation and summarization
  validate_data_columns(loaded_data)
  print_data_summary(loaded_data)
  
  return(loaded_data)
}

# Function to validate data columns
validate_data_columns <- function(data) {
  # Required columns
  required_licor_cols <- c("date", "Tx", "A", "E", "gsw", "VPDleaf", "Ci")
  required_cimis_cols <- c("Date", "Avg Vap Pres (mBars)", "Max Air Temp (F)")  # Updated to match actual column names
  
  # Check LICOR data
  stop_on_missing_columns(data$licor_2022, required_licor_cols, "2022 LICOR")
  stop_on_missing_columns(data$licor_2023, required_licor_cols, "2023 LICOR")
  
  # Check CIMIS data
  stop_on_missing_columns(data$cimis_2022, required_cimis_cols, "2022 CIMIS")
  stop_on_missing_columns(data$cimis_2023, required_cimis_cols, "2023 CIMIS")
}

# Helper function to check missing columns (critical stop on error)
stop_on_missing_columns <- function(data, required_cols, dataset_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Critical error: Missing columns in", dataset_name, "data:",
               paste(missing_cols, collapse = ", ")))
  }
}

# Function to print data summary
print_data_summary <- function(data) {
  message("\nData loading summary:")
  message(paste("2022 LICOR rows:", nrow(data$licor_2022)))
  message(paste("2023 LICOR rows:", nrow(data$licor_2023)))
  message(paste("2022 CIMIS rows:", nrow(data$cimis_2022)))
  message(paste("2023 CIMIS rows:", nrow(data$cimis_2023)))
}
