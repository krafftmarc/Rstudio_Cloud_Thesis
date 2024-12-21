# 2_data_loading.R
# Functions for loading and validating data

# Default file paths
default_paths <- list(
  licor_2022 = licor_comb_2022_final,
  licor_2023 = X2023_comb_Licor,
  cimis_2022 = CIMIS_growing_season_2022,
  cimis_2023 = CIMIS_2023
)

# Function to load all required data files
load_data <- function(paths = default_paths) {
  # Validate file existence
  lapply(paths, function(path) {
    if (!file.exists(path)) {
      stop(paste("File not found:", path))
    }
  })
  
  # Create empty list to store data
  loaded_data <- list()
  
  # Try-catch block for each file
  tryCatch({
    # Load 2022 LICOR data
    message("Loading 2022 LICOR data...")
    loaded_data$licor_2022 <- read.csv(paths$licor_2022)
    
    # Load 2023 LICOR data
    message("Loading 2023 LICOR data...")
    loaded_data$licor_2023 <- read_excel(paths$licor_2023)
    
    # Load CIMIS data
    message("Loading CIMIS data...")
    loaded_data$cimis_2022 <- read.csv(paths$cimis_2022)
    loaded_data$cimis_2023 <- read.csv(paths$cimis_2023)
    
    validate_data_columns(loaded_data)
    print_data_summary(loaded_data)
    
  }, error = function(e) {
    stop(paste("Error loading data:", e$message))
  })
  
  return(loaded_data)
}

# Function to validate data columns
validate_data_columns <- function(data) {
  # Required columns
  required_licor_cols <- c("date", "Tx", "A", "E", "gsw", "VPDleaf", "Ci")
  required_cimis_cols <- c("Date", "Avg.Vap.Pres..mBars.", "Max.Air.Temp..F.")
  
  # Check LICOR data
  check_missing_columns(data$licor_2022, required_licor_cols, "2022 LICOR")
  check_missing_columns(data$licor_2023, required_licor_cols, "2023 LICOR")
  
  # Check CIMIS data
  check_missing_columns(data$cimis_2022, required_cimis_cols, "2022 CIMIS")
  check_missing_columns(data$cimis_2023, required_cimis_cols, "2023 CIMIS")
}

# Helper function to check missing columns
check_missing_columns <- function(data, required_cols, dataset_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning(paste("Missing columns in", dataset_name, "data:", 
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