# 3_data_processing.R
# Functions for processing and cleaning data

# Function to process LICOR data
process_licor_data <- function(data, year) {
  processed_data <- data %>%
    mutate(
      date = as.Date(date),
      year = year,
      treatment = case_when(
        Tx == "1" ~ "Basic (1x4L)",
        Tx == "2" ~ "Double 2L",
        Tx == "3" ~ "Double 4L",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(treatment)) %>%
    filter_physiological_bounds()
  
  return(processed_data)
}

# Function to filter physiologically impossible values
filter_physiological_bounds <- function(data) {
  data %>%
    filter(
      A > -5 & A < 30,    # Net photosynthesis bounds
      E > 0 & E < 0.02,   # Transpiration bounds
      gsw > 0 & gsw < 1   # Stomatal conductance bounds
    )
}

# Function to process CIMIS data
process_cimis_data <- function(data, year) {
  processed_data <- data %>%
    mutate(
      Date = as.Date(Date),
      year = year,
      VPD_kPa = (Avg.Vap.Pres..mBars. / 10)  # Convert to kPa
    )
  return(processed_data)
}

# Function to check data quality
check_data_quality <- function(data) {
  # Check for missing values
  missing_summary <- lapply(data, function(df) {
    colSums(is.na(df))
  })
  
  # Define bounds for LICOR data
  licor_bounds <- list(
    A = c(-5, 30),     # Net photosynthesis bounds
    E = c(0, 0.02),    # Transpiration bounds
    gsw = c(0, 1),     # Stomatal conductance bounds
    VPDleaf = c(0, 10) # VPD bounds
  )
  
  # Check for outliers in LICOR data
  licor_outliers <- check_licor_outliers(data, licor_bounds)
  
  return(list(
    missing_values = missing_summary,
    outliers = licor_outliers
  ))
}

# Helper function to check LICOR outliers
check_licor_outliers <- function(data, bounds) {
  lapply(c("licor_2022", "licor_2023"), function(year) {
    df <- data[[year]]
    lapply(names(bounds), function(var) {
      if (var %in% names(df)) {
        out_of_bounds <- df[[var]] < bounds[[var]][1] | 
          df[[var]] > bounds[[var]][2]
        sum(out_of_bounds, na.rm = TRUE)
      }
    })
  })
}