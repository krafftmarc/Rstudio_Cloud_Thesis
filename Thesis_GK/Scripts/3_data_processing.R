# 3_data_processing.R

#------------------------------------------------------------------------------
# Part 1: LICOR Data Processing Functions
#------------------------------------------------------------------------------

# Helper function to validate time columns with improved checks
validate_time_columns <- function(data, year) {
  hhmmss_cols <- grep("hhmmss", names(data), value = TRUE)
  
  if (length(hhmmss_cols) == 0) {
    stop(paste("No hhmmss columns found in", year, "data"))
  }
  
  valid_cols <- sapply(hhmmss_cols, function(col) {
    !all(is.na(data[[col]])) && 
      any(grepl("^(05|13)[0-9]{2}", data[[col]]))
  })
  
  valid_hhmmss_cols <- hhmmss_cols[valid_cols]
  
  if (length(valid_hhmmss_cols) == 0) {
    stop(paste("No valid hhmmss columns found in", year, "data"))
  }
  
  if (length(valid_hhmmss_cols) > 1) {
    warning(paste("Multiple valid hhmmss columns found in", year, "data:", 
                  paste(valid_hhmmss_cols, collapse=", "), 
                  "\nWill use the first column:", valid_hhmmss_cols[1]))
  }
  
  return(valid_hhmmss_cols[1])
}

# Function to clean and standardize time values
clean_time_values <- function(time_values, year) {
  cleaned <- sapply(time_values, function(t) {
    if (is.na(t)) return(NA)
    
    t <- as.character(t)
    
    if (grepl(":", t)) {
      return(t)
    } else if (nchar(t) == 6) {
      return(sprintf("%02d:%02d:%02d", 
                     as.numeric(substr(t, 1, 2)),
                     as.numeric(substr(t, 3, 4)),
                     as.numeric(substr(t, 5, 6))))
    } else {
      t_num <- as.numeric(t)
      if (!is.na(t_num)) {
        return(sprintf("%06d", t_num))
      }
    }
    return(NA)
  })
  
  return(cleaned)
}

# Helper function to clean time columns
clean_time_columns <- function(data, time_col) {
  hhmmss_cols <- grep("hhmmss", names(data), value = TRUE)
  
  if (length(hhmmss_cols) > 1) {
    data <- data %>%
      select(-one_of(setdiff(hhmmss_cols, time_col))) %>%
      rename(hhmmss = !!time_col)
  } else if (length(hhmmss_cols) == 1) {
    data <- data %>% rename(hhmmss = !!hhmmss_cols[1])
  }
  
  return(data)
}

# Function to define and validate critical LICOR columns
get_licor_columns <- function(data, year) {
  # Define expected column patterns and their standardized names
  column_patterns <- list(
    date = "^date$",
    treatment = "^Tx$",
    photosynthesis = "^A$",
    transpiration = "^E$",
    conductance = "^gsw$",
    vpd = "^VPDleaf$",
    co2 = "^Ci$",
    stress = "^Stress$",
    time = "^Time_(id|ID)$",
    block = "(?i)^Block$",     # Added (?i) for case-insensitive matching
    row = "(?i)^Row$",         # Added (?i) for case-insensitive matching
    vine = "(?i)^Vine$",       # Added (?i) for case-insensitive matching
    vine_id = "(?i)^Vine_id$", # Added (?i) for case-insensitive matching
    variety = "(?i)^(Var|Variety)$", # Added pattern to match both Var and Variety
    leaf_temp = "^TleafEB$"
  )
  
  # Initialize list to store matched columns
  matched_columns <- list()
  
  # Find matching columns and validate
  for (var_name in names(column_patterns)) {
    matching_cols <- grep(column_patterns[[var_name]], names(data), value = TRUE)
    
    if (length(matching_cols) == 0) {
      if (var_name %in% c("date", "treatment", "photosynthesis", "transpiration", 
                          "conductance", "vpd", "co2", "variety")) {
        stop(sprintf("Critical column matching pattern '%s' not found in %d data", 
                     column_patterns[[var_name]], year))
      }
      message(sprintf("Optional column matching pattern '%s' not found in %d data", 
                      column_patterns[[var_name]], year))
      next
    }
    
    if (length(matching_cols) > 1) {
      warning(sprintf("Multiple columns match pattern '%s' in %d data: %s\nUsing first match: %s", 
                      column_patterns[[var_name]], year,
                      paste(matching_cols, collapse = ", "),
                      matching_cols[1]))
    }
    
    matched_columns[[var_name]] <- matching_cols[1]
  }
  
  # Return named vector of column mappings
  return(setNames(unlist(matched_columns), names(matched_columns)))
}

# Function to filter data based on physiological bounds
filter_physiological_bounds <- function(data) {
  bounds <- list(
    photosynthesis = c(-5, 30),
    transpiration = c(0, 0.02),
    conductance = c(0, 1),
    vpd = c(0, 10),
    leaf_temp = c(0, 50)
  )
  
  filtered_data <- data %>%
    filter(
      between(photosynthesis, bounds$photosynthesis[1], bounds$photosynthesis[2]) | 
        is.na(photosynthesis),
      between(transpiration, bounds$transpiration[1], bounds$transpiration[2]) | 
        is.na(transpiration),
      between(conductance, bounds$conductance[1], bounds$conductance[2]) | 
        is.na(conductance),
      between(vpd, bounds$vpd[1], bounds$vpd[2]) | 
        is.na(vpd),
      WUEi > 0 | is.na(WUEi)
    )
  
  rows_filtered <- nrow(data) - nrow(filtered_data)
  if (rows_filtered > 0) {
    message(sprintf("Filtered out %d rows with values outside physiological bounds", 
                    rows_filtered))
  }
  
  return(filtered_data)
}

# Helper function to validate processed data
validate_processed_data <- function(data) {
  # Check if data exists and is a data frame
  if (!is.data.frame(data)) {
    stop("Processed data must be a data frame")
  }
  
  # Check if required columns exist
  required_cols <- c("date", "year", "treatment", "stress_level", "measurement_period", 
                     "photosynthesis", "transpiration", "conductance", "vpd", "co2")
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in processed data:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Validate treatment values
  valid_treatments <- c("4L (Pre-treatment)", "2L", "4L")
  invalid_treatments <- setdiff(unique(data$treatment), valid_treatments)
  if (length(invalid_treatments) > 0) {
    stop(paste("Invalid treatment values found:", 
               paste(invalid_treatments, collapse = ", ")))
  }
  
  # Validate stress levels
  valid_stress <- c("Stressed", "Not Stressed", "Normal Schedule")
  invalid_stress <- setdiff(unique(data$stress_level), valid_stress)
  if (length(invalid_stress) > 0) {
    stop(paste("Invalid stress level values found:", 
               paste(invalid_stress, collapse = ", ")))
  }
  
  # Validate measurement periods
  valid_periods <- c("Pre-dawn", "Midday", "Other")
  invalid_periods <- setdiff(unique(data$measurement_period), valid_periods)
  if (length(invalid_periods) > 0) {
    stop(paste("Invalid measurement period values found:", 
               paste(invalid_periods, collapse = ", ")))
  }
  
  # Check date range
  year_range <- range(data$year)
  if (any(year_range < 2022) || any(year_range > 2023)) {
    stop(paste("Invalid year range:", 
               paste(year_range, collapse = " to "), 
               ". Expected 2022-2023"))
  }
  
  # Print validation summary
  message("\nData validation summary:")
  message("------------------------")
  message(sprintf("Total rows: %d", nrow(data)))
  message(sprintf("Date range: %s to %s", 
                  min(data$date), max(data$date)))
  message("Treatment counts:")
  print(table(data$treatment))
  message("\nStress level counts:")
  print(table(data$stress_level))
  message("\nMeasurement period counts:")
  print(table(data$measurement_period))
  
  return(TRUE)
}

# Main function to process LICOR data
process_licor_data <- function(data, year) {
  # Get validated column mappings
  cols <- get_licor_columns(data, year)
  
  # Print column mapping for debugging
  message("\nColumn mapping for year ", year, ":")
  print(cols)
  
  # Select only the matched columns
  filtered_data <- data[, cols, drop = FALSE]
  
  # Standardize column names
  names(filtered_data) <- names(cols)
  
  # Process data with standardized column names
  processed_data <- filtered_data %>%
    mutate(
      treatment = case_when(
        year == 2022 & treatment == "Base" ~ "4L (Pre-treatment)",
        year == 2022 & treatment == "4" ~ "4L",
        year == 2022 & treatment == "2" ~ "2L",
        year == 2023 & treatment == "4L" ~ "4L",
        year == 2023 & treatment == "2L" ~ "2L",
        TRUE ~ NA_character_
      ),
      # Add variety standardization
      variety = case_when(
        toupper(variety) %in% c("CH", "CHARDONNAY") ~ "CH",
        toupper(variety) %in% c("CS", "CAB", "CABERNET", "CABERNET SAUVIGNON") ~ "CS",
        TRUE ~ NA_character_
      ),
      # Convert date if needed
      date = if(year == 2022) {
        as.Date(date, format = "%m/%d/%y")
      } else {
        as.Date(substr(date, 1, 8), format = "%Y%m%d")
      },
      
      # Ensure consistent data types for block, row, vine
      block = as.character(block),  # Convert to character
      row = as.character(row),      # Convert to character
      vine = as.character(vine),    # Convert to character
      
      year = year,
      
      # Calculate WUEi (Intrinsic Water Use Efficiency)
      WUEi = photosynthesis / conductance,
      
      # Standardize measurement periods
      measurement_period = case_when(
        toupper(time) %in% c("PREDAWN", "PRE-DAWN", "PRE_DAWN") ~ "Pre-dawn",
        toupper(time) %in% c("MIDDAY", "MID-DAY", "MID_DAY") ~ "Midday",
        TRUE ~ "Other"
      ),
      
      # Standardize stress levels
      stress_level = case_when(
        tolower(stress) == "s" ~ "Stressed",
        tolower(stress) == "ns" ~ "Not Stressed",
        TRUE ~ "Normal Schedule"
      )
    ) %>%
    filter(!is.na(treatment)) %>%
    filter_physiological_bounds()
  
  # Add diagnostic message for processed data
  message("\nProcessing summary for year ", year, ":")
  message("Treatment counts:")
  print(table(processed_data$treatment))
  message("\nVariety counts:")
  print(table(processed_data$variety))
  message("\nStress level counts:")
  print(table(processed_data$stress_level))
  message("\nMeasurement period counts:")
  print(table(processed_data$measurement_period))
  message("\nTotal rows: ", nrow(processed_data))
  
  # Validate final processed data
  validate_processed_data(processed_data)
  
  return(processed_data)
}

#------------------------------------------------------------------------------
# Part 2: CIMIS Data Processing Functions
#------------------------------------------------------------------------------

# Function to process CIMIS data
process_cimis_data <- function(data, year) {
  # Print column names at the start of processing
  log_message("CIMIS data columns:")
  log_message(paste(names(data), collapse=", "))
  
  required_cols <- c("Date", "ETo (in)", "Precip (in)", "Sol Rad (Ly/day)", 
                     "Avg Vap Pres (mBars)", "Max Air Temp (F)", "Min Air Temp (F)",
                     "Avg Air Temp (F)", "Avg Rel Hum (%)", "Avg Wind Speed (mph)")
  
  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required CIMIS columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Process data with error handling
  processed_data <- tryCatch({
    data %>%
      # Select and rename columns for consistency
      dplyr::select(dplyr::all_of(required_cols)) %>%
      dplyr::rename(
        date = "Date",
        ETo = "ETo (in)",
        precip = "Precip (in)",
        solar_rad = "Sol Rad (Ly/day)",
        VPD = "Avg Vap Pres (mBars)",
        tmax = "Max Air Temp (F)",
        tmin = "Min Air Temp (F)",
        tmean = "Avg Air Temp (F)",
        RH = "Avg Rel Hum (%)",
        wind = "Avg Wind Speed (mph)"
      ) %>%
      # Convert date string to Date type and add conversions
      dplyr::mutate(
        # Convert date to proper Date type
        date = as.Date(date, format = "%m/%d/%Y"),
        year = year,
        # Convert temperatures from F to C
        tmax = (tmax - 32) * 5/9,
        tmin = (tmin - 32) * 5/9,
        tmean = (tmean - 32) * 5/9,
        # Convert vapor pressure from mBars to kPa
        VPD = VPD / 10,
        # Convert ETo and precip from inches to mm
        ETo = ETo * 25.4,
        precip = precip * 25.4
      ) %>%
      # Arrange by date
      dplyr::arrange(date)
    
  }, error = function(e) {
    stop(paste("Error processing CIMIS data for year", year, ":", e$message))
  })
  
  # Add diagnostic output for date type
  log_message(paste("Date column class after processing:", class(processed_data$date)[1]))
  
  # Validate processed data
  validate_cimis_data(processed_data)
  
  # Print summary statistics
  message("\nCIMIS data summary for year ", year, ":")
  message("----------------------------------------")
  message("Date range: ", min(processed_data$date), " to ", max(processed_data$date))
  message("Number of days: ", nrow(processed_data))
  message("\nTemperature range (°C):")
  message("  Maximum: ", round(range(processed_data$tmax, na.rm = TRUE), 1)[1], 
          " to ", round(range(processed_data$tmax, na.rm = TRUE), 1)[2])
  message("  Minimum: ", round(range(processed_data$tmin, na.rm = TRUE), 1)[1],
          " to ", round(range(processed_data$tmin, na.rm = TRUE), 1)[2])
  message("\nTotal precipitation (mm): ", round(sum(processed_data$precip, na.rm = TRUE), 1))
  message("Total ETo (mm): ", round(sum(processed_data$ETo, na.rm = TRUE), 1))
  message("Mean VPD (kPa): ", round(mean(processed_data$VPD, na.rm = TRUE), 2))
  message("----------------------------------------")
  
  return(processed_data)
}

# Helper function to validate CIMIS data
validate_cimis_data <- function(data) {
  # Check data structure
  if (!is.data.frame(data)) {
    stop("CIMIS data must be a data frame")
  }
  
  # Check required columns exist
  required_cols <- c("date", "year", "ETo", "precip", "solar_rad", "VPD",
                     "tmax", "tmin", "tmean", "RH", "wind")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in processed CIMIS data:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Validate value ranges
  if (any(data$tmax < -10 | data$tmax > 50, na.rm = TRUE)) {
    warning("Maximum temperatures outside expected range (-10°C to 50°C)")
  }
  if (any(data$tmin < -10 | data$tmin > 40, na.rm = TRUE)) {
    warning("Minimum temperatures outside expected range (-10°C to 40°C)")
  }
  if (any(data$RH < 0 | data$RH > 100, na.rm = TRUE)) {
    warning("Relative humidity values outside valid range (0-100%)")
  }
  if (any(data$VPD < 0, na.rm = TRUE)) {
    warning("Negative vapor pressure deficit values found")
  }
  if (any(data$solar_rad < 0, na.rm = TRUE)) {
    warning("Negative solar radiation values found")
  }
  
  return(TRUE)
}