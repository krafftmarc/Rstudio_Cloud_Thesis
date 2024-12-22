# 3_data_processing.R
# Functions for processing and cleaning data

# Helper function to validate time columns with improved checks
validate_time_columns <- function(data, year) {
  hhmmss_cols <- grep("hhmmss", names(data), value = TRUE)
  
  if (length(hhmmss_cols) == 0) {
    stop(paste("No hhmmss columns found in", year, "data"))
  }
  
  # Check for empty or invalid time columns
  valid_cols <- sapply(hhmmss_cols, function(col) {
    !all(is.na(data[[col]])) && 
      any(grepl("^(05|13)[0-9]{2}", data[[col]]))  # Check for expected time patterns
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
    
    # Handle different formats
    if (grepl(":", t)) {
      # Already in HH:MM:SS format
      return(t)
    } else if (nchar(t) == 6) {
      # Convert HHMMSS to HH:MM:SS
      return(sprintf("%02d:%02d:%02d", 
                     as.numeric(substr(t, 1, 2)),
                     as.numeric(substr(t, 3, 4)),
                     as.numeric(substr(t, 5, 6))))
    } else {
      # Try to handle other numeric formats
      t_num <- as.numeric(t)
      if (!is.na(t_num)) {
        # Convert to HHMMSS format
        return(sprintf("%06d", t_num))
      }
    }
    return(NA)
  })
  
  return(cleaned)
}

# Helper function to clean time columns
clean_time_columns <- function(data, time_col) {
  # Get all hhmmss columns
  hhmmss_cols <- grep("hhmmss", names(data), value = TRUE)
  
  # If we have multiple columns, keep only the validated one
  if (length(hhmmss_cols) > 1) {
    data <- data %>%
      select(-one_of(setdiff(hhmmss_cols, time_col))) %>%
      rename(hhmmss = !!time_col)
  } else if (length(hhmmss_cols) == 1) {
    data <- data %>% rename(hhmmss = !!hhmmss_cols[1])
  }
  
  return(data)
}

# Filter for physiological bounds including WUEi
filter_physiological_bounds <- function(data) {
  bounds <- list(
    A = c(-5, 30),     # Photosynthesis
    E = c(0, 0.02),    # Transpiration
    gsw = c(0, 1),     # Stomatal conductance
    VPDleaf = c(0, 10) # Vapor pressure deficit
  )
  
  filtered_data <- data %>%
    filter(
      between(A, bounds$A[1], bounds$A[2]) | is.na(A),
      between(E, bounds$E[1], bounds$E[2]) | is.na(E),
      between(gsw, bounds$gsw[1], bounds$gsw[2]) | is.na(gsw),
      between(VPDleaf, bounds$VPDleaf[1], bounds$VPDleaf[2]) | is.na(VPDleaf),
      WUEi > 0 | is.na(WUEi)  # Ensure WUEi values are positive
    )
  
  rows_filtered <- nrow(data) - nrow(filtered_data)
  if (rows_filtered > 0) {
    message(paste("Filtered out", rows_filtered, "rows with values outside physiological bounds"))
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
                     "A", "E", "gsw", "VPDleaf", "Ci")
  
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
  
  # Validate numeric columns are actually numeric
  numeric_cols <- c("A", "E", "gsw", "VPDleaf", "Ci")
  for (col in numeric_cols) {
    if (!is.numeric(data[[col]])) {
      stop(paste("Column", col, "must be numeric"))
    }
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

process_licor_data <- function(data, year) {
  # Print column names to aid in extraction
  message("Column names for year ", year, ":")
  print(colnames(data))
  
  # Define columns to retain (with variations)
  columns_to_keep <- c("date", "Tx", "A", "E", "gsw", "VPDleaf", "Ci", "Stress",
                       "Block", "Row", "Vine", "Vine_id", "Time_id", "Time_ID")
  
  # Extract relevant columns (ignore others like Fv/Fm)
  selected_columns <- intersect(columns_to_keep, colnames(data))
  filtered_data <- data[, selected_columns, drop = FALSE]
  
  
  message("Columns retained for processing: ", paste(colnames(data), collapse=", "))
  
  # Identify time column (accounting for variations)
  time_col <- intersect(c("Time_id", "Time_ID"), colnames(data))[1]
  if (is.null(time_col)) {
    stop("No time_id column found for year ", year)
  }
  message("Using time column: ", time_col)
  
  # Helper function to handle different date formats
  convert_date <- function(date_str, year) {
    if(year == 2022) {
      as.Date(date_str, format = "%m/%d/%y")
    } else {
      as.Date(substr(date_str, 1, 8), format = "%Y%m%d")
    }
  }
  
  # Process data
  processed_data <- data %>%
    mutate(
      Tx = if(year == 2022) {
        if_else(is.nan(Tx), "Base", as.character(Tx))
      } else {
        as.character(Tx)
      },
      date = convert_date(date, year),
      year = year,
      
      # Calculate WUEi (Intrinsic Water Use Efficiency)
      WUEi = A / gsw,
      
      # Standardize measurement periods based on time column
      measurement_period = case_when(
        toupper(get(time_col)) %in% c("PREDAWN", "PRE-DAWN", "PRE_DAWN") ~ "Pre-dawn",
        toupper(get(time_col)) %in% c("MIDDAY", "MID-DAY", "MID_DAY") ~ "Midday",
        TRUE ~ "Heatwave"
      ),
      
      # Standardize stress levels
      stress_level = case_when(
        tolower(Stress) == "s" ~ "Stressed",
        tolower(Stress) == "ns" ~ "Not Stressed",
        TRUE ~ "Normal"
      ),
      
      # Treatment mapping logic
      treatment = case_when(
        year == 2022 & Tx == "Base" ~ "Baseline (1x4L)",
        year == 2022 & Tx == "4" ~ "Double 4L",
        year == 2022 & Tx == "2" ~ "Double 2L",
        year == 2023 & Tx == "4L" ~ "Double 4L",
        year == 2023 & Tx == "2L" ~ "Double 2L",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(treatment)) %>%
    filter_physiological_bounds()
  
  # Add diagnostic message for after processing
  message("After processing for year ", year, ":")
  message("Treatment values: ", paste(unique(processed_data$treatment), collapse=", "))
  message("Number of rows per treatment:")
  print(table(processed_data$treatment))
  message("Number of rows by stress level:")
  print(table(processed_data$stress_level))
  message("Number of rows by measurement period:")
  print(table(processed_data$measurement_period))
  message("Total rows: ", nrow(processed_data))
  
  return(processed_data)
}


# Update filter_physiological_bounds function
filter_physiological_bounds <- function(data) {
  data %>%
    filter(
      between(A, -5, 30) | is.na(A),           # Net photosynthesis bounds
      between(E, 0, 0.02) | is.na(E),          # Transpiration bounds
      between(gsw, 0, 1) | is.na(gsw),         # Stomatal conductance bounds
      (WUEi > 0 | is.na(WUEi)),                # WUEi bounds
      between(TleafEB, 0, 50) | is.na(TleafEB), # Leaf temperature bounds (°C)
      (Ci > 0 | is.na(Ci)),                    # Intercellular CO2 bounds
      (Pci > 0 | is.na(Pci))                   # Partial pressure CO2 bounds
    )
}
# Function to process CIMIS data
process_cimis_data <- function(data, year) {
  # Define required columns
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
      # Add year and convert units
      dplyr::mutate(
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