# validation_utils.R

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