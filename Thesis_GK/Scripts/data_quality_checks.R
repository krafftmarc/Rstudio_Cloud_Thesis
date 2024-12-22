# Function to check data quality
check_data_quality <- function(data) {
  # Initialize list to store quality check results
  quality_results <- list()
  
  # Check LICOR data
  if (!is.null(data$licor_2022) && !is.null(data$licor_2023)) {
    quality_results$licor <- list(
      missing_values = check_missing_values(list(
        "2022" = data$licor_2022,
        "2023" = data$licor_2023
      )),
      outliers = check_outliers(list(
        "2022" = data$licor_2022,
        "2023" = data$licor_2023
      ))
    )
  }
  
  # Check CIMIS data
  if (!is.null(data$cimis_2022) && !is.null(data$cimis_2023)) {
    quality_results$cimis <- list(
      missing_values = check_missing_values(list(
        "2022" = data$cimis_2022,
        "2023" = data$cimis_2023
      ))
    )
  }
  
  # Print summary of quality checks
  print_quality_summary(quality_results)
  
  return(quality_results)
}

# Function to check missing values
check_missing_values <- function(data_list) {
  lapply(names(data_list), function(year) {
    df <- data_list[[year]]
    missing_counts <- colSums(is.na(df))
    missing_percentages <- (missing_counts / nrow(df)) * 100
    
    list(
      counts = missing_counts,
      percentages = missing_percentages
    )
  }) %>% setNames(names(data_list))
}

# Function to check outliers
check_outliers <- function(data_list) {
  # Define bounds for different variables
  bounds <- list(
    A = c(-5, 30),     # Net photosynthesis bounds
    E = c(0, 0.02),    # Transpiration bounds
    gsw = c(0, 1),     # Stomatal conductance bounds
    VPDleaf = c(0, 10) # VPD bounds
  )
  
  lapply(names(data_list), function(year) {
    df <- data_list[[year]]
    
    # Check each variable against its bounds
    outlier_counts <- lapply(names(bounds), function(var) {
      if (var %in% names(df)) {
        values <- df[[var]]
        lower_bound <- bounds[[var]][1]
        upper_bound <- bounds[[var]][2]
        
        list(
          below_bound = sum(values < lower_bound, na.rm = TRUE),
          above_bound = sum(values > upper_bound, na.rm = TRUE),
          total_outliers = sum(values < lower_bound | values > upper_bound, na.rm = TRUE)
        )
      } else {
        NULL
      }
    }) %>% setNames(names(bounds))
    
    outlier_counts
  }) %>% setNames(names(data_list))
}

# Function to print quality summary
print_quality_summary <- function(quality_results) {
  cat("\nData Quality Summary:\n")
  cat("===================\n\n")
  
  # Print LICOR data summary
  if (!is.null(quality_results$licor)) {
    cat("LICOR Data Quality:\n")
    cat("-----------------\n")
    
    # Missing values summary
    cat("\nMissing Values:\n")
    for (year in names(quality_results$licor$missing_values)) {
      cat(paste("\nYear", year, ":\n"))
      missing_data <- quality_results$licor$missing_values[[year]]
      problematic_cols <- which(missing_data$percentages > 5)
      if (length(problematic_cols) > 0) {
        cat(paste("Columns with >5% missing values:\n"))
        for (col in names(problematic_cols)) {
          cat(sprintf("  %s: %.1f%%\n", col, missing_data$percentages[col]))
        }
      } else {
        cat("No columns with significant missing values\n")
      }
    }
    
    # Outliers summary
    cat("\nOutliers Summary:\n")
    for (year in names(quality_results$licor$outliers)) {
      cat(paste("\nYear", year, ":\n"))
      for (var in names(quality_results$licor$outliers[[year]])) {
        outlier_data <- quality_results$licor$outliers[[year]][[var]]
        if (!is.null(outlier_data) && outlier_data$total_outliers > 0) {
          cat(sprintf("  %s: %d total outliers (%d below bound, %d above bound)\n",
                      var, outlier_data$total_outliers,
                      outlier_data$below_bound, outlier_data$above_bound))
        }
      }
    }
  }
  
  # Print CIMIS data summary
  if (!is.null(quality_results$cimis)) {
    cat("\nCIMIS Data Quality:\n")
    cat("------------------\n")
    
    # Missing values summary
    cat("\nMissing Values:\n")
    for (year in names(quality_results$cimis$missing_values)) {
      cat(paste("\nYear", year, ":\n"))
      missing_data <- quality_results$cimis$missing_values[[year]]
      problematic_cols <- which(missing_data$percentages > 5)
      if (length(problematic_cols) > 0) {
        cat(paste("Columns with >5% missing values:\n"))
        for (col in names(problematic_cols)) {
          cat(sprintf("  %s: %.1f%%\n", col, missing_data$percentages[col]))
        }
      } else {
        cat("No columns with significant missing values\n")
      }
    }
  }
  
  cat("\n")
}

# Helper function to check physiological bounds
check_physiological_bounds <- function(data) {
  bounds <- list(
    A = c(-5, 30),     # Net photosynthesis bounds
    E = c(0, 0.02),    # Transpiration bounds
    gsw = c(0, 1),     # Stomatal conductance bounds
    VPDleaf = c(0, 10) # VPD bounds
  )
  
  out_of_bounds <- lapply(names(bounds), function(var) {
    if (var %in% names(data)) {
      values <- data[[var]]
      lower_bound <- bounds[[var]][1]
      upper_bound <- bounds[[var]][2]
      
      list(
        below = sum(values < lower_bound, na.rm = TRUE),
        above = sum(values > upper_bound, na.rm = TRUE)
      )
    } else {
      NULL
    }
  }) %>% setNames(names(bounds))
  
  return(out_of_bounds)
}