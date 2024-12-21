# Clear environment and connections
closeAllConnections()

# Source functions file
source("/cloud/project/Thesis_GK/Scripts/functions.R")

# Verify functions are loaded
required_functions <- c(
  "create_comparison_plots",
  "process_cimis",
  "load_cimis_2022",
  "load_cimis_2023"
)

# Check if all functions exist
missing_functions <- required_functions[!sapply(required_functions, exists)]
if (length(missing_functions) > 0) {
  stop("Missing required functions: ", paste(missing_functions, collapse=", "))
}

# Define main analysis function
run_complete_analysis <- function() {
  message("Starting analysis...")
  
  tryCatch({
    # Load data with validation
    message("Loading 2022 data...")
    data_2022 <- load_wp_2022()
    message("Loading 2023 data...")
    data_2023 <- load_wp_2023()
    
    message("Loading CIMIS 2022 data...")
    cimis_2022 <- load_cimis_2022()
    message("Number of rows in CIMIS 2022:", nrow(cimis_2022))
    
    message("Loading CIMIS 2023 data...")
    cimis_2023 <- load_cimis_2023()
    message("Number of rows in CIMIS 2023:", nrow(cimis_2023))
    
    # Process data with validation
    message("Processing water potential data...")
    combined_data <- process_water_potential(data_2022, data_2023)
    message("Number of rows in combined water potential data:", nrow(combined_data))
    
    message("Processing CIMIS data...")
    cimis_data <- process_cimis(cimis_2022, cimis_2023)
    message("Number of rows in processed CIMIS data:", nrow(cimis_data))
    
    # Debug columns in cimis_data
    message("Columns in cimis_data:")
    print(colnames(cimis_data))
    message("First few rows of cimis_data:")
    print(head(cimis_data))
    
    # Validate data before plotting
    if (!all(c("Date", "Temp_C", "RH", "VPD") %in% colnames(cimis_data))) {
      stop("Missing required columns in CIMIS data")
    }
    
    # Create plots with validation
    message("Creating comparison plots...")
    plots <- create_comparison_plots(combined_data, cimis_data)
    message("Number of plots created:", length(plots))
    
    message("Saving plots...")
    save_plots(plots)
    
    # Run statistical analysis with validation
    message("Running statistical analysis...")
    stats_results <- perform_statistical_analysis(combined_data, cimis_data)
    
    message("Analysis complete!")
    
    return(list(
      combined_data = combined_data,
      cimis_data = cimis_data,
      plots = plots,
      stats = stats_results
    ))
  }, error = function(e) {
    message("Error occurred in run_complete_analysis: ", e$message)
    print(sys.calls())
    stop(e)
  })
}

# Run the analysis
results <- run_complete_analysis()