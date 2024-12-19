# Part 5: Main Analysis Workflow --------------------------------------------------

# Main analysis workflow function
main_analysis <- function(water_potential_2022, water_potential_2023, 
                          cimis_2022, cimis_2023) {
  # Process data
  wp_data <- process_water_potential(water_potential_2022, water_potential_2023)
  cimis_data <- process_cimis(cimis_2022, cimis_2023)
  
  # Merge datasets
  merged_data <- wp_data %>%
    left_join(cimis_data, by = c("Date", "Season"))
  
  # Perform statistical analysis
  models <- perform_statistical_analysis(merged_data)
  
  # Create visualizations
  plots <- create_publication_plots(merged_data)
  
  # Generate summary tables
  tables <- create_summary_tables(merged_data)
  
  # Return results
  return(list(
    data = merged_data,
    models = models,
    plots = plots,
    tables = tables
  ))
}

# Complete workflow script
run_complete_analysis <- function() {
  # 1. Create output directories
  create_output_dirs()
  
  # 2. Load and validate data
  message("Loading and validating data...")
  wp_2022 <- load_wp_2022()
  wp_2023 <- load_wp_2023()
  cimis_2022 <- load_cimis_2022()
  cimis_2023 <- load_cimis_2023()
  
  # 3. Check data consistency
  message("\nChecking data consistency...")
  check_data_consistency(wp_2022, wp_2023, cimis_2022, cimis_2023)
  
  # 4. Run main analysis
  message("\nRunning main analysis...")
  results <- main_analysis(wp_2022, wp_2023, cimis_2022, cimis_2023)
  
  # 5. Save all outputs
  message("\nSaving outputs...")
  
  # Save plots
  save_plots(results$plots)
  
  # Save tables
  export_tables(results$tables)
  
  # Save statistical results
  save_statistical_results(results$models)
  
  # 6. Print completion message
  print_output_locations()
  
  # Return results object for further analysis if needed
  return(results)
}

# Usage Example:
# To run the complete analysis:
# results <- run_complete_analysis()

# To save the results object for later use:
# saveRDS(results, "results/analysis_results.rds")

# If you want to load the results later:
# results <- readRDS("results/analysis_results.rds")

# File organization:
# Create the following files:
# 1. functions.R: Contains all functions from parts 1-4
# 2. run_analysis.R: Contains the main workflow from part 5
# 3. analysis_script.R: Main script to run everything

# Example contents of analysis_script.R:
#
# # Load all functions
# source("functions.R")
#
# # Run complete analysis
# results <- run_complete_analysis()
#
# # Optional: Save results object
# saveRDS(results, "results/analysis_results.rds")