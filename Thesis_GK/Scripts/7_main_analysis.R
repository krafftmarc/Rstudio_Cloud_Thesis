# 7_main_analysis.R
# Main analysis workflow

# Source all required scripts
source("1_packages_setup.R")
source("2_data_loading.R")
source("3_data_processing.R")
source("4_statistical_analysis.R")
source("5_visualization.R")
source("6_export_functions.R")

# Main analysis function
main_analysis <- function(data_paths = default_paths) {
  # Load and check data
  message("Loading data files...")
  all_data <- load_data(data_paths)
  
  message("Checking data quality...")
  quality_check <- check_data_quality(all_data)
  print(quality_check)
  
  # Process data
  licor_2022 <- process_licor_data(all_data$licor_2022, 2022)
  licor_2023 <- process_licor_data(all_data$licor_2023, 2023)
  licor_combined <- bind_rows(licor_2022, licor_2023)
  
  cimis_2022 <- process_cimis_data(all_data$cimis_2022, 2022)
  cimis_2023 <- process_cimis_data(all_data$cimis_2023, 2023)
  
  # Calculate statistics
  basic_stats_2022 <- calculate_basic_stats(licor_2022)
  basic_stats_2023 <- calculate_basic_stats(licor_2023)
  basic_stats_combined <- calculate_basic_stats(licor_combined)
  
  # Run models
  mixed_model_results <- run_mixed_models(licor_combined)
  vpd_analysis <- analyze_vpd_response(licor_combined)
  
  # Create plots
  treatment_plots <- plot_treatment_effects(licor_combined)
  vpd_response_plot <- plot_vpd_response(licor_combined)
  annual_comparison <- plot_annual_comparison(basic_stats_combined)
  
  # Compile results
  results <- list(
    basic_stats = list(
      year_2022 = basic_stats_2022,
      year_2023 = basic_stats_2023,
      combined = basic_stats_combined
    ),
    mixed_models = mixed_model_results,
    treatment_plots = treatment_plots,
    vpd_analysis = list(
      models = vpd_analysis$models,
      plot = vpd_response_plot
    ),
    annual_comparison = annual_comparison
  )
  
  # Export results
  export_tables(results)
  export_figures(results)
  
  return(results)
}