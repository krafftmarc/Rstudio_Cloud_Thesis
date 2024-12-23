# 7_main_analysis.R

main_analysis <- function(paths = default_paths) {
  # Clean directories first
  unlink("output_tables/*")
  unlink("output_figures/*")
  
  # Create directories
  dir.create("output_tables", showWarnings = FALSE, recursive = TRUE)
  dir.create("output_figures", showWarnings = FALSE, recursive = TRUE)
  
  # Load and check data
  log_message("Loading data files...")
  all_data <- load_data(paths)
  
  log_message("Checking data quality...")
  quality_check <- check_data_quality(all_data)
  print(quality_check)
  
  # Process LICOR data
  log_message("\nProcessing LICOR data...")
  licor_2022 <- process_licor_data(all_data$licor_2022, 2022)
  licor_2023 <- process_licor_data(all_data$licor_2023, 2023)
  licor_combined <- bind_rows(licor_2022, licor_2023)
  
  # Process CIMIS data
  log_message("\nProcessing CIMIS data...")
  cimis_2022 <- process_cimis_data(all_data$cimis_2022, 2022)
  cimis_2023 <- process_cimis_data(all_data$cimis_2023, 2023)
  
  # Merge LICOR and CIMIS data
  log_message("\nMerging LICOR and CIMIS data...")
  merged_data <- licor_combined %>%
    left_join(
      bind_rows(
        cimis_2022 %>% select(date, VPD, tmax),
        cimis_2023 %>% select(date, VPD, tmax)
      ),
      by = "date"
    )
  
  # Calculate statistics
  log_message("\nCalculating basic statistics...")
  basic_stats <- calculate_basic_stats(merged_data)
  
  # Run models
  log_message("\nFitting mixed models...")
  mixed_model_results <- run_mixed_models(merged_data)
  
  # Run VPD analysis
  log_message("\nAnalyzing VPD response...")
  vpd_analysis <- analyze_vpd_response(merged_data)
  
  # Create plots
  log_message("\nCreating visualization plots...")
  treatment_plots <- plot_treatment_effects(merged_data)
  vpd_response_plots <- plot_vpd_response(merged_data)
  wuei_plot <- plot_WUEi(merged_data)
  tleaf_plots <- plot_tleaf_responses(merged_data)
  tmax_plots <- plot_tmax_interactions(merged_data)
  annual_comparison <- plot_annual_comparison(basic_stats)
  
  # Compile results
  results <- list(
    data = merged_data,
    models = mixed_model_results,
    treatment_plots = treatment_plots$combined,  # Main combined plot
    individual_plots = treatment_plots$individual,  # Individual plots
    vpd_analysis = list(
      plot = vpd_response_plots,
      models = vpd_analysis$models,
      data = vpd_analysis$data
    ),
    wuei_plot = wuei_plot,
    annual_comparison = annual_comparison,
    interactions = list(
      tleaf = tleaf_plots,
      tmax = tmax_plots
    ),
    basic_stats = list(
      combined = basic_stats
    )
  )
  
  # Export results
  tryCatch({
    export_figures(results)
    export_tables(results)
    message("Results exported successfully")
  }, error = function(e) {
    message(sprintf("Error exporting results: %s", e$message))
  })
  
  return(results)
}