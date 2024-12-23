# 7_main_analysis.R
main_analysis <- function(paths = default_paths) {
  # Load and check data
  log_message("Loading data files...")
  all_data <- load_data(paths)
  
  log_message("Checking data quality...")
  quality_check <- check_data_quality(all_data)
  print(quality_check)
  
  # Process data
  log_message("\nProcessing LICOR data...")
  licor_2022 <- process_licor_data(all_data$licor_2022, 2022)
  licor_2023 <- process_licor_data(all_data$licor_2023, 2023)
  licor_combined <- bind_rows(licor_2022, licor_2023)
  
  log_message("Checking combined data structure:")
  log_message(paste("Treatment levels:", paste(unique(licor_combined$treatment), collapse=", ")))
  log_message(paste("Years:", paste(unique(licor_combined$year), collapse=", ")))
  log_message("Number of rows by treatment and year:")
  print(table(licor_combined$treatment, licor_combined$year))
  
  # Process CIMIS data with diagnostic output
  log_message("\nProcessing CIMIS data...")
  log_message("CIMIS 2022 original columns:")
  log_message(paste(names(all_data$cimis_2022), collapse=", "))
  cimis_2022 <- process_cimis_data(all_data$cimis_2022, 2022)
  log_message("CIMIS 2022 processed columns:")
  log_message(paste(names(cimis_2022), collapse=", "))
  
  log_message("CIMIS 2023 original columns:")
  log_message(paste(names(all_data$cimis_2023), collapse=", "))
  cimis_2023 <- process_cimis_data(all_data$cimis_2023, 2023)
  log_message("CIMIS 2023 processed columns:")
  log_message(paste(names(cimis_2023), collapse=", "))
  
  # Merge LICOR and CIMIS data
  log_message("\nMerging LICOR and CIMIS data...")
  licor_2022_merged <- merge_vpd_data(licor_2022, cimis_2022)
  licor_2023_merged <- merge_vpd_data(licor_2023, cimis_2023)
  licor_combined <- bind_rows(licor_2022_merged, licor_2023_merged)
  
  # Verify merged data columns
  log_message("Merged data columns:")
  log_message(paste(names(licor_combined), collapse=", "))
  
  # Calculate statistics
  log_message("\nCalculating basic statistics...")
  basic_stats_2022 <- calculate_basic_stats(licor_2022)
  basic_stats_2023 <- calculate_basic_stats(licor_2023)
  basic_stats_combined <- calculate_basic_stats(licor_combined)
  
  # Run models with error checking
  log_message("\nFitting mixed models...")
  mixed_model_results <- run_mixed_models(licor_combined)
  
  # Run VPD analysis
  log_message("\nAnalyzing VPD response...")
  vpd_analysis <- analyze_vpd_response(licor_combined)
  
  # Create plots
  log_message("\nCreating visualization plots...")
  treatment_plots <- plot_treatment_effects(licor_combined)
  vpd_response_plot <- plot_vpd_response(licor_combined)
  annual_comparison <- plot_annual_comparison(basic_stats_combined)
  wuei_plot <- plot_WUEi(licor_combined)
  tleaf_interactions <- plot_tleaf_responses(licor_combined)
  tmax_interactions <- plot_tmax_interactions(licor_combined)
  
  # Save WUEi plot with error checking
  tryCatch({
    ggsave("output_figures/WUEi_plot.png", plot = wuei_plot, width = 10, height = 6, dpi = 300)
    log_message("WUEi plot saved successfully")
  }, error = function(e) {
    log_message(paste("ERROR saving WUEi plot:", e$message))
  })
  
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
    annual_comparison = annual_comparison,
    wuei_plot = wuei_plot
  )
  
  # Export results
  export_tables(results)
  export_figures(results)
  
  return(results)
}

# Add flag to prevent auto-execution when sourced
if (!exists("IS_SOURCED")) {
  IS_SOURCED <- TRUE
}