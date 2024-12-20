run_complete_analysis <- function() {
  # Load data
  data_2022 <- load_wp_2022()
  data_2023 <- load_wp_2023()
  cimis_2022 <- load_cimis_2022()
  cimis_2023 <- load_cimis_2023()
  
  # Process data
  combined_data <- process_water_potential(data_2022, data_2023)
  cimis_data <- process_cimis(cimis_2022, cimis_2023)
  
  # Debug columns in cimis_data
  message("Columns in cimis_data:")
  print(colnames(cimis_data))
  
  # Create plots
  plots <- create_comparison_plots(combined_data, cimis_data)
  save_plots(plots)
  
  # Run statistical analysis with VPD
  stats_results <- perform_statistical_analysis(combined_data, cimis_data)
  
  message("Analysis complete!")
  
  return(list(
    combined_data = combined_data,
    cimis_data = cimis_data,
    plots = plots,
    stats = stats_results
  ))
}
