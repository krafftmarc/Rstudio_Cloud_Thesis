# 6_export_functions.R
# Functions for exporting results to files

# Export tables to CSV files
export_tables <- function(results, output_dir = "output_tables") {
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)
  
  # Export basic statistics
  export_basic_stats(results$basic_stats, output_dir)
  
  # Export model results
  export_model_results(results$mixed_models, output_dir)
}

# Helper function to export basic statistics
export_basic_stats <- function(stats, output_dir) {
  write.csv(stats$year_2022, 
            file.path(output_dir, "basic_stats_2022.csv"))
  write.csv(stats$year_2023, 
            file.path(output_dir, "basic_stats_2023.csv"))
  write.csv(stats$combined, 
            file.path(output_dir, "basic_stats_combined.csv"))
}

# Helper function to export model results
export_model_results <- function(models, output_dir) {
  lapply(names(models), function(param) {
    # Export EMMs
    write.csv(as.data.frame(models[[param]]$emm),
              file.path(output_dir, paste0("emmeans_", param, ".csv")))
    
    # Export pairwise comparisons
    write.csv(as.data.frame(models[[param]]$pairs),
              file.path(output_dir, paste0("pairwise_", param, ".csv")))
    
    # Export ANOVA results
    write.csv(as.data.frame(models[[param]]$anova),
              file.path(output_dir, paste0("anova_", param, ".csv")))
  })
}

# Export figures
export_figures <- function(results, output_dir = "output_figures") {
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)
  
  # Export treatment effects plot
  ggsave(file.path(output_dir, "treatment_effects.png"),
         plot = results$treatment_plots,
         width = 10, height = 12, dpi = 300)
  
  # Export VPD response plot
  ggsave(file.path(output_dir, "vpd_response.png"),
         plot = results$vpd_analysis$plot,
         width = 10, height = 8, dpi = 300)
  
  # Export annual comparison plot
  ggsave(file.path(output_dir, "annual_comparison.png"),
         plot = results$annual_comparison,
         width = 10, height = 6, dpi = 300)
}