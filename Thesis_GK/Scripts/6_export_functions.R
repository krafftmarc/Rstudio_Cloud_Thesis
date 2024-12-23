# 6_export_functions.R
# Functions for exporting results to files

# Export tables to CSV files
export_tables <- function(results, output_dir = "output_tables") {
  log_message("Starting table export...")
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)
  log_message(paste("Created output directory:", output_dir))
  
  # Validate results structure
  if (!is.list(results)) {
    log_message("ERROR: results must be a list")
    return(FALSE)
  }
  
  if (!all(c("basic_stats", "mixed_models") %in% names(results))) {
    log_message("ERROR: results missing required components")
    log_message(paste("Available components:", paste(names(results), collapse = ", ")))
    return(FALSE)
  }
  
  # Export basic statistics
  tryCatch({
    log_message("Exporting basic statistics...")
    export_basic_stats(results$basic_stats, output_dir)
  }, error = function(e) {
    log_message(paste("ERROR exporting basic stats:", e$message))
  })
  
  # Export model results
  tryCatch({
    log_message("Exporting model results...")
    export_model_results(results$mixed_models, output_dir)
  }, error = function(e) {
    log_message(paste("ERROR exporting model results:", e$message))
  })
  
  # Export VPD analysis results if available
  if (!is.null(results$vpd_analysis) && !is.null(results$vpd_analysis$results)) {
    tryCatch({
      log_message("Exporting VPD analysis results...")
      export_vpd_results(results$vpd_analysis$results, output_dir)
    }, error = function(e) {
      log_message(paste("ERROR exporting VPD analysis:", e$message))
    })
  }
  
  log_message("Table export completed")
}

# Helper function to export basic statistics
export_basic_stats <- function(stats, output_dir) {
  if (!all(c("year_2022", "year_2023", "combined") %in% names(stats))) {
    log_message("ERROR: basic_stats missing required components")
    log_message(paste("Available components:", paste(names(stats), collapse = ", ")))
    return(FALSE)
  }
  
  for (year in c("2022", "2023", "combined")) {
    filename <- file.path(output_dir, paste0("basic_stats_", year, ".csv"))
    tryCatch({
      write.csv(stats[[paste0("year_", year)]], filename, row.names = TRUE)
      log_message(paste("Exported:", filename))
    }, error = function(e) {
      log_message(paste("ERROR exporting", filename, ":", e$message))
    })
  }
}

# Helper function to export model results
export_model_results <- function(models, output_dir) {
  if (is.null(models) || length(models) == 0) {
    log_message("WARNING: No model results to export")
    return(FALSE)
  }
  
  log_message(paste("Processing models:", paste(names(models), collapse = ", ")))
  
  lapply(names(models), function(param) {
    # Export EMMs
    tryCatch({
      if (!is.null(models[[param]]$emm)) {
        # CSV output
        emm_file <- file.path(output_dir, paste0("emmeans_", param, ".csv"))
        emm_data <- as.data.frame(models[[param]]$emm)
        write.csv(emm_data, emm_file, row.names = FALSE)
        log_message(paste("Exported:", emm_file))
        
        # Text output
        txt_file <- file.path(output_dir, paste0("emmeans_", param, ".txt"))
        capture.output(print(models[[param]]$emm), file = txt_file)
        log_message(paste("Exported:", txt_file))
        
        # PNG output - create table visualization
        png_file <- file.path(output_dir, paste0("emmeans_", param, ".png"))
        emm_plot <- gridExtra::tableGrob(emm_data)
        ggsave(png_file, emm_plot, width = 10, height = 6)
        log_message(paste("Exported:", png_file))
      }
    }, error = function(e) {
      log_message(paste("ERROR exporting EMMs for", param, ":", e$message))
    })
    
    # ANOVA results in multiple formats
    tryCatch({
      if (!is.null(models[[param]]$anova)) {
        # CSV output
        anova_file <- file.path(output_dir, paste0("anova_", param, ".csv"))
        anova_df <- as.data.frame(models[[param]]$anova)
        anova_df$Term <- rownames(anova_df)
        write.csv(anova_df, anova_file, row.names = FALSE)
        log_message(paste("Exported:", anova_file))
        
        # Text output
        txt_file <- file.path(output_dir, paste0("anova_", param, ".txt"))
        capture.output(print(models[[param]]$anova), file = txt_file)
        log_message(paste("Exported:", txt_file))
        
        # PNG output
        png_file <- file.path(output_dir, paste0("anova_", param, ".png"))
        anova_plot <- gridExtra::tableGrob(anova_df)
        ggsave(png_file, anova_plot, width = 10, height = 6)
        log_message(paste("Exported:", png_file))
      }
    }, error = function(e) {
      log_message(paste("ERROR exporting ANOVA results for", param, ":", e$message))
    })
  })
}

# Helper function to export VPD analysis results
export_vpd_results <- function(vpd_results, output_dir) {
  if (is.null(vpd_results) || length(vpd_results) == 0) {
    log_message("WARNING: No VPD results to export")
    return(FALSE)
  }
  
  log_message(paste("Processing VPD analyses:", paste(names(vpd_results), collapse = ", ")))
  
  lapply(names(vpd_results), function(param) {
    # Export ANOVA results
    tryCatch({
      if (!is.null(vpd_results[[param]]$anova)) {
        filename <- file.path(output_dir, paste0("vpd_anova_", param, ".csv"))
        anova_df <- as.data.frame(vpd_results[[param]]$anova)
        anova_df$Term <- rownames(anova_df)
        write.csv(anova_df, filename, row.names = FALSE)
        log_message(paste("Exported:", filename))
      }
    }, error = function(e) {
      log_message(paste("ERROR exporting VPD ANOVA for", param, ":", e$message))
    })
    
    # Export EMMs at different VPD levels
    tryCatch({
      if (!is.null(vpd_results[[param]]$emm)) {
        filename <- file.path(output_dir, paste0("vpd_emmeans_", param, ".csv"))
        write.csv(as.data.frame(vpd_results[[param]]$emm),
                  filename, row.names = FALSE)
        log_message(paste("Exported:", filename))
      }
    }, error = function(e) {
      log_message(paste("ERROR exporting VPD EMMs for", param, ":", e$message))
    })
  })
}

# Export figures
export_figures <- function(results, output_dir = "output_figures") {
  log_message("Starting figure export...")
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)
  log_message(paste("Created output directory:", output_dir))
  
  # Validate results structure
  if (!is.list(results)) {
    log_message("ERROR: results must be a list")
    return(FALSE)
  }
  
  # Export treatment effects plot
  tryCatch({
    if (!is.null(results$treatment_plots)) {
      filename <- file.path(output_dir, "treatment_effects.png")
      ggsave(filename, plot = results$treatment_plots,
             width = 10, height = 12, dpi = 300)
      log_message(paste("Exported:", filename))
    } else {
      log_message("WARNING: treatment_plots not found in results")
    }
  }, error = function(e) {
    log_message(paste("ERROR exporting treatment effects plot:", e$message))
  })
  
  # Export VPD response plots - both combined and individual
  tryCatch({
    if (!is.null(results$vpd_analysis$plot)) {
      # Combined VPD response plot
      filename <- file.path(output_dir, "vpd_response_combined.png")
      ggsave(filename, plot = results$vpd_analysis$plot,
             width = 12, height = 15, dpi = 300, limitsize = FALSE)
      log_message(paste("Successfully exported combined VPD response plot:", filename))
      
      # Individual VPD response plots
      if (!is.null(results$vpd_analysis$plots)) {
        # Photosynthesis
        if (!is.null(results$vpd_analysis$plots$photo)) {
          filename <- file.path(output_dir, "vpd_response_photosynthesis.png")
          ggsave(filename, plot = results$vpd_analysis$plots$photo,
                 width = 10, height = 6, dpi = 300)
          log_message(paste("Successfully exported photosynthesis VPD response plot:", filename))
        }
        
        # Conductance
        if (!is.null(results$vpd_analysis$plots$cond)) {
          filename <- file.path(output_dir, "vpd_response_conductance.png")
          ggsave(filename, plot = results$vpd_analysis$plots$cond,
                 width = 10, height = 6, dpi = 300)
          log_message(paste("Successfully exported conductance VPD response plot:", filename))
        }
        
        # Transpiration
        if (!is.null(results$vpd_analysis$plots$trans)) {
          filename <- file.path(output_dir, "vpd_response_transpiration.png")
          ggsave(filename, plot = results$vpd_analysis$plots$trans,
                 width = 10, height = 6, dpi = 300)
          log_message(paste("Successfully exported transpiration VPD response plot:", filename))
        }
      }
      
      # In export_figures function, add:
      # Export interaction plots
      if (!is.null(results$interactions)) {
        if (!is.null(results$interactions$tleaf$plot)) {
          filename <- file.path(output_dir, "tleaf_interactions.png")
          ggsave(filename, plot = results$interactions$tleaf$plot,
                 width = 12, height = 8, dpi = 300)
          log_message(paste("Exported:", filename))
        }
        
        if (!is.null(results$interactions$tmax$plot)) {
          filename <- file.path(output_dir, "tmax_interactions.png")
          ggsave(filename, plot = results$interactions$tmax$plot,
                 width = 12, height = 15, dpi = 300)
          log_message(paste("Exported:", filename))
        }
      }
      
      # If separate temperature response plot exists
      if (!is.null(results$temp_analysis$plot)) {
        filename <- file.path(output_dir, "temp_response.png")
        ggsave(filename, plot = results$temp_analysis$plot,
               width = 12, height = 15, dpi = 300, limitsize = FALSE)
        log_message(paste("Successfully exported temperature response plot:", filename))
      }
      
      # If combined environmental response plot exists
      if (!is.null(results$env_response$plot)) {
        filename <- file.path(output_dir, "environmental_response.png")
        ggsave(filename, plot = results$env_response$plot,
               width = 15, height = 20, dpi = 300, limitsize = FALSE)
        log_message(paste("Successfully exported environmental response plot:", filename))
      }
    } else {
      log_message("WARNING: vpd_analysis plot not found in results")
    }
  }, error = function(e) {
    log_message(paste("ERROR exporting response plots:", e$message))
  })
  
  # Export annual comparison plot
  tryCatch({
    if (!is.null(results$annual_comparison)) {
      filename <- file.path(output_dir, "annual_comparison.png")
      ggsave(filename, plot = results$annual_comparison,
             width = 10, height = 6, dpi = 300)
      log_message(paste("Exported:", filename))
    } else {
      log_message("WARNING: annual_comparison not found in results")
    }
  }, error = function(e) {
    log_message(paste("ERROR exporting annual comparison plot:", e$message))
  })
  
  # Export WUEi plot if it exists
  tryCatch({
    if (!is.null(results$wuei_plot)) {
      filename <- file.path(output_dir, "wuei_comparison.png")
      ggsave(filename, plot = results$wuei_plot,
             width = 10, height = 6, dpi = 300)
      log_message(paste("Exported:", filename))
    }
  }, error = function(e) {
    log_message(paste("ERROR exporting WUEi plot:", e$message))
  })
  
  log_message("Figure export completed")
}