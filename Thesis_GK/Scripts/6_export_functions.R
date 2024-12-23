# 6_export_functions.R
# Functions for exporting results to files

# Add the new table export function
export_tables <- function(results, output_dir = "output_tables") {
  message("\nStarting table export...")
  
  # Create output directory with error handling and full path
  output_dir <- file.path(getwd(), output_dir)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  message(sprintf("Output directory: %s", output_dir))
  
  # Clean old files
  unlink(file.path(output_dir, "*"))
  
  if (!dir.exists(output_dir)) {
    stop(sprintf("Failed to create output directory: %s", output_dir))
  }
  
  # Export basic statistics
  if (!is.null(results$basic_stats)) {
    message("Exporting basic statistics...")
    write.csv(results$basic_stats$combined, 
              file.path(output_dir, "basic_statistics.csv"))
    
    capture.output(
      print(results$basic_stats$combined), 
      file = file.path(output_dir, "basic_statistics.txt")
    )
  }
  
  # Export model results
  if (!is.null(results$models)) {
    vars <- names(results$models)
    message(sprintf("Processing variables: %s", paste(vars, collapse = ", ")))
    
    for (var in vars) {
      # Export ANOVA results
      if (!is.null(results$models[[var]]$anova)) {
        write.csv(
          as.data.frame(results$models[[var]]$anova),
          file.path(output_dir, sprintf("anova_%s.csv", var))
        )
        capture.output(
          print(results$models[[var]]$anova),
          file = file.path(output_dir, sprintf("anova_%s.txt", var))
        )
      }
      
      # Export EMMs
      if (!is.null(results$models[[var]]$emm)) {
        write.csv(
          as.data.frame(results$models[[var]]$emm),
          file.path(output_dir, sprintf("emmeans_%s.csv", var))
        )
        capture.output(
          print(results$models[[var]]$emm),
          file = file.path(output_dir, sprintf("emmeans_%s.txt", var))
        )
      }
      
      # Export contrasts
      if (!is.null(results$models[[var]]$contrasts)) {
        write.csv(
          as.data.frame(results$models[[var]]$contrasts),
          file.path(output_dir, sprintf("contrasts_%s.csv", var))
        )
        capture.output(
          print(results$models[[var]]$contrasts),
          file = file.path(output_dir, sprintf("contrasts_%s.txt", var))
        )
      }
    }
  }
  
  message("\nTable export completed")
  return(TRUE)
}

# Export figures function with improved error handling
export_figures <- function(results, output_dir = "output_figures") {
  message("\nStarting figure export...")
  
  # Create output directory with error handling and full path
  output_dir <- file.path(getwd(), output_dir)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  message(sprintf("Output directory: %s", output_dir))
  
  # ADD THE unlink LINE HERE, before the if statement
  unlink(file.path(output_dir, "*"))  # Clean old files
  
  if (!dir.exists(output_dir)) {
    stop(sprintf("Failed to create output directory: %s", output_dir))
  }
  
  # Helper function for safe plot saving
  safe_save_plot <- function(plot, filename, width, height, ...) {
    tryCatch({
      # Save as both PDF and PNG
      ggsave(filename = sub("\\.pdf$", ".pdf", filename),
             plot = plot,
             width = width,
             height = height,
             device = "pdf",
             dpi = 300,
             ...)
      message(sprintf("Saved PDF: %s", basename(filename)))
      
      ggsave(filename = sub("\\.pdf$", ".png", filename),
             plot = plot,
             width = width,
             height = height,
             device = "png",
             dpi = 300,
             ...)
      message(sprintf("Saved PNG: %s", sub("\\.pdf$", ".png", basename(filename))))
      return(TRUE)
    }, error = function(e) {
      message(sprintf("ERROR saving %s: %s", basename(filename), e$message))
      return(FALSE)
    })
  }
  
  # Track successful exports
  exports <- list()
  
  # Export treatment effects plot
  if (!is.null(results$treatment_plots)) {
    message("\nExporting treatment effects plot...")
    exports$treatment <- safe_save_plot(
      results$treatment_plots,
      file.path(output_dir, "treatment_effects.pdf"),
      width = 12,
      height = 16
    )
  }
  
  # Export VPD response plots
  if (!is.null(results$vpd_analysis)) {
    message("\nExporting VPD response plots...")
    if (!is.null(results$vpd_analysis$plot)) {
      exports$vpd <- safe_save_plot(
        results$vpd_analysis$plot,
        file.path(output_dir, "vpd_response.pdf"),
        width = 10,
        height = 14
      )
    }
  }
  
  # Export WUEi plot
  if (!is.null(results$wuei_plot)) {
    message("\nExporting WUEi plot...")
    exports$wuei <- safe_save_plot(
      results$wuei_plot,
      file.path(output_dir, "wuei_comparison.pdf"),
      width = 8,
      height = 6
    )
  }
  
  # Export annual comparison plot
  if (!is.null(results$annual_comparison)) {
    message("\nExporting annual comparison plot...")
    exports$annual <- safe_save_plot(
      results$annual_comparison,
      file.path(output_dir, "annual_comparison.pdf"),
      width = 8,
      height = 6
    )
  }
  
  # Export interaction plots
  if (!is.null(results$interactions)) {
    message("\nExporting interaction plots...")
    if (!is.null(results$interactions$tleaf$plot)) {
      exports$tleaf <- safe_save_plot(
        results$interactions$tleaf$plot,
        file.path(output_dir, "tleaf_interactions.pdf"),
        width = 10,
        height = 8
      )
    }
    if (!is.null(results$interactions$tmax$plot)) {
      exports$tmax <- safe_save_plot(
        results$interactions$tmax$plot,
        file.path(output_dir, "tmax_interactions.pdf"),
        width = 12,
        height = 15
      )
    }
  }
  
  # Print summary of exports
  message("\nExport Summary:")
  message(sprintf("Successfully exported %d/%d plots", 
                  sum(unlist(exports)), length(exports)))
  
  message("\nFigure export completed")
  return(invisible(exports))
}

# Function to verify exported files with improved checking
verify_exports <- function(results, tables_dir = "output_tables", figures_dir = "output_figures") {
  message("\nVerifying exported files...")
  
  # Get full paths
  tables_dir <- file.path(getwd(), tables_dir)
  figures_dir <- file.path(getwd(), figures_dir)
  
  # Check directories
  dirs_exist <- c(
    tables = dir.exists(tables_dir),
    figures = dir.exists(figures_dir)
  )
  
  message("\nDirectory Status:")
  message(sprintf("Tables directory (%s): %s", tables_dir, 
                  if(dirs_exist["tables"]) "OK" else "Missing"))
  message(sprintf("Figures directory (%s): %s", figures_dir, 
                  if(dirs_exist["figures"]) "OK" else "Missing"))
  
  # List files if directories exist
  if (dirs_exist["tables"]) {
    message("\nExisting table files:")
    table_files <- list.files(tables_dir, pattern = "\\.(csv|txt)$")
    if (length(table_files) > 0) {
      message(paste(" -", table_files, collapse = "\n"))
    } else {
      message("No table files found")
    }
  }
  
  if (dirs_exist["figures"]) {
    message("\nExisting figure files:")
    figure_files <- list.files(figures_dir, pattern = "\\.(pdf|png)$")
    if (length(figure_files) > 0) {
      message(paste(" -", figure_files, collapse = "\n"))
    } else {
      message("No figure files found")
    }
  }
  
  message("\nExport verification completed")
  return(invisible(list(
    dirs_exist = dirs_exist,
    table_files = if(dirs_exist["tables"]) table_files else character(0),
    figure_files = if(dirs_exist["figures"]) figure_files else character(0)
  )))
}