# Part 1: Load required libraries
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
library(gt)
library(ggpubr)
library(car)
library(multcomp)
library(performance)
library(MuMIn)
library(grid)
library(gtable)
library(webshot2)
library(gt)

# Add after library imports in functions.R
calculate_r2 <- function(model) {
  r2 <- MuMIn::r.squaredGLMM(model)
  result <- r2[1, ]  # Get first row as this contains the R2m and R2c values
  return(list(
    marginal = result["R2m"],
    conditional = result["R2c"]
  ))
}

calculate_pvalues <- function(model) {
  # Use car::Anova to get chi-square test results
  anova_result <- car::Anova(model, type = 3, test.statistic = "Chisq")
  return(anova_result)
}

# Part 2: Data Loading Functions

# Function to load 2022 Water Potential data
load_wp_2022 <- function() {
  tryCatch({
    wp_2022 <- Tyree_2022_Cleaned %>%
      dplyr::mutate(
        Date = as.Date(paste(Year, Month, Day, sep = "-")),
        Season = "2022",
        Variety = factor(Variety, levels = c("CH", "CS")),
        Time_of_Day = case_when(
          Time_id == "1PM" ~ "Midday",
          Time_id == "5AM" ~ "Pre-dawn",
          TRUE ~ Time_id
        ),
        Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday")),
        Block_ID = as.factor(Block_ID),
        Vine_ID = as.factor(Vine_ID),
        Tx = case_when(
          is.nan(as.numeric(as.character(Tx))) ~ "Baseline",
          is.na(Tx) ~ "Baseline",
          TRUE ~ as.character(Tx)
        ),
        Stress = NA_character_
      ) %>%
      dplyr::filter(!is.na(PSI), !is.na(Variety))
    
    message("Successfully loaded 2022 Water Potential data")
    return(wp_2022)
  }, error = function(e) {
    message("Error loading 2022 Water Potential data: ", e$message)
    return(NULL)
  })
}

# Function to load 2023 Water Potential data
load_wp_2023 <- function() {
  tryCatch({
    wp_2023 <- Clean_Tyree_Water_Potentials_2023 %>%
      dplyr::mutate(
        Date = date,
        Season = "2023",
        Variety = factor(variety, levels = c("CH", "CS")),
        Time_of_Day = case_when(
          time_block == 500 ~ "Pre-dawn",
          time_block == 1300 ~ "Midday",
          TRUE ~ as.character(time_block)
        ),
        Time_of_Day = factor(Time_of_Day, levels = c("Pre-dawn", "Midday")),
        Block_ID = as.factor(block),
        Vine_ID = as.factor(vine_id),
        PSI = `Ψ`,
        Tx = ifelse(is.na(Tx), "Baseline", as.character(Tx)),
        pot_type = tolower(pot_type)
      ) %>%
      dplyr::filter(!is.na(PSI), !is.na(Variety))
    
    # Select only needed columns
    wp_2023_selected <- wp_2023 %>%
      dplyr::transmute(
        Date = Date,
        Season = Season,
        Variety = Variety,
        Time_of_Day = Time_of_Day,
        Block_ID = Block_ID,
        Vine_ID = Vine_ID,
        Tx = Tx,
        PSI = PSI,
        pot_type = pot_type,
        Stress = Stress
      )
    
    message("Successfully loaded 2023 Water Potential data")
    return(wp_2023_selected)
  }, error = function(e) {
    message("Error loading 2023 Water Potential data: ", e$message)
    return(NULL)
  })
}

# Process water potential data with comprehensive treatment standardization
process_water_potential <- function(data_2022, data_2023) {
  message("Starting water potential data processing...")
  
  # Validate inputs
  if(is.null(data_2022) || is.null(data_2023)) {
    stop("Both 2022 and 2023 data must be provided")
  }
  
  # Define common columns
  common_cols <- c("Date", "Season", "Variety", "Time_of_Day", 
                   "Block_ID", "Vine_ID", "Tx", "PSI", 
                   "pot_type", "Stress")
  
  # Verify all required columns exist in both datasets
  missing_cols_2022 <- common_cols[!common_cols %in% names(data_2022)]
  missing_cols_2023 <- common_cols[!common_cols %in% names(data_2023)]
  
  if(length(missing_cols_2022) > 0 || length(missing_cols_2023) > 0) {
    stop(sprintf("Missing columns: 2022: %s, 2023: %s", 
                 paste(missing_cols_2022, collapse=", "), 
                 paste(missing_cols_2023, collapse=", ")))
  }
  
  # Process 2022 data
  message("Processing 2022 data...")
  data_2022_processed <- data_2022 %>%
    dplyr::select(all_of(common_cols)) %>%
    # Standardize pot_type to lowercase if it exists
    mutate(
      pot_type = if("pot_type" %in% names(.)) tolower(pot_type) else "leaf",
      # Ensure Treatment is properly formatted
      Treatment = case_when(
        is.na(Stress) ~ as.character(Tx),
        TRUE ~ paste(Tx, Stress, sep="_")
      )
    )
  
  # Process 2023 data
  message("Processing 2023 data...")
  data_2023_processed <- data_2023 %>%
    dplyr::select(all_of(common_cols)) %>%
    mutate(
      pot_type = if("pot_type" %in% names(.)) tolower(pot_type) else "leaf",
      Treatment = case_when(
        is.na(Stress) ~ as.character(Tx),
        TRUE ~ paste(Tx, Stress, sep="_")
      )
    )
  
  # Combine datasets and standardize treatment labels
  message("Combining datasets...")
  combined_data <- bind_rows(data_2022_processed, data_2023_processed) %>%
    dplyr::mutate(
      Season = factor(Season),
      # Updated Tx standardization logic
      Tx = case_when(
        Tx %in% c("2", "2L", 2) ~ "2L",  # Handle "2" and its variations
        Tx %in% c("4", "4L", 4) ~ "4L",  # Handle "4" and its variations
        Tx %in% c("Baseline", NA, "") ~ "Baseline",  # Handle empty or NA values
        TRUE ~ as.character(Tx)  # Convert all other cases to characters
      ),
      # Ensure Tx is a factor with consistent levels
      Tx = factor(Tx, levels = c("Baseline", "2L", "4L")),
      # Ensure pot_type is factor with consistent levels
      pot_type = factor(pot_type, levels = c("leaf", "stem"))
    )
  
  # Add diagnostic output
  message("\nData processing summary:")
  message("Total rows in combined dataset: ", nrow(combined_data))
  message("Unique values in Tx: ", paste(unique(combined_data$Tx), collapse=", "))
  message("Unique values in pot_type: ", paste(unique(combined_data$pot_type), collapse=", "))
  
  message("\nPot type distribution by year and variety:")
  print(table(combined_data$Season, combined_data$pot_type, combined_data$Variety))
  
  return(combined_data)
}

# Create helper function to validate column presence
validate_columns <- function(data, required_cols, dataset_name) {
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if(length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in %s: %s", 
                 dataset_name, 
                 paste(missing_cols, collapse=", ")))
  }
}

# Function to load 2022 CIMIS data
load_cimis_2022 <- function() {
  tryCatch({
    message("Loading 2022 CIMIS data...")
    data_2022 <- CIMIS_growing_season_2022 %>%
      dplyr::select(
        Date,
        temp = `Avg Air Temp (F)`,
        rh = `Avg Rel Hum (%)`
      ) %>%
      dplyr::mutate(
        Date = as.Date(Date, format = "%m/%d/%Y"),
        Season = "2022",
        temp = as.numeric(temp),
        rh = as.numeric(rh)
      ) %>%
      dplyr::filter(!is.na(temp), !is.na(rh), rh > 0)
    
    message("Successfully loaded 2022 CIMIS data")
    return(data_2022)
  }, error = function(e) {
    message("Error loading 2022 CIMIS data: ", e$message)
    return(NULL)
  })
}

# Function to load 2023 CIMIS data
load_cimis_2023 <- function() {
  tryCatch({
    message("Loading 2023 CIMIS data...")
    data_2023 <- CIMIS_2023 %>%
      dplyr::select(
        Date,
        temp = `Avg Air Temp (F)`,
        rh = `Avg Rel Hum (%)`
      ) %>%
      dplyr::mutate(
        Date = as.Date(Date, format = "%m/%d/%Y"),
        Season = "2023",
        temp = as.numeric(temp),
        rh = as.numeric(rh)
      ) %>%
      dplyr::filter(!is.na(temp), !is.na(rh), rh > 0)
    
    message("Successfully loaded 2023 CIMIS data")
    return(data_2023)
  }, error = function(e) {
    message("Error loading 2023 CIMIS data: ", e$message)
    return(NULL)
  })
}

# Function to process CIMIS data
process_cimis <- function(cimis_2022, cimis_2023) {
  message("\nStarting CIMIS processing...")
  
  process_year_data <- function(data) {
    if(is.null(data)) return(NULL)
    
    data %>%
      dplyr::mutate(
        Temp_C = (temp - 32) * 5/9,  # Convert F to C
        RH = rh,
        VPD = calculate_vpd(Temp_C, RH)
      ) %>%
      dplyr::select(
        Date,
        Season,
        Temp_C,
        RH,
        VPD
      )
  }
  
  # Process each year
  data_2022 <- process_year_data(cimis_2022)
  data_2023 <- process_year_data(cimis_2023)
  
  # Combine and arrange
  combined_cimis <- dplyr::bind_rows(data_2022, data_2023) %>%
    dplyr::arrange(Date)
  
  message("CIMIS processing complete")
  return(combined_cimis)
}

# VPD calculation function
calculate_vpd <- function(temp_C, RH) {
  if(any(is.na(temp_C)) || any(is.na(RH))) {
    warning("Missing values in temperature or RH data")
    return(NA)
  }
  
  # Add this after calculate_vpd() function
  calculate_model_r2 <- function(model) {
    r2 <- r.squaredGLMM(model)
    list(
      marginal = r2[1,"R2m"],
      conditional = r2[1,"R2c"]
    )
  }
  
  # Calculate saturation vapor pressure (es) using Tetens equation
  es <- 0.611 * exp((17.27 * temp_C) / (temp_C + 237.3))
  # Calculate actual vapor pressure (ea)
  ea <- es * (RH / 100)
  # Calculate VPD in kPa
  vpd <- es - ea
  return(vpd)
}

# Add this after calculate_vpd() function and before Part 3: Visualization Functions

# Statistical analysis function
perform_statistical_analysis <- function(combined_data, cimis_data) {
  # Create output directories if they don't exist
  if (!dir.exists("tables")) dir.create("tables")
  
  # Merge water potential and environmental data
  analysis_data <- combined_data %>%
    left_join(cimis_data %>% dplyr::select(Date, Temp_C, VPD), by = "Date")
  
  # Basic Analysis with VPD - filter out NA Time_of_Day
  basic_stats <- suppressWarnings({
    analysis_data %>%
      filter(!is.na(Time_of_Day)) %>%
      group_by(Season, Variety, Tx, Time_of_Day) %>%
      summarise(
        n = n(),
        mean_psi = round(mean(PSI, na.rm = TRUE), 2),
        sd_psi = round(sd(PSI, na.rm = TRUE), 2),
        mean_vpd = round(mean(VPD, na.rm = TRUE), 2),
        sd_vpd = round(sd(VPD, na.rm = TRUE), 2),
        .groups = 'drop'
      )
  })
  
  # Create formatted GT table for basic stats
  basic_stats_table <- basic_stats %>%
    gt() %>%
    tab_header(
      title = md("**Water Potential and VPD Summary Statistics**"),
      subtitle = "By Season, Variety, Treatment, and Time of Day"
    ) %>%
    fmt_number(
      columns = c(mean_psi, sd_psi, mean_vpd, sd_vpd),
      decimals = 2
    ) %>%
    cols_label(
      Season = "Season",
      Variety = "Variety",
      Tx = "Treatment",
      Time_of_Day = "Time of Day",
      n = "N",
      mean_psi = md("Mean Ψ (MPa)"),
      sd_psi = "SD Ψ",
      mean_vpd = "Mean VPD (kPa)",
      sd_vpd = "SD VPD"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      ),
      locations = cells_column_labels()
    ) %>%
    tab_options(
      heading.background.color = "ghostwhite",
      column_labels.background.color = "ghostwhite",
      table.border.top.width = px(2),
      table.border.bottom.width = px(2)
    )
  
  # Save tables without expand parameter
  gtsave(basic_stats_table, 
         filename = "tables/basic_stats_summary.html")
  
  gtsave(basic_stats_table, 
         filename = "tables/basic_stats_summary.png")
  
  # Correlation analysis
  correlations <- suppressWarnings({
    analysis_data %>%
      filter(!is.na(Time_of_Day)) %>%
      group_by(Season, Variety, Time_of_Day) %>%
      summarise(
        n = n(),
        temp_cor = round(cor(PSI, Temp_C, use = "complete.obs"), 3),
        vpd_cor = round(cor(PSI, VPD, use = "complete.obs"), 3),
        .groups = 'drop'
      )
  })
  
  # Create correlation table
  correlation_table <- correlations %>%
    gt() %>%
    tab_header(
      title = "Correlations between Water Potential and Environmental Variables",
      subtitle = "By Season, Variety, and Time of Day"
    ) %>%
    fmt_number(
      columns = c(temp_cor, vpd_cor),
      decimals = 3
    )
  
  # Save correlation table
  gtsave(correlation_table, 
         filename = "tables/correlation_summary.html")
  
  gtsave(correlation_table, 
         filename = "tables/correlation_summary.png")
  
  # Mixed effects models
  models <- list()
  for(time in c("Pre-dawn", "Midday")) {
    subset_data <- analysis_data %>%
      filter(Time_of_Day == time) %>%
      filter(!is.na(PSI), !is.na(VPD)) %>%
      mutate(across(c(Tx, Season, Variety), factor))
    
    suppressWarnings({
      model <- lmer(PSI ~ Tx + Variety + VPD + (1|Block_ID),
                    data = subset_data,
                    control = lmerControl(check.nobs.vs.nlev = "ignore",
                                          check.nobs.vs.rankZ = "ignore",
                                          check.nobs.vs.nRE = "ignore"))
      models[[time]] <- model
    })
  }
  
  # Save detailed statistical results
  sink("tables/statistical_analysis_detailed.txt")
  cat("COMPREHENSIVE STATISTICAL ANALYSIS RESULTS\n")
  cat("========================================\n\n")
  
  cat("1. SUMMARY STATISTICS\n")
  cat("-----------------\n\n")
  print(basic_stats, n = Inf)
  cat("\n\n")
  
  cat("2. CORRELATIONS\n")
  cat("-------------\n\n")
  print(correlations, n = Inf)
  cat("\n\n")
  
  cat("3. MIXED EFFECTS MODEL RESULTS\n")
  cat("--------------------------\n\n")
  for(time in c("Pre-dawn", "Midday")) {
    cat(sprintf("\n%s Analysis:\n", time))
    cat("================== \n")
    
    # Model Summary
    cat("Model Summary:\n")
    print(summary(models[[time]]))
    cat("\n")
    
    # R² values
    r2_stats <- calculate_r2(models[[time]])
    cat("Model R² Values:\n")
    cat("--------------\n")
    cat(sprintf("Marginal R² (fixed effects): %.3f\n", r2_stats$marginal))
    cat(sprintf("Conditional R² (total): %.3f\n", r2_stats$conditional))
    cat("\n")
    
    # Type III tests with p-values
    cat("Type III Analysis of Deviance (Wald chi-square tests):\n")
    cat("------------------------------------------------\n")
    anova_table <- calculate_pvalues(models[[time]])
    print(anova_table)
    cat("\n-------------------\n")
  }
  
  sink()
  
  return(list(
    basic_stats = basic_stats,
    correlations = correlations,
    models = models,
    tables = list(
      basic_stats_table = basic_stats_table,
      correlation_table = correlation_table
    )
  ))
}

# Helper function to create output directories
create_output_dirs <- function() {
  dirs <- c("figures", "tables")
  for(dir in dirs) {
    if(!dir.exists(dir)) dir.create(dir)
  }
}

# Part 3: Visualization Functions ------------------------------------------------

# Create comparison plots with temperature and VPD
create_comparison_plots <- function(combined_data, cimis_data) {
  message("Starting plot creation...")
  plots <- list()
  
  # Validate input data
  if(is.null(combined_data) || is.null(cimis_data)) {
    stop("Both combined_data and cimis_data must be provided")
  }
  
  # Required columns check
  required_cols_combined <- c("Date", "PSI", "Variety", "Time_of_Day", "Tx", "pot_type")
  required_cols_cimis <- c("Date", "Temp_C", "VPD")
  
  validate_columns(combined_data, required_cols_combined, "combined_data")
  validate_columns(cimis_data, required_cols_cimis, "cimis_data")
  
  # Merge water potential and CIMIS data
  analysis_data <- combined_data %>%
    left_join(cimis_data %>% dplyr::select(Date, Temp_C, VPD), by = "Date") %>%
    mutate(Date = as.Date(Date))  # Ensure Date is properly formatted
  
  # Create plots for each variety, year, and pot_type combination
  for(var in c("CH", "CS")) {
    for(year in c("2022", "2023")) {
      for(pot in unique(combined_data$pot_type)) {
        message(sprintf("Creating plot for %s %s %s", var, year, pot))
        
        # Subset data
        subset_data <- analysis_data %>%
          filter(
            Variety == var,
            Season == year,
            pot_type == pot,
            !is.na(PSI),
            !is.na(VPD)
          )
        
        if(nrow(subset_data) > 0) {
          # Create plot
          plot_key <- paste(var, year, tolower(pot), sep="_")
          
          p <- ggplot() +
            # Water potential points
            geom_point(data = subset_data, 
                       aes(x = Date, y = PSI, color = Time_of_Day, shape = Tx),
                       alpha = 0.7, size = 3) +
            
            # Water potential trend lines
            geom_smooth(data = subset_data,
                        aes(x = Date, y = PSI, color = Time_of_Day),
                        method = "loess", se = TRUE, alpha = 0.2) +
            
            # Temperature line
            geom_line(data = subset_data,
                      aes(x = Date, y = -Temp_C * 0.2, linetype = "Temperature"),
                      color = "red", linewidth = 1) +
            
            # VPD line
            geom_line(data = subset_data,
                      aes(x = Date, y = VPD * -2, linetype = "VPD"),
                      color = "blue", linewidth = 1) +
            
            # Scales and labels
            scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
            scale_y_continuous(
              name = "Water Potential (MPa)",
              sec.axis = sec_axis(~ -. / 0.2, name = "Temperature (°C)")
            ) +
            scale_linetype_manual(
              name = "Environmental Variables",
              values = c("Temperature" = "solid", "VPD" = "dashed")
            ) +
            scale_color_brewer(palette = "Set1") +
            
            # Labels
            labs(
              title = paste(var, year, pot, "Water Potential with Environmental Variables"),
              x = "Date",
              color = "Time of Day",
              shape = "Treatment"
            ) +
            
            # Theme
            theme_classic() +
            theme(
              legend.position = "bottom",
              legend.box = "vertical",
              plot.title = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 11)
            )
          
          plots[[plot_key]] <- p
          message("Created plot for: ", plot_key)
        } else {
          message("No data available for: ", var, " ", year, " ", pot)
        }
      }
    }
  }
  
  message("Plot creation complete. Created ", length(plots), " plots.")
  return(plots)
}

# Save plots function
save_plots <- function(plots, output_dir = "figures") {
  message("Starting to save plots...")
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
    message("Created output directory: ", output_dir)
  }
  
  # Loop through all plots in the list and save them
  for (plot_name in names(plots)) {
    tryCatch({
      file_path <- file.path(output_dir, paste0(plot_name, ".png"))
      message("Saving plot to: ", file_path)
      
      ggsave(
        filename = file_path,
        plot = plots[[plot_name]],
        width = 10,
        height = 8,
        dpi = 300
      )
      
      message("Successfully saved plot: ", plot_name)
    }, error = function(e) {
      message("Error saving plot '", plot_name, "': ", e$message)
    })
  }
  message("Finished saving all plots")
}

# Part 4: Main Analysis Function ------------------------------------------------

run_complete_analysis <- function() {
  message("Starting analysis...")
  
  # Load data with validation
  message("Loading 2022 data...")
  data_2022 <- load_wp_2022()
  if(is.null(data_2022)) stop("Failed to load 2022 data")
  
  message("Loading 2023 data...")
  data_2023 <- load_wp_2023()
  if(is.null(data_2023)) stop("Failed to load 2023 data")
  
  message("Loading CIMIS 2022 data...")
  cimis_2022 <- load_cimis_2022()
  if(is.null(cimis_2022)) stop("Failed to load CIMIS 2022 data")
  
  message("Loading CIMIS 2023 data...")
  cimis_2023 <- load_cimis_2023()
  if(is.null(cimis_2023)) stop("Failed to load CIMIS 2023 data")
  
  # Process data
  message("Processing water potential data...")
  combined_data <- process_water_potential(data_2022, data_2023)
  
  message("Processing CIMIS data...")
  cimis_data <- process_cimis(cimis_2022, cimis_2023)
  
  # Create plots
  message("Creating comparison plots...")
  plots <- create_comparison_plots(combined_data, cimis_data)
  
  message("Saving plots...")
  save_plots(plots)
  
  # Run statistical analysis
  message("Running statistical analysis...")
  stats_results <- perform_statistical_analysis(combined_data, cimis_data)
  
  message("Analysis complete!")
  
  return(list(
    combined_data = combined_data,
    cimis_data = cimis_data,
    plots = plots,
    stats = stats_results
  ))
}

# Helper function to ensure required files exist
check_required_files <- function() {
  required_files <- c(
    "Tyree_2022_Cleaned",
    "Clean_Tyree_Water_Potentials_2023",
    "CIMIS_growing_season_2022",
    "CIMIS_2023"
  )
  
  missing_files <- required_files[!sapply(required_files, exists)]
  
  if(length(missing_files) > 0) {
    message("Missing required data files:")
    print(missing_files)
    return(FALSE)
  }
  
  return(TRUE)
}

# Updated table export function that doesn't rely on gtable
export_tables <- function(tables, filename_prefix = "water_potential_summary") {
  # Create directories if they don't exist
  create_output_dirs()
  
  # Export raw statistics to CSV
  write.csv(tables$raw_stats, 
            file = file.path("tables", paste0(filename_prefix, "_raw.csv")),
            row.names = FALSE)
  
  # Export formatted table as HTML
  gtsave(tables$formatted_table,
         filename = file.path("tables", paste0(filename_prefix, ".html")))
  
  # Export formatted table as PNG (alternative to gtable_render)
  filepath <- file.path("tables", paste0(filename_prefix, ".png"))
  gt::gtsave(
    tables$formatted_table,
    filename = filepath,
    expand = 10
  )
}

# Function to save statistical results
save_statistical_results <- function(models) {
  # Create directories if they don't exist
  create_output_dirs()
  
  # Save to text file
  sink("tables/statistical_results.txt")
  
  cat("Statistical Analysis Results\n\n")
  cat("=============================\n\n")
  
  # Print Pre-dawn results
  cat("\nPRE-DAWN RESULTS\n")
  cat("----------------\n")
  summary_prewritten <- capture.output(summary(models$`Pre-dawn`))
  cat(summary_prewritten, sep = "\n")
  
  cat("\nType III ANOVA:\n")
  print(Anova(models$`Pre-dawn`, type = 3))
  
  # Print Midday results
  cat("\nMIDDAY RESULTS\n")
  cat("--------------\n")
  print(summary(models$Midday))
  cat("\nType III ANOVA:\n")
  print(Anova(models$Midday, type = 3))
  
  # Create separate tables for coefficients
  cat("\nModel Coefficients\n")
  cat("----------------\n")
  print(coef(summary(models$`Pre-dawn`)))
  print(coef(summary(models$Midday)))
  
  sink()
}

# Function to print output locations
print_output_locations <- function() {
  message("\nAnalysis complete! Files have been saved to:")
  message("\nFigures:")
  message("- figures/seasonal_predawn.png")
  message("- figures/seasonal_midday.png")
  message("- figures/temperature_predawn.png")
  message("- figures/temperature_midday.png")
  message("- figures/treatment_boxplot.png")
  message("- figures/heatwave_boxplot.png")
  message("\nTables:")
  message("- tables/water_potential_summary.png")
  message("- tables/statistical_results.txt")
}

