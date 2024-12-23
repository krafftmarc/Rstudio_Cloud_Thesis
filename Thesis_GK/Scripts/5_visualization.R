# 5_visualization.R
# Functions for creating plots and visualizations

#------------------------------------------------------------------------------
# Helper Functions - ADD THESE NEW FUNCTIONS AT THE TOP
#------------------------------------------------------------------------------

# Enhanced theme function for consistent plotting
create_enhanced_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size - 1, hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = base_size - 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}

# Color palette function
get_treatment_colors <- function(treatment_levels) {
  color_map <- c(
    "2L" = "#1f77b4",  # Blue
    "4L" = "#2ca02c",  # Green
    "4L (Pre-treatment)" = "#ff7f0e"  # Orange
  )
  return(color_map[treatment_levels])
}

# Data validation function
validate_plot_data <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  na_cols <- sapply(data[required_cols], function(x) all(is.na(x)))
  if (any(na_cols)) {
    stop(paste("Columns contain all NA values:", 
               paste(names(na_cols)[na_cols], collapse = ", ")))
  }
  return(TRUE)
}

# Outlier handling function
handle_outliers <- function(data, var, bounds) {
  if (var %in% names(data) && all(c("lower", "upper") %in% names(bounds))) {
    outliers <- data[[var]] < bounds$lower | data[[var]] > bounds$upper
    if (any(outliers, na.rm = TRUE)) {
      warning(sprintf("%d outliers found in %s (%.1f%%)", 
                      sum(outliers, na.rm = TRUE), 
                      var, 
                      100 * mean(outliers, na.rm = TRUE)))
    }
    data[[var]][outliers] <- NA
  }
  return(data)
}


# Enhanced plot_treatment_effects function
plot_treatment_effects <- function(data) {
  # Print data structure for debugging
  message("Data structure for plotting:")
  str(data)
  
  # Validate input data with expanded required columns
  required_cols <- c("treatment", "photosynthesis", "transpiration", "conductance", 
                     "WUEi", "stress_level", "year", "variety", "leaf_temp")
  validate_plot_data(data, required_cols)
  
  # Define physiological bounds
  bounds <- list(
    photosynthesis = list(lower = -5, upper = 30),
    transpiration = list(lower = 0, upper = 0.02),
    conductance = list(lower = 0, upper = 1),
    WUEi = list(lower = 0, upper = 200)
  )
  
  # Handle outliers with improved error reporting
  message("Handling outliers...")
  tryCatch({
    data <- lapply(names(bounds), function(var) {
      handle_outliers(data, var, bounds[[var]])
    }) %>% bind_rows()
  }, error = function(e) {
    stop(paste("Error handling outliers:", e$message))
  })
  
  # Set up theme and colors
  message("Setting up theme and colors...")
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  # Print treatment levels for debugging
  message("Treatment levels found:")
  print(unique(data$treatment))
  
  # Create individual plots with error handling
  message("Creating individual plots...")
  
  plots <- tryCatch({
    # Photosynthesis plot
    plot_physiology <- ggplot(data, aes(x = treatment)) +
      geom_boxplot(aes(y = photosynthesis, fill = treatment),
                   outlier.shape = 21, outlier.alpha = 0.5) +
      scale_fill_manual(values = treatment_colors) +
      facet_grid(stress_level ~ year + variety) +
      labs(title = "Net Photosynthesis by Treatment",
           y = expression(A[net]~"("*mu*"mol m"^-2~"s"^-1*")")) +
      my_theme
    
    # Transpiration plot
    plot_transpiration <- ggplot(data, aes(x = treatment)) +
      geom_boxplot(aes(y = transpiration, fill = treatment),
                   outlier.shape = 21, outlier.alpha = 0.5) +
      scale_fill_manual(values = treatment_colors) +
      facet_grid(stress_level ~ year + variety) +
      labs(title = "Transpiration by Treatment",
           y = expression(E~"(mmol m"^-2~"s"^-1*")")) +
      my_theme
    
    # Conductance plot
    plot_conductance <- ggplot(data, aes(x = treatment)) +
      geom_boxplot(aes(y = conductance, fill = treatment),
                   outlier.shape = 21, outlier.alpha = 0.5) +
      scale_fill_manual(values = treatment_colors) +
      facet_grid(stress_level ~ year + variety) +
      labs(title = "Stomatal Conductance by Treatment",
           y = expression(g[sw]~"(mol m"^-2~"s"^-1*")")) +
      my_theme
    
    # WUEi plot
    plot_wuei <- ggplot(data, aes(x = treatment)) +
      geom_boxplot(aes(y = WUEi, fill = treatment),
                   outlier.shape = 21, outlier.alpha = 0.5) +
      scale_fill_manual(values = treatment_colors) +
      facet_grid(stress_level ~ year + variety) +
      labs(title = "Water Use Efficiency by Treatment",
           y = expression(WUE[i]~"("*mu*"mol mol"^-1*")")) +
      my_theme
    
    message("Creating temperature plots by year...")
    
    # Ensure year is properly formatted
    data$year <- as.factor(data$year)
    
    # Separate data by year
    data_2022 <- subset(data, year == "2022")
    data_2023 <- subset(data, year == "2023")
    
    # Temperature plots
    plot_temp_photo_2022 <- ggplot(data_2022, 
                                   aes(x = leaf_temp, y = photosynthesis, color = treatment)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", span = 0.75, se = TRUE) +
      scale_color_manual(values = treatment_colors) +
      facet_grid(stress_level ~ variety) +
      scale_x_continuous(limits = c(25, 45), breaks = seq(25, 45, by = 5)) +
      labs(x = expression("Leaf Temperature ("*degree*"C)"),
           y = expression(A[net]~"("*mu*"mol m"^-2~"s"^-1*")")) +
      my_theme +
      ggtitle("2022")
    
    plot_temp_photo_2023 <- ggplot(data_2023, 
                                   aes(x = leaf_temp, y = photosynthesis, color = treatment)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", span = 0.75, se = TRUE) +
      scale_color_manual(values = treatment_colors) +
      facet_grid(stress_level ~ variety) +
      scale_x_continuous(limits = c(25, 40), breaks = seq(25, 40, by = 5)) +
      labs(x = expression("Leaf Temperature ("*degree*"C)"),
           y = expression(A[net]~"("*mu*"mol m"^-2~"s"^-1*")")) +
      my_theme +
      ggtitle("2023")
    
    list(
      plot_physiology = plot_physiology,
      plot_transpiration = plot_transpiration,
      plot_conductance = plot_conductance,
      plot_wuei = plot_wuei,
      plot_temp_2022 = plot_temp_photo_2022,
      plot_temp_2023 = plot_temp_photo_2023
    )
  }, error = function(e) {
    stop(paste("Error creating plots:", e$message))
  })
  
  message("Combining plots...")
  
  # Combine plots using patchwork
  combined_plot <- (plots$plot_physiology + plots$plot_transpiration) / 
    (plots$plot_conductance + plots$plot_wuei) /
    (plots$plot_temp_2022 + plots$plot_temp_2023) +
    plot_annotation(
      title = "Gas Exchange Parameters by Treatment, Variety, and Stress Level",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  # Return both individual plots and combined plot
  return(list(
    combined = combined_plot,
    individual = plots
  ))
}


# Fix VPD response plots function
plot_vpd_response <- function(data) {
  # Validate input data
  required_cols <- c("vpd", "photosynthesis", "conductance", "transpiration", 
                     "treatment", "stress_level", "year")
  validate_plot_data(data, required_cols)
  
  # Ensure proper data types
  data$year <- as.factor(data$year)
  data$treatment <- as.factor(data$treatment)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  # Define physiological bounds
  bounds <- list(
    photosynthesis = list(lower = -5, upper = 30),
    transpiration = list(lower = 0, upper = 0.02),
    conductance = list(lower = 0, upper = 1),
    vpd = list(lower = 0, upper = 10)
  )
  
  # Handle outliers
  data <- lapply(names(bounds), function(var) {
    handle_outliers(data, var, bounds[[var]])
  }) %>% bind_rows()
  
  # Create VPD response plots
  p_photo <- ggplot(data, aes(x = vpd, y = photosynthesis)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "loess", span = 0.75) +
    scale_color_manual(values = treatment_colors) +
    facet_grid(stress_level ~ year) +
    labs(title = "VPD Response - Photosynthesis",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(A~(μmol~m^-2~s^-1)),
         color = "Treatment") +
    my_theme
  
  p_cond <- ggplot(data, aes(x = vpd, y = conductance)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "loess", span = 0.75) +
    scale_color_manual(values = treatment_colors) +
    facet_grid(stress_level ~ year) +
    labs(title = "VPD Response - Conductance",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(g[sw]~(mol~m^-2~s^-1)),
         color = "Treatment") +
    my_theme
  
  p_trans <- ggplot(data, aes(x = vpd, y = transpiration)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "loess", span = 0.75) +
    scale_color_manual(values = treatment_colors) +
    facet_grid(stress_level ~ year) +
    labs(title = "VPD Response - Transpiration",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(E~(mmol~m^-2~s^-1)),
         color = "Treatment") +
    my_theme
  
  # Combine plots
  vpd_plots <- p_photo / p_cond / p_trans +
    plot_annotation(
      title = "VPD Response Curves by Treatment and Stress Level",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  return(vpd_plots)
}

# Updated plot_tleaf_responses function
plot_tleaf_responses <- function(data) {
  # Add validation
  required_cols <- c("year", "treatment", "stress_level", "leaf_temp", "photosynthesis")
  validate_plot_data(data, required_cols)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  # Ensure factors
  data$year <- as.factor(data$year)
  data$treatment <- as.factor(data$treatment)
  data$stress_level <- as.factor(data$stress_level)
  
  # Calculate full interaction model stats
  interaction_model <- lm(photosynthesis ~ leaf_temp * treatment * stress_level, data = data)
  model_summary <- Anova(interaction_model, type = 3)
  
  # Extract interaction p-values
  tx_stress_p <- model_summary["treatment:stress_level", "Pr(>F)"]
  tx_temp_p <- model_summary["leaf_temp:treatment", "Pr(>F)"]
  stress_temp_p <- model_summary["leaf_temp:stress_level", "Pr(>F)"]
  three_way_p <- model_summary["leaf_temp:treatment:stress_level", "Pr(>F)"]
  
  # Create interaction stats label with improved formatting
  interaction_label <- sprintf(
    "Interactions:\nTx × Stress: p = %.3f\nTx × Temp: p = %.3f\nStress × Temp: p = %.3f\nThree-way: p = %.3f",
    tx_stress_p, tx_temp_p, stress_temp_p, three_way_p
  )
  
  # Plot with improved aesthetics
  p_interaction <- ggplot(data, aes(x = leaf_temp, y = photosynthesis, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", span = 0.75, se = TRUE) +
    scale_color_manual(values = treatment_colors) +
    facet_wrap(~year) +
    my_theme +
    labs(title = "Leaf Temperature Response by Treatment and Stress Level",
         x = "Leaf Temperature (°C)",
         y = expression(A[net]~(μmol~m^-2~s^-1)),
         color = "Treatment",
         linetype = "Stress Level") +
    annotate("text", x = -Inf, y = Inf,
             label = interaction_label,
             hjust = -0.1, vjust = 1.5)
  
  return(list(
    plot = p_interaction,
    stats = list(
      model = interaction_model,
      summary = model_summary,
      interactions = list(
        tx_stress = tx_stress_p,
        tx_temp = tx_temp_p,
        stress_temp = stress_temp_p,
        three_way = three_way_p
      )
    )
  ))
}

# Updated plot_tmax_interactions function
plot_tmax_interactions <- function(data) {
  # Add validation
  required_cols <- c("tmax", "photosynthesis", "conductance", "transpiration",
                     "treatment", "stress_level", "year")
  validate_plot_data(data, required_cols)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  # Calculate models for each response variable
  models <- list(
    photo = lm(photosynthesis ~ tmax * treatment * stress_level, data = data),
    cond = lm(conductance ~ tmax * treatment * stress_level, data = data),
    trans = lm(transpiration ~ tmax * treatment * stress_level, data = data)
  )
  
  # Get stats for each model
  stats <- lapply(models, function(model) {
    summary <- Anova(model, type = 3)
    list(
      tx_stress = summary["treatment:stress_level", "Pr(>F)"],
      tx_temp = summary["tmax:treatment", "Pr(>F)"],
      stress_temp = summary["tmax:stress_level", "Pr(>F)"],
      three_way = summary["tmax:treatment:stress_level", "Pr(>F)"]
    )
  })
  
  # Create plots with improved aesthetics
  p_photo <- ggplot(data, aes(x = tmax, y = photosynthesis, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", span = 0.75, se = TRUE) +
    scale_color_manual(values = treatment_colors) +
    facet_wrap(~year) +
    my_theme +
    labs(title = "Temperature Response - Photosynthesis",
         subtitle = sprintf("Tx × Stress: p = %.3f, Three-way: p = %.3f", 
                            stats$photo$tx_stress, stats$photo$three_way),
         x = "Maximum Temperature (°C)",
         y = expression(A[net]~(μmol~m^-2~s^-1)))
  
  p_cond <- ggplot(data, aes(x = tmax, y = conductance, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", span = 0.75, se = TRUE) +
    scale_color_manual(values = treatment_colors) +
    facet_wrap(~year) +
    my_theme +
    labs(title = "Temperature Response - Conductance",
         subtitle = sprintf("Tx × Stress: p = %.3f, Three-way: p = %.3f", 
                            stats$cond$tx_stress, stats$cond$three_way),
         x = "Maximum Temperature (°C)",
         y = expression(g[sw]~(mol~m^-2~s^-1)))
  
  p_trans <- ggplot(data, aes(x = tmax, y = transpiration, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", span = 0.75, se = TRUE) +
    scale_color_manual(values = treatment_colors) +
    facet_wrap(~year) +
    my_theme +
    labs(title = "Temperature Response - Transpiration",
         subtitle = sprintf("Tx × Stress: p = %.3f, Three-way: p = %.3f", 
                            stats$trans$tx_stress, stats$trans$three_way),
         x = "Maximum Temperature (°C)",
         y = expression(E~(mmol~m^-2~s^-1)))
  
  # Combine plots with improved layout
  combined_plots <- p_photo / p_cond / p_trans +
    plot_annotation(
      title = "Gas Exchange Responses to Temperature with Treatment × Stress Interactions",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  return(list(
    plot = combined_plots,
    plots = list(photo = p_photo, cond = p_cond, trans = p_trans),
    stats = stats
  ))
}

# Plot WUEi
plot_WUEi <- function(data) {
  # Add validation
  required_cols <- c("treatment", "WUEi", "year")
  validate_plot_data(data, required_cols)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  ggplot(data, aes(x = treatment, y = WUEi)) +
    geom_boxplot(aes(fill = treatment), outlier.shape = 21, outlier.alpha = 0.5) +
    scale_fill_manual(values = treatment_colors) +
    facet_wrap(~year) +
    my_theme +
    labs(title = "Intrinsic Water Use Efficiency (WUEi)",
         y = expression(WUE[i]~(μmol~CO[2]~mol^-1~H[2]*O)),
         x = "Treatment",
         fill = "Treatment")
}

# Updated plot_annual_comparison function
plot_annual_comparison <- function(stats_data) {
  # Add validation
  required_cols <- c("treatment", "photosynthesis_mean", "photosynthesis_se", "year")
  validate_plot_data(stats_data, required_cols)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(stats_data$treatment))
  
  ggplot(stats_data, aes(x = treatment, y = photosynthesis_mean)) +
    geom_bar(stat = "identity", aes(fill = treatment), position = "dodge") +
    geom_errorbar(aes(ymin = photosynthesis_mean - photosynthesis_se, 
                      ymax = photosynthesis_mean + photosynthesis_se),
                  position = position_dodge(0.9), 
                  width = 0.2) +
    scale_fill_manual(values = treatment_colors) +
    facet_wrap(~year) +
    my_theme +
    labs(title = "Net Photosynthesis by Treatment",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Treatment",
         fill = "Treatment")
}

# Updated plot_diurnal_patterns function
plot_diurnal_patterns <- function(data) {
  # Add validation
  required_cols <- c("measurement_period", "A", "treatment", "stress_level")
  validate_plot_data(data, required_cols)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  ggplot(data, aes(x = measurement_period, y = A)) +
    geom_boxplot(aes(fill = treatment), outlier.shape = 21, outlier.alpha = 0.5) +
    scale_fill_manual(values = treatment_colors) +
    facet_wrap(~stress_level) +
    my_theme +
    labs(title = "Diurnal Patterns of Net Photosynthesis",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Measurement Period",
         fill = "Treatment")
}

# Updated plot_stress_response function
plot_stress_response <- function(data) {
  # Add validation
  required_cols <- c("stress_level", "A", "treatment", "measurement_period")
  validate_plot_data(data, required_cols)
  
  # Set up theme and colors
  my_theme <- create_enhanced_theme()
  treatment_colors <- get_treatment_colors(unique(data$treatment))
  
  ggplot(data, aes(x = stress_level, y = A)) +
    geom_boxplot(aes(fill = treatment), outlier.shape = 21, outlier.alpha = 0.5) +
    scale_fill_manual(values = treatment_colors) +
    facet_wrap(~measurement_period) +
    my_theme +
    labs(title = "Stress Response of Net Photosynthesis",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Stress Level",
         fill = "Treatment")
}

# Add to 5_visualization.R

plot_treatment_stress_comparison <- function(data) {
  # Split data by year and variety
  data_splits <- split(data, list(data$year, data$variety))
  
  # Create plots for each combination
  plots <- lapply(names(data_splits), function(split_name) {
    split_data <- data_splits[[split_name]]
    year_var <- strsplit(split_name, "\\.")[[1]]
    
    # Treatment comparison
    treatment_model <- lm(photosynthesis ~ leaf_temp * treatment, data = split_data)
    treatment_r2 <- summary(treatment_model)$r.squared
    treatment_p <- anova(treatment_model)$"Pr(>F)"[3]  # interaction term p-value
    
    p1 <- ggplot(split_data, aes(x = leaf_temp, y = photosynthesis, color = treatment)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = TRUE) +
      labs(title = paste(year_var[1], year_var[2], "- Treatment Comparison"),
           subtitle = sprintf("R² = %.2f, p = %.6f", treatment_r2, treatment_p),
           x = "Leaf Temperature (TleafEB)",
           y = "Photosynthetic Rate (A)") +
      theme_bw()
    
    # Stress level comparison
    stress_model <- lm(photosynthesis ~ leaf_temp * stress_level, data = split_data)
    stress_r2 <- summary(stress_model)$r.squared
    stress_p <- anova(stress_model)$"Pr(>F)"[3]  # interaction term p-value
    
    p2 <- ggplot(split_data, aes(x = leaf_temp, y = photosynthesis, color = stress_level)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = TRUE) +
      labs(title = paste(year_var[1], year_var[2], "- Stress Level Comparison"),
           subtitle = sprintf("R² = %.2f, p = %.6f", stress_r2, stress_p),
           x = "Leaf Temperature (TleafEB)",
           y = "Photosynthetic Rate (A)") +
      theme_bw()
    
    # Return both plots
    list(treatment = p1, stress = p2)
  })
  
  # Combine all plots using patchwork
  combined_plots <- wrap_plots(unlist(plots, recursive = FALSE),
                               ncol = 2,
                               guides = "collect") +
    plot_annotation(
      title = "Comparison of Photosynthetic Rate and Leaf Temperature",
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  return(combined_plots)
}