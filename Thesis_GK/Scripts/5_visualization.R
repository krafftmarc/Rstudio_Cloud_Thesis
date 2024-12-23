# 5_visualization.R
# Functions for creating plots and visualizations

# Function to create treatment effect plots
plot_treatment_effects <- function(data) {
  # Create individual plots for each response variable
  plot_physiology <- ggplot(data, aes(x = treatment)) +
    geom_boxplot(aes(y = photosynthesis, fill = treatment)) +
    facet_grid(stress_level ~ year) +  # Update faceting once variety is added
    labs(title = "Net Photosynthesis by Treatment",
         y = expression(A[net]~"("*mu*"mol m"^-2~"s"^-1*")")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_transpiration <- ggplot(data, aes(x = treatment)) +
    geom_boxplot(aes(y = transpiration, fill = treatment)) +
    facet_grid(stress_level ~ year) +
    labs(title = "Transpiration by Treatment",
         y = expression(E~"(mmol m"^-2~"s"^-1*")")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_conductance <- ggplot(data, aes(x = treatment)) +
    geom_boxplot(aes(y = conductance, fill = treatment)) +
    facet_grid(stress_level ~ year) +
    labs(title = "Stomatal Conductance by Treatment",
         y = expression(g[sw]~"(mol m"^-2~"s"^-1*")")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_wuei <- ggplot(data, aes(x = treatment)) +
    geom_boxplot(aes(y = WUEi, fill = treatment)) +
    facet_grid(stress_level ~ year) +
    labs(title = "Water Use Efficiency by Treatment",
         y = expression(WUE[i]~"("*mu*"mol mol"^-1*")")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # New leaf temperature vs photosynthesis plot
  plot_temp_photo <- ggplot(data, aes(x = leaf_temp, y = photosynthesis, color = treatment)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_grid(stress_level ~ year) +
    labs(title = "Leaf Temperature vs Net Photosynthesis",
         x = expression("Leaf Temperature ("*degree*"C)"),
         y = expression(A[net]~"("*mu*"mol m"^-2~"s"^-1*")")) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Combine plots using patchwork
  combined_plot <- (plot_physiology + plot_transpiration) / 
    (plot_conductance + plot_wuei) /
    plot_temp_photo +
    plot_annotation(
      title = "Gas Exchange Parameters by Treatment and Stress Level",
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  return(combined_plot)
}

# Fix VPD response plots function
plot_vpd_response <- function(data) {
  # Ensure year is a factor
  data$year <- as.factor(data$year)
  
  # Helper function to calculate R2 and p-value for each subplot
  get_model_stats <- function(data, x_var, y_var) {
    model <- lm(as.formula(paste(y_var, "~", x_var, "* treatment")), data = data)
    r2 <- summary(model)$r.squared
    p_val <- anova(model)$"Pr(>F)"[3]  # interaction term p-value
    return(list(r2 = r2, p = p_val))
  }
  
  # Create VPD response plots
  p_photo <- ggplot(data, aes(x = vpd, y = photosynthesis)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "lm") +
    facet_grid(stress_level ~ year) +
    theme_bw() +
    labs(title = "VPD Response - Photosynthesis",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(A~(μmol~m^-2~s^-1)),
         color = "Treatment")
  
  p_cond <- ggplot(data, aes(x = vpd, y = conductance)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "lm") +
    facet_grid(stress_level ~ year) +
    theme_bw() +
    labs(title = "VPD Response - Conductance",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(g[sw]~(mol~m^-2~s^-1)),
         color = "Treatment")
  
  p_trans <- ggplot(data, aes(x = vpd, y = transpiration)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "lm") +
    facet_grid(stress_level ~ year) +
    theme_bw() +
    labs(title = "VPD Response - Transpiration",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(E~(mmol~m^-2~s^-1)),
         color = "Treatment")
  
  # Combine plots using patchwork
  vpd_plots <- p_photo / p_cond / p_trans +
    plot_annotation(
      title = "VPD Response Curves by Treatment and Stress Level",
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  return(vpd_plots)
}

# Plot leaf temperature responses with treatment × stress interactions
plot_tleaf_responses <- function(data) {
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
  
  # Create interaction stats label
  interaction_label <- sprintf(
    "Interactions:\nTx × Stress: p = %.3f\nTx × Temp: p = %.3f\nStress × Temp: p = %.3f\nThree-way: p = %.3f",
    tx_stress_p, tx_temp_p, stress_temp_p, three_way_p
  )
  
  # Plot with all interactions shown
  p_interaction <- ggplot(data, aes(x = leaf_temp, y = photosynthesis, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(~year) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(title = "Leaf Temperature Response by Treatment and Stress Level",
         x = "Leaf Temperature (°C)",
         y = expression(A[net]~(μmol~m^-2~s^-1)),
         color = "Treatment",
         linetype = "Stress Level") +
    annotate("text", x = -Inf, y = Inf,
             label = interaction_label,
             hjust = -0.1, vjust = 1.5)
  
  # Return plot and stats
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

# Plot max temperature responses with treatment × stress interactions
plot_tmax_interactions <- function(data) {
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
  
  # Create plots with interaction labels
  p_photo <- ggplot(data, aes(x = tmax, y = photosynthesis, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(~year) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(title = "Temperature Response - Photosynthesis",
         subtitle = sprintf("Tx × Stress: p = %.3f, Three-way: p = %.3f", 
                            stats$photo$tx_stress, stats$photo$three_way),
         x = "Maximum Temperature (°C)",
         y = expression(A[net]~(μmol~m^-2~s^-1))) +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("Tx × Temp: p = %.3f\nStress × Temp: p = %.3f",
                             stats$photo$tx_temp, stats$photo$stress_temp),
             hjust = -0.1, vjust = 1.5)
  
  # Similar plots for conductance and transpiration
  p_cond <- ggplot(data, aes(x = tmax, y = conductance, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(~year) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(title = "Temperature Response - Conductance",
         subtitle = sprintf("Tx × Stress: p = %.3f, Three-way: p = %.3f", 
                            stats$cond$tx_stress, stats$cond$three_way),
         x = "Maximum Temperature (°C)",
         y = expression(g[sw]~(mol~m^-2~s^-1)))
  
  p_trans <- ggplot(data, aes(x = tmax, y = transpiration, color = treatment, linetype = stress_level)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(~year) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(title = "Temperature Response - Transpiration",
         subtitle = sprintf("Tx × Stress: p = %.3f, Three-way: p = %.3f", 
                            stats$trans$tx_stress, stats$trans$three_way),
         x = "Maximum Temperature (°C)",
         y = expression(E~(mmol~m^-2~s^-1)))
  
  # Combine plots
  combined_plots <- p_photo / p_cond / p_trans +
    plot_annotation(
      title = "Gas Exchange Responses to Temperature with Treatment × Stress Interactions",
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  return(list(
    plot = combined_plots,
    plots = list(photo = p_photo, cond = p_cond, trans = p_trans),
    stats = stats
  ))
}

# Plot WUEi
plot_WUEi <- function(data) {
  # WUEi is already calculated in the data processing step
  ggplot(data, aes(x = treatment, y = WUEi)) +
    geom_boxplot(aes(fill = treatment)) +
    facet_wrap(~year) +
    theme_bw() +
    labs(title = "Intrinsic Water Use Efficiency (WUEi)",
         y = expression(WUE[i]~(μmol~CO[2]~mol^-1~H[2]*O)),
         x = "Treatment",
         fill = "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# Create summary statistics plot
plot_annual_comparison <- function(stats_data) {
  ggplot(stats_data, 
         aes(x = treatment, y = photosynthesis_mean)) +
    geom_bar(stat = "identity", aes(fill = treatment), position = "dodge") +
    geom_errorbar(aes(ymin = photosynthesis_mean - photosynthesis_se, 
                      ymax = photosynthesis_mean + photosynthesis_se),
                  position = position_dodge(0.9), 
                  width = 0.2) +
    facet_wrap(~year) +
    theme_bw() +
    labs(title = "Net Photosynthesis by Treatment",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Treatment",
         fill = "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# Add new plot for diurnal patterns
plot_diurnal_patterns <- function(data) {
  ggplot(data, aes(x = measurement_period, y = A)) +
    geom_boxplot(aes(fill = treatment)) +
    facet_wrap(~stress_level) +
    theme_bw() +
    labs(title = "Diurnal Patterns of Net Photosynthesis",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Measurement Period",
         fill = "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# Add new plot for stress response
plot_stress_response <- function(data) {
  ggplot(data, aes(x = stress_level, y = A)) +
    geom_boxplot(aes(fill = treatment)) +
    facet_wrap(~measurement_period) +
    theme_bw() +
    labs(title = "Stress Response of Net Photosynthesis",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Stress Level",
         fill = "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
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