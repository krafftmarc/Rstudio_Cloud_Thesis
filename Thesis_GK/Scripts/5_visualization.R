# Functions for creating plots and visualizations
# Plot treatment effects
plot_treatment_effects <- function(data) {
  # Create individual plots
  plots <- list(
    photosynthesis = create_treatment_plot(data, "A", "Net Photosynthesis",
                                           expression(A~(μmol~m^-2~s^-1))),
    transpiration = create_treatment_plot(data, "E", "Transpiration",
                                          expression(E~(mol~m^-2~s^-1))),
    conductance = create_treatment_plot(data, "gsw", "Stomatal Conductance",
                                        expression(g[sw]~(mol~m^-2~s^-1)))
  )
  
  # Combine plots
  combined_plot <- ggarrange(plots$photosynthesis, 
                             plots$transpiration,
                             plots$conductance,
                             ncol = 1,
                             common.legend = TRUE,
                             legend = "bottom")
  
  return(combined_plot)
}

# Helper function to create individual treatment plots
create_treatment_plot <- function(data, var, title, ylab) {
  ggplot(data, aes_string(x = "treatment", y = var)) +
    geom_boxplot(aes(fill = treatment)) +
    facet_grid(stress_level ~ measurement_period) +
    theme_bw() +
    labs(title = title,
         y = ylab,
         x = "Treatment",
         fill = "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# Plot VPD response
plot_vpd_response <- function(data) {
  ggplot(data, aes(x = VPDleaf, y = A)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    geom_smooth(aes(color = treatment), method = "lm") +
    facet_grid(stress_level ~ measurement_period) +
    theme_bw() +
    labs(title = "VPD Response of Net Photosynthesis",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(A~(μmol~m^-2~s^-1)),
         color = "Treatment") +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# Plot WUEi
plot_WUEi <- function(data) {
  # Calculate WUEi
  data_with_wuei <- data %>%
    mutate(WUEi = A/gsw)
  
  ggplot(data_with_wuei, aes(x = treatment, y = WUEi)) +
    geom_boxplot(aes(fill = treatment)) +
    facet_grid(stress_level ~ measurement_period) +
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
         aes(x = treatment, y = A_mean)) +
    geom_bar(stat = "identity", aes(fill = treatment), position = "dodge") +
    geom_errorbar(aes(ymin = A_mean - A_se, 
                      ymax = A_mean + A_se),
                  position = position_dodge(0.9), 
                  width = 0.2) +
    facet_grid(stress_level ~ measurement_period) +
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