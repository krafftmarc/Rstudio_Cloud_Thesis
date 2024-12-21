# 5_visualization.R
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
  ggplot(data, aes_string(x = "treatment", y = var, fill = "factor(year)")) +
    geom_boxplot(position = position_dodge(0.8)) +
    theme_bw() +
    labs(title = title,
         y = ylab,
         x = "Treatment",
         fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot VPD response
plot_vpd_response <- function(data) {
  ggplot(data, aes(x = VPDleaf, y = A, 
                   color = treatment, 
                   linetype = factor(year))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(title = "VPD Response of Net Photosynthesis by Year",
         x = "Leaf-to-air VPD (kPa)",
         y = expression(A~(μmol~m^-2~s^-1)),
         linetype = "Year")
}

# Create annual comparison plot
plot_annual_comparison <- function(stats_data) {
  ggplot(stats_data, 
         aes(x = treatment, y = A_mean, fill = factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = A_mean - A_se, 
                      ymax = A_mean + A_se),
                  position = position_dodge(0.9), 
                  width = 0.2) +
    theme_bw() +
    labs(title = "Annual Comparison of Net Photosynthesis",
         y = expression(A~(μmol~m^-2~s^-1)),
         x = "Treatment",
         fill = "Year")
}