# Save this as /cloud/project/Thesis_GK/Scripts/statistical_analysis.R

# Load required libraries
library(lme4)
library(emmeans)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(multcomp)
library(gt)

# Create directories if they don't exist
create_stat_dirs <- function() {
  if (!dir.exists("figures")) dir.create("figures")
  if (!dir.exists("tables")) dir.create("tables")
}

# Updated statistical analysis function to include VPD
perform_statistical_analysis <- function(combined_data, cimis_data) {
  # Create output directories
  create_stat_dirs()
  
  # Merge water potential and environmental data
  analysis_data <- combined_data %>%
    left_join(cimis_data %>% select(Date, Temp_C, VPD), by = "Date")
  
  message("Checking merged data:")
  message("Rows in combined_data: ", nrow(combined_data))
  message("Rows in analysis_data: ", nrow(analysis_data))
  
  # Basic Analysis with VPD ----------------------------------------
  basic_stats <- analysis_data %>%
    group_by(Season, Variety, Tx, Time_of_Day) %>%
    summarise(
      n = n(),
      mean_psi = mean(PSI, na.rm = TRUE),
      sd_psi = sd(PSI, na.rm = TRUE),
      se_psi = sd_psi / sqrt(n),
      mean_vpd = mean(VPD, na.rm = TRUE),
      sd_vpd = sd(VPD, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Basic plot with VPD
  basic_plot <- ggplot(analysis_data, aes(x = VPD, y = PSI, color = Variety)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm") +
    facet_grid(Time_of_Day ~ Season) +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "Water Potential vs VPD by Variety and Time of Day",
      x = "Vapor Pressure Deficit (kPa)",
      y = "Water Potential (MPa)"
    ) +
    theme_classic()
  
  # Save basic plot
  ggsave("figures/vpd_analysis.png", basic_plot, width = 10, height = 8)
  
  # Complex Analysis with VPD ----------------------------------------
  models <- list()
  anova_results <- list()
  
  for(time in c("Pre-dawn", "Midday")) {
    message(paste("Fitting model for", time))
    subset_data <- analysis_data %>% 
      filter(Time_of_Day == time) %>%
      mutate(
        Tx = factor(Tx),
        Season = factor(Season),
        Variety = factor(Variety)
      )
    
    # Fit model including VPD
    model <- lmer(PSI ~ Tx + Variety + Season + VPD + (1|Block_ID), 
                  data = subset_data,
                  control = lmerControl(check.nobs.vs.nlev = "ignore",
                                        check.nobs.vs.rankZ = "ignore",
                                        check.nobs.vs.nRE = "ignore"))
    
    models[[time]] <- model
    anova_results[[time]] <- Anova(model, type = 3)
  }
  
  # Create VPD response plot
  vpd_plot <- ggplot(analysis_data, 
                     aes(x = VPD, y = PSI, color = Variety, shape = Tx)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_grid(Time_of_Day ~ Season) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "VPD Effects on Water Potential by Treatment and Variety",
      x = "Vapor Pressure Deficit (kPa)",
      y = "Water Potential (MPa)"
    ) +
    theme_classic()
  
  # Save VPD plot
  ggsave("figures/vpd_response.png", vpd_plot, width = 12, height = 8)
  
  # Save statistical results to text file
  sink("tables/statistical_analysis_vpd.txt")
  
  cat("STATISTICAL ANALYSIS RESULTS INCLUDING VPD\n")
  cat("========================================\n\n")
  
  # Basic Statistics
  cat("1. BASIC STATISTICS WITH VPD\n")
  cat("-------------------------\n")
  print(basic_stats)
  cat("\n\n")
  
  # Complex Analysis Results
  cat("2. MIXED EFFECTS MODEL RESULTS (INCLUDING VPD)\n")
  cat("------------------------------------------\n")
  
  for(time in c("Pre-dawn", "Midday")) {
    cat(sprintf("\n%s Analysis:\n", time))
    cat("-------------------\n")
    cat("Type III ANOVA Results:\n")
    print(anova_results[[time]])
    cat("\n")
    cat("Model Summary:\n")
    print(summary(models[[time]]))
    cat("\n")
    
    # Add estimated marginal means
    cat("Estimated Marginal Means:\n")
    print(emmeans(models[[time]], ~ Tx + Variety))
    cat("\n-------------------\n")
  }
  
  sink()
  
  # Create and save GT table with VPD
  basic_stats_table <- basic_stats %>%
    gt() %>%
    fmt_number(
      columns = c(mean_psi, sd_psi, se_psi, mean_vpd, sd_vpd),
      decimals = 2
    ) %>%
    tab_header(
      title = "Water Potential and VPD Summary Statistics",
      subtitle = "By Treatment, Variety, and Time of Day"
    )
  
  gtsave(basic_stats_table, "tables/summary_statistics_vpd.html")
  
  message("Statistical analysis with VPD completed successfully!")
  
  return(list(
    basic_stats = basic_stats,
    models = models,
    anova_results = anova_results
  ))
}