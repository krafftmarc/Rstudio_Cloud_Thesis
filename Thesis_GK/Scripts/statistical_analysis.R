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

# Update statistical analysis function
perform_statistical_analysis <- function(combined_data) {
  # Create output directories
  create_stat_dirs()
  
  # Basic Analysis ----------------------------------------
  basic_stats <- combined_data %>%
    group_by(Season, Variety, Tx, Time_of_Day) %>%
    summarise(
      n = n(),
      mean_psi = mean(PSI, na.rm = TRUE),
      sd_psi = sd(PSI, na.rm = TRUE),
      se_psi = sd_psi / sqrt(n),
      .groups = 'drop'
    )
  
  # Basic plot
  basic_plot <- ggplot(combined_data, aes(x = Tx, y = PSI, fill = Variety)) +
    geom_boxplot(alpha = 0.7) +
    facet_grid(Time_of_Day ~ Season) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Water Potential by Treatment, Variety, and Time of Day",
      x = "Treatment",
      y = "Water Potential (MPa)"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12)
    )
  
  # Save basic plot
  ggsave("figures/basic_analysis.png", basic_plot, width = 10, height = 8)
  
  # Complex Analysis ----------------------------------------
  models <- list()
  anova_results <- list()
  
  for(time in c("Pre-dawn", "Midday")) {
    message(paste("Fitting model for", time))
    subset_data <- combined_data %>% 
      filter(Time_of_Day == time) %>%
      # Convert Tx to factor to avoid rank deficiency
      mutate(
        Tx = factor(Tx),
        Season = factor(Season),
        Variety = factor(Variety)
      )
    
    # Fit simpler model
    model <- lmer(PSI ~ Tx + Variety + Season + (1|Block_ID), 
                  data = subset_data,
                  control = lmerControl(check.nobs.vs.nlev = "ignore",
                                        check.nobs.vs.rankZ = "ignore",
                                        check.nobs.vs.nRE = "ignore"))
    
    models[[time]] <- model
    anova_results[[time]] <- Anova(model, type = 3)
  }
  
  # Create interaction plot
  interaction_plot <- ggplot(basic_stats, 
                             aes(x = Tx, y = mean_psi, color = Variety)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1, aes(group = Variety)) +
    geom_errorbar(
      aes(ymin = mean_psi - se_psi, ymax = mean_psi + se_psi),
      width = 0.2
    ) +
    facet_grid(Time_of_Day ~ Season) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Treatment and Variety Effects on Water Potential",
      x = "Treatment",
      y = "Mean Water Potential (MPa) ± SE"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12)
    )
  
  # Save interaction plot
  ggsave("figures/complex_analysis.png", interaction_plot, width = 12, height = 8)
  
  # Save statistical results to text file
  sink("tables/statistical_analysis.txt")
  
  cat("STATISTICAL ANALYSIS RESULTS\n")
  cat("===========================\n\n")
  
  # Basic Statistics
  cat("1. BASIC STATISTICS\n")
  cat("------------------\n")
  print(basic_stats)
  cat("\n\n")
  
  # Complex Analysis Results
  cat("2. MIXED EFFECTS MODEL RESULTS\n")
  cat("----------------------------\n")
  
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
  
  # Create and save GT table
  basic_stats_table <- basic_stats %>%
    gt() %>%
    fmt_number(
      columns = c(mean_psi, sd_psi, se_psi),
      decimals = 2
    ) %>%
    tab_header(
      title = "Water Potential Summary Statistics",
      subtitle = "By Treatment, Variety, and Time of Day"
    )
  
  gtsave(basic_stats_table, "tables/summary_statistics.html")
  
  message("Statistical analysis completed successfully!")
  
  return(list(
    basic_stats = basic_stats,
    models = models,
    anova_results = anova_results
  ))
}