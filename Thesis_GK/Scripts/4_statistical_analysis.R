# 4_statistical_analysis.R

# Print function needs to be defined before it's used
print_model_summaries <- function(results) {
  log_message("\nModel Summaries:")
  log_message("--------------")
  
  for (var in names(results)) {
    log_message(paste("\nVariable:", var))
    log_message("Type III ANOVA with interactions:")
    print(results[[var]]$anova)
    
    log_message("\nEstimated Marginal Means by Treatment × Stress × Variety:")
    print(results[[var]]$emm)
    
    log_message("\nPairwise Comparisons:")
    print(results[[var]]$contrasts)
  }
}

# Update calculate_basic_stats function to handle leaf_temp properly
calculate_basic_stats <- function(data) {
  data %>%
    group_by(year, treatment, variety, stress_level) %>%
    summarise(
      across(
        c(photosynthesis, transpiration, conductance, WUEi, leaf_temp, co2),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          se = ~sd(.x, na.rm = TRUE) / sqrt(n())
        )
      ),
      n = n(),
      .groups = 'drop'
    )
}

# Function to run statistical models with interactions
run_mixed_models <- function(data) {
  # Initial diagnostics
  log_message("\nInitial Data Diagnostics:")
  log_message("----------------------")
  log_message(paste("Total rows:", nrow(data)))
  log_message("\nMissing values by column:")
  print(sapply(data, function(x) sum(is.na(x))))
  
  # Scale variables
  data_scaled <- data %>%
    group_by(year) %>%
    mutate(
      vpd_leaf_scaled = if(all(is.na(vpd))) NA else scale(vpd),
      transpiration_scaled = if(all(is.na(transpiration))) NA else scale(transpiration),
      photosynthesis_scaled = if(all(is.na(photosynthesis))) NA else scale(photosynthesis),
      conductance_scaled = if(all(is.na(conductance))) NA else scale(conductance)
    ) %>%
    ungroup()
  
  # Update factors including variety and stress
  data_scaled <- data_scaled %>%
    mutate(
      treatment = factor(treatment, levels = c("2L", "4L")),
      year = factor(year),
      stress_level = factor(stress_level),
      variety = factor(variety)
    )
  
  # Define and fit models with full interactions
  models <- list()
  
  # Photosynthesis model
  models$photosynthesis <- tryCatch({
    fit <- lm(photosynthesis_scaled ~ treatment * stress_level * variety * year, 
              data = data_scaled)
    list(
      fit = fit,
      anova = Anova(fit, type = 3),
      emm = emmeans(fit, specs = ~ treatment | stress_level | variety | year),
      contrasts = pairs(emmeans(fit, specs = ~ treatment | stress_level | variety | year))
    )
  }, error = function(e) {
    log_message(paste("Error fitting photosynthesis model:", e$message))
    NULL
  })
  
  # Transpiration model
  models$transpiration <- tryCatch({
    fit <- lm(transpiration_scaled ~ treatment * stress_level * variety * year, 
              data = data_scaled)
    list(
      fit = fit,
      anova = Anova(fit, type = 3),
      emm = emmeans(fit, specs = ~ treatment | stress_level | variety | year),
      contrasts = pairs(emmeans(fit, specs = ~ treatment | stress_level | variety | year))
    )
  }, error = function(e) {
    log_message(paste("Error fitting transpiration model:", e$message))
    NULL
  })
  
  # Conductance model
  models$conductance <- tryCatch({
    fit <- lm(conductance_scaled ~ treatment * stress_level * variety * year, 
              data = data_scaled)
    list(
      fit = fit,
      anova = Anova(fit, type = 3),
      emm = emmeans(fit, specs = ~ treatment | stress_level | variety | year),
      contrasts = pairs(emmeans(fit, specs = ~ treatment | stress_level | variety | year))
    )
  }, error = function(e) {
    log_message(paste("Error fitting conductance model:", e$message))
    NULL
  })
  
  # Remove NULL models and print summaries
  models <- models[!sapply(models, is.null)]
  if(length(models) == 0) {
    stop("No models could be fitted successfully")
  }
  print_model_summaries(models)
  return(models)
}


# Separate VPD response analysis function
analyze_vpd_response <- function(data) {
  log_message("Beginning VPD response analysis...")
  
  data_scaled <- data %>%
    group_by(year) %>%
    mutate(
      vpd_leaf_scaled = scale(vpd),
      transpiration_scaled = scale(transpiration),
      photosynthesis_scaled = scale(photosynthesis),
      conductance_scaled = scale(conductance),
      treatment = factor(treatment),
      year = factor(year),
      stress_level = factor(stress_level),
      variety = factor(variety)
    ) %>%
    ungroup()
  
  vpd_models <- list()
  
  # VPD models with full interactions
  vpd_models$photosynthesis <- tryCatch({
    fit <- lm(photosynthesis_scaled ~ vpd_leaf_scaled * treatment * stress_level * variety * year, 
              data = data_scaled)
    list(
      fit = fit,
      anova = Anova(fit, type = 3),
      emm = emmeans(fit, specs = ~ treatment | stress_level | variety | year,
                    at = list(vpd_leaf_scaled = c(-1, 0, 1))),
      slopes = emtrends(fit, specs = ~ treatment | stress_level | variety | year, 
                        var = "vpd_leaf_scaled")
    )
  }, error = function(e) {
    log_message(paste("ERROR in photosynthesis VPD model:", e$message))
    NULL
  })
  
  # Add similar models for conductance and transpiration
  
  return(list(
    models = vpd_models,
    data = data_scaled
  ))
}

# Helper function to merge LICOR and CIMIS data
merge_vpd_data <- function(licor_data, cimis_data) {
  merged_data <- licor_data %>%
    left_join(
      cimis_data %>% 
        select(date, VPD, tmax),
      by = "date"
    )
  return(merged_data)
}