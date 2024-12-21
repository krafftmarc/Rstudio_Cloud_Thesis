# 4_statistical_analysis.R
# Functions for statistical analysis

# Calculate basic statistics
calculate_basic_stats <- function(data) {
  data %>%
    group_by(year, treatment) %>%
    summarise(
      across(
        c(A, E, gsw, VPDleaf, Ci),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          se = ~sd(.x, na.rm = TRUE)/sqrt(n())
        )
      ),
      n = n()
    )
}

# Run mixed effects models
run_mixed_models <- function(data) {
  # Define models
  models <- list(
    A = lmer(A ~ treatment * year + (1|date), data = data),
    E = lmer(E ~ treatment * year + (1|date), data = data),
    gsw = lmer(gsw ~ treatment * year + (1|date), data = data)
  )
  
  # Calculate statistics for each model
  results <- lapply(models, analyze_model)
  
  return(results)
}

# Helper function to analyze individual models
analyze_model <- function(model) {
  list(
    emm = emmeans(model, specs = c("treatment", "year")),
    pairs = pairs(emmeans(model, specs = c("treatment", "year"))),
    anova = Anova(model, type = "III")
  )
}

# Analyze VPD response
analyze_vpd_response <- function(data) {
  # Create VPD response models
  vpd_models <- list(
    A = lmer(A ~ VPDleaf * treatment * year + (1|date), data = data),
    gsw = lmer(gsw ~ VPDleaf * treatment * year + (1|date), data = data),
    E = lmer(E ~ VPDleaf * treatment * year + (1|date), data = data)
  )
  
  # Analyze each VPD model
  vpd_results <- lapply(vpd_models, analyze_model)
  
  return(list(
    models = vpd_models,
    results = vpd_results
  ))
}