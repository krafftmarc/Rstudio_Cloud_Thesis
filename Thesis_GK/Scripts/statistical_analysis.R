library(lme4)
library(car)
library(MuMIn)

# Add these helper functions after your library imports
calculate_r2 <- function(model) {
  r2 <- MuMIn::r.squaredGLMM(model)
  return(list(
    marginal = r2[1,"R2m"],
    conditional = r2[1,"R2c"]
  ))
}

calculate_pvalues <- function(model) {
  anova_table <- car::Anova(model, type = 3, test.statistic = "Chisq")
  return(anova_table)
}

calculate_r2 <- function(model) {
  var_comp <- as.data.frame(VarCorr(model))
  var_fixed <- sum(var_comp$vcov)
  var_residual <- attr(VarCorr(model), "sc")^2
  
  fixed_effects <- fixef(model)
  fitted_values <- model.matrix(model) %*% fixed_effects
  var_fixed_effects <- var(fitted_values)
  
  R2m <- var_fixed_effects / (var_fixed_effects + var_residual)
  R2c <- (var_fixed_effects + var_fixed) / (var_fixed_effects + var_fixed + var_residual)
  
  return(c(R2m = R2m, R2c = R2c))
}

calculate_pvalues <- function(model, df) {
  t_vals <- summary(model)$coefficients[, "t value"]
  p_vals <- 2 * (1 - pt(abs(t_vals), df))
  names(p_vals) <- rownames(summary(model)$coefficients)
  return(p_vals)
}

create_stat_dirs <- function() {
  if (!dir.exists("figures")) dir.create("figures")
  if (!dir.exists("tables")) dir.create("tables")
}

perform_statistical_analysis <- function(combined_data, cimis_data) {
  create_stat_dirs()
  
  analysis_data <- combined_data %>%
    left_join(cimis_data %>% select(Date, Temp_C, VPD), by = "Date")
  
  models <- list()
  anova_results <- list()
  r2_values <- list()
  p_values_list <- list()
  
  for(time in c("Pre-dawn", "Midday")) {
    subset_data <- analysis_data %>% 
      filter(Time_of_Day == time) %>%
      mutate(Tx = factor(Tx), Season = factor(Season), Variety = factor(Variety))
    
    model <- lmer(PSI ~ Tx + Variety + Season + VPD + (1|Block_ID), 
                  data = subset_data,
                  control = lmerControl(check.nobs.vs.nlev = "ignore"))
    
    if (isSingular(model)) {
      message(sprintf("Warning: %s model fit is singular.", time))
    }
    
    models[[time]] <- model
    anova_results[[time]] <- car::Anova(model, type = 3)
    
    r2_values[[time]] <- tryCatch({
      MuMIn::r.squaredGLMM(model)
    }, error = function(e) {
      return(c(R2m = NA, R2c = NA))
    })
    
    if (any(is.na(r2_values[[time]]))) {
      r2_values[[time]] <- calculate_r2(model)
    }
    
    p_values_list[[time]] <- calculate_pvalues(model, df = nrow(subset_data) - length(fixef(model)))
  }
  
  sink("tables/statistical_analysis_detailed.txt")
  cat("STATISTICAL ANALYSIS RESULTS INCLUDING VPD\n")
  cat("========================================\n\n")
  
  # In your perform_statistical_analysis function, replace the results printing section with:
  for(time in c("Pre-dawn", "Midday")) {
    cat(sprintf("\n%s Analysis:\n", time))
    cat("================== \n\n")
    
    # Model Summary
    cat("Model Summary:\n")
    cat("--------------\n")
    print(summary(models[[time]]))
    cat("\n")
    
    # Calculate and print R² values
    r2_stats <- calculate_r2(models[[time]])
    cat("Model R² Values:\n")
    cat("--------------\n")
    cat(sprintf("Marginal R² (fixed effects only): %.3f\n", r2_stats$marginal))
    cat(sprintf("Conditional R² (total): %.3f\n", r2_stats$conditional))
    cat("\n")
    
    # Calculate and print Type III tests with p-values
    cat("Type III Analysis of Deviance (Wald Chi-square tests):\n")
    cat("------------------------------------------------\n")
    anova_results <- calculate_pvalues(models[[time]])
    print(anova_results)
    cat("\n-------------------\n")
  }
  sink()
  
  return(list(models = models, anova_results = anova_results, r2_values = r2_values, p_values = p_values_list))
}
