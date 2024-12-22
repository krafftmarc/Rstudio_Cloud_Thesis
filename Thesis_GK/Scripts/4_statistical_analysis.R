# 4_statistical_analysis.R
# Functions for statistical analysis

# Calculate basic statistics with updated variable names
calculate_basic_stats <- function(data) {
  data %>%
    group_by(year, treatment) %>%
    summarise(
      across(
        # Updated to use TleafEB instead of Tleaf
        c(A, E, gsw, WUEi, TleafEB, Ci, Pci),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          se = ~sd(.x, na.rm = TRUE) / sqrt(n())
        )
      ),
      n = n(),
      .groups = 'drop'  # Explicitly drop grouping
    )
}


# Function to run mixed effects models with diagnostics
run_mixed_models <- function(data) {
  # Print initial diagnostics
  message("\nInitial Data Diagnostics:")
  message("----------------------")
  message("Total rows: ", nrow(data))
  message("\nMissing values by column:")
  print(sapply(data, function(x) sum(is.na(x))))
  
  # First standardize numeric variables with NA handling
  data_scaled <- data %>%
    group_by(year) %>%
    mutate(
      VPDleaf_scaled = if(all(is.na(VPDleaf))) NA else scale(VPDleaf),
      E_scaled = if(all(is.na(E))) NA else scale(E),
      A_scaled = if(all(is.na(A))) NA else scale(A),
      gsw_scaled = if(all(is.na(gsw))) NA else scale(gsw)
    ) %>%
    ungroup()
  
  # Print diagnostics after scaling
  message("\nAfter scaling - Missing values in scaled variables:")
  print(sapply(data_scaled[c("VPDleaf_scaled", "E_scaled", "A_scaled", "gsw_scaled")], 
               function(x) sum(is.na(x))))
  
  # Update treatment factor levels
  data_scaled <- data_scaled %>%
    mutate(
      treatment = factor(treatment, 
                         levels = c("Baseline (1x4L)", "Double 4L", "Double 2L")),
      year = factor(year),
      stress_level = factor(stress_level),
      treatment_year = interaction(treatment, year)
    )
  
  # Print model structure diagnostics
  message("\nModel Structure Diagnostics:")
  message("---------------------------")
  message("Treatment levels: ", paste(levels(data_scaled$treatment), collapse = ", "))
  message("Year levels: ", paste(levels(data_scaled$year), collapse = ", "))
  message("\nTreatment x Year combinations:")
  print(table(data_scaled$treatment_year))
  
  # Check complete cases by variable combination
  vars_needed <- c("A_scaled", "E_scaled", "gsw_scaled", "treatment", "year", "date")
  complete_cases <- complete.cases(data_scaled[vars_needed])
  
  message("\nComplete cases analysis:")
  message("Total complete cases: ", sum(complete_cases))
  message("Missing cases: ", sum(!complete_cases))
  
  if(sum(complete_cases) == 0) {
    message("\nDetailed missing value analysis:")
    for(var in vars_needed) {
      message(var, ": ", sum(is.na(data_scaled[[var]])), " missing values")
    }
    stop("No complete cases available for analysis")
  }
  
  # Subset to complete cases for modeling
  data_model <- data_scaled[complete_cases, ]
  
  message("\nSample sizes by treatment and year in model data:")
  print(table(data_model$treatment, data_model$year))
  
  # Define models
  message("\nFitting models...")
  
  models <- list(
    A = tryCatch({
      lmer(A_scaled ~ treatment * year + (1|date), 
           data = data_model,
           control = lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.nRE = "ignore"))
    }, error = function(e) {
      message("Error fitting A model: ", e$message)
      return(NULL)
    }),
    
    E = tryCatch({
      lmer(E_scaled ~ treatment * year + (1|date), 
           data = data_model,
           control = lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.nRE = "ignore"))
    }, error = function(e) {
      message("Error fitting E model: ", e$message)
      return(NULL)
    }),
    
    gsw = tryCatch({
      lmer(gsw_scaled ~ treatment * year + (1|date), 
           data = data_model,
           control = lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.nRE = "ignore"))
    }, error = function(e) {
      message("Error fitting gsw model: ", e$message)
      return(NULL)
    })
  )
  
  # Remove NULL models
  models <- models[!sapply(models, is.null)]
  
  if(length(models) == 0) {
    stop("No models could be fitted successfully")
  }
  
  # Calculate statistics for each successful model
  results <- lapply(models, function(model) {
    # Get variance components
    vc <- as.data.frame(VarCorr(model))
    
    list(
      summary = summary(model),
      anova = anova(model, ddf = "Kenward-Roger"),
      emm = emmeans(model, specs = pairwise ~ treatment * year,
                    adjust = "tukey"),
      ranef = ranef(model),
      var_components = vc,
      diagnostics = list(
        residuals = residuals(model),
        fitted = fitted(model)
      )
    )
  })
  
  print_model_summaries(results)
  
  return(results)
}
# Helper function to print summary tables
print_model_summaries <- function(results) {
  message("\nModel Summaries:")
  message("--------------")
  
  for (var in names(results)) {
    message("\nVariable: ", var)
    message("Fixed effects:")
    print(results[[var]]$anova)
    
    message("\nEstimated Marginal Means:")
    print(results[[var]]$emm$emmeans)
    
    message("\nPairwise Comparisons:")
    print(results[[var]]$emm$contrasts)
  }
}

# Function to analyze VPD response with scaled variables
analyze_vpd_response <- function(data) {
  # Scale numeric predictors
  data_scaled <- data %>%
    group_by(year) %>%
    mutate(
      VPDleaf_scaled = scale(VPDleaf),
      E_scaled = scale(E),
      A_scaled = scale(A),
      gsw_scaled = scale(gsw)
    ) %>%
    ungroup()
  
  # Create VPD response models
  vpd_models <- list(
    A = lmer(A_scaled ~ VPDleaf_scaled * treatment * year + (1|date),
             data = data_scaled),
    gsw = lmer(gsw_scaled ~ VPDleaf_scaled * treatment * year + (1|date),
               data = data_scaled),
    E = lmer(E_scaled ~ VPDleaf_scaled * treatment * year + (1|date),
             data = data_scaled)
  )
  
  # Analyze each VPD model
  vpd_results <- lapply(vpd_models, analyze_model)
  
  return(list(
    models = vpd_models,
    results = vpd_results
  ))
}