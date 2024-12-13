# Load required packages
library(tidyverse)
library(kableExtra)  # For formatted tables
library(flextable)   # Alternative for Word documents
library(gt)          # For modern HTML tables
library(stargazer)   # For regression tables
library(gtsummary)   # For clinical/summary tables
licor_data <- licor_comb_2022_final

# Create summary statistics grouped by Tx and Stress
# First, let's look at unique values in Tx column
unique(licor_data$Tx)

summary_table <- licor_data %>%
  mutate(Tx = as.character(Tx)) %>%
  group_by(Var, Tx, Stress) %>%
  summarise(
    # Photosynthesis (A)
    A_mean = mean(A, na.rm = TRUE),
    A_se = sd(A, na.rm = TRUE)/sqrt(sum(!is.na(A))),
    
    # Stomatal conductance (gsw)
    gs_mean = mean(gsw, na.rm = TRUE),
    gs_se = sd(gsw, na.rm = TRUE)/sqrt(sum(!is.na(gsw))),
    
    # Transpiration (E) - multiply by 1000 to shift decimal
    E_mean = mean(E, na.rm = TRUE) * 1000,
    E_se = sd(E, na.rm = TRUE)/sqrt(sum(!is.na(E))) * 1000,
    
    # Intrinsic Water Use Efficiency (WUEi)
    WUEi_mean = mean(A/gsw, na.rm = TRUE),
    WUEi_se = sd(A/gsw, na.rm = TRUE)/sqrt(sum(!is.na(A/gsw))),
    
    .groups = 'drop'
  ) %>%
  mutate(
    Var = case_when(
      Var == "ch" ~ "Chardonnay",
      Var == "cs" ~ "Cabernet Sauvignon",
      TRUE ~ Var
    ),
    Tx = case_when(
      Tx == "NaN" ~ "Control",
      Tx == "2" ~ "2L Emitter",
      Tx == "4" ~ "4L Emitter",
      TRUE ~ Tx
    )
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# Create table using gt
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Gas Exchange Parameters by Variety, Treatment, and Stress Level",
  ) %>%
  tab_spanner(
    label = "Photosynthesis (μmol m⁻² s⁻¹)",
    columns = c(A_mean, A_se)
  ) %>%
  tab_spanner(
    label = "Stomatal Conductance (mol m⁻² s⁻¹)",
    columns = c(gs_mean, gs_se)
  ) %>%
  tab_spanner(
    label = "Transpiration (mmol m⁻² s⁻¹)",
    columns = c(E_mean, E_se)
  ) %>%
  tab_spanner(
    label = "WUEi (μmol CO₂/mol H₂O)",
    columns = c(WUEi_mean, WUEi_se)
  ) %>%
  cols_label(
    A_mean = "Mean",
    A_se = "SE",
    gs_mean = "Mean",
    gs_se = "SE",
    E_mean = "Mean",
    E_se = "SE",
    WUEi_mean = "Mean",
    WUEi_se = "SE"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  ) %>%
  tab_source_note(
    source_note = "Values represent means ± standard error"
  )

# Display the table
gt_table

# Using flextable for Word documents
flex_table <- flextable(summary_table) %>%
  theme_vanilla() %>%
  add_header_row(values = c("", "", "Photosynthesis", "Conductance",
                            "Transpiration", "WUEi"),
                 colwidths = c(1, 1, 2, 2, 2, 2)) %>%
  align(align = "center", part = "all") %>%
  autofit()

# Using gt for web/HTML output
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Gas Exchange Parameters by Treatment",
    subtitle = "Means ± Standard Error"
  ) %>%
  cols_label(
    Var = "Variety",
    block = "Treatment",
    A_mean = "Mean",
    A_se = "SE",
    gs_mean = "Mean",
    gs_se = "SE",
    E_mean = "Mean",
    E_se = "SE",
    WUEi_mean = "Mean",
    WUEi_se = "SE"
  ) %>%
  tab_spanner(
    label = "Photosynthesis",
    columns = c(A_mean, A_se)
  ) %>%
  tab_spanner(
    label = "Conductance",
    columns = c(gs_mean, gs_se)
  ) %>%
  tab_spanner(
    label = "Transpiration",
    columns = c(E_mean, E_se)
  ) %>%
  tab_spanner(
    label = "WUEi",
    columns = c(WUEi_mean, WUEi_se)
  )

# For ANOVA results
anova_table <- stargazer(container_model,
                         type = "text",
                         title = "ANOVA Results",
                         digits = 3)

# Save tables
# For Word
save_as_docx(flex_table, path = "summary_table.docx")

# For PDF/HTML
save_kable(kable_table, file = "summary_table.html")

# Export as CSV
write.csv(summary_table, "summary_table.csv")

# Create publication-quality summary for high temperature data
high_temp_summary <- licor_data %>%
  filter(Tleaf > 30) %>%
  group_by(Var, block) %>%
  summarise(
    n = n(),
    A_mean = mean(A, na.rm = TRUE),
    A_se = sd(A, na.rm = TRUE)/sqrt(n()),
    gs_mean = mean(gsw, na.rm = TRUE),
    gs_se = sd(gsw, na.rm = TRUE)/sqrt(n()),
    Tleaf_mean = mean(Tleaf, na.rm = TRUE),
    Tleaf_se = sd(Tleaf, na.rm = TRUE)/sqrt(n())
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# Create a formatted table for high temperature data
high_temp_table <- gt(high_temp_summary) %>%
  tab_header(
    title = "Gas Exchange Parameters During Heat Stress (>30°C)",
    subtitle = "Treatment Means ± Standard Error"
  ) %>%
  fmt_number(
    columns = contains("mean"),
    decimals = 2
  ) %>%
  fmt_number(
    columns = contains("se"),
    decimals = 3
  ) %>%
  cols_label(
    n = "Sample Size",
    A_mean = "A (μmol m⁻² s⁻¹)",
    gs_mean = "gs (mol m⁻² s⁻¹)",
    Tleaf_mean = "Tleaf (°C)"
  )

# Export tables
gtsave(high_temp_table, "high_temp_summary.html")