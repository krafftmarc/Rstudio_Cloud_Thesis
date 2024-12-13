# Load required packages
install.packages("tidyverse")
install.packages("kableExtra")
install.packages("flextable")
install.packages("gt")
install.packages("stargazer")
install.packages("gtsummary")
library(tidyverse)
library(kableExtra)  # For formatted tables
library(flextable)   # Alternative for Word documents
library(gt)          # For modern HTML tables
library(stargazer)   # For regression tables
library(gtsummary)   # For clinical/summary tables

# Example using your gas exchange data
# First create summary statistics
summary_table <- licor_data %>%
  group_by(Var, block) %>%
  summarise(
    A_mean = mean(A, na.rm = TRUE),
    A_se = sd(A, na.rm = TRUE)/sqrt(n()),
    gs_mean = mean(gsw, na.rm = TRUE),
    gs_se = sd(gsw, na.rm = TRUE)/sqrt(n()),
    E_mean = mean(E, na.rm = TRUE),
    E_se = sd(E, na.rm = TRUE)/sqrt(n()),
    WUEi_mean = mean(A/gsw, na.rm = TRUE),
    WUEi_se = sd(A/gsw, na.rm = TRUE)/sqrt(n())
  ) %>%
  mutate(across(where(is.numeric), round, 3))

# Using kable for PDF/HTML output
kable_table <- kable(summary_table,
                     format = "html",
                     caption = "Gas Exchange Parameters by Treatment") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE) %>%
  add_header_above(c(" " = 2,
                     "Photosynthesis" = 2,
                     "Conductance" = 2,
                     "Transpiration" = 2,
                     "WUEi" = 2)) %>%
  footnote(general = "Values represent means ± standard error")

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