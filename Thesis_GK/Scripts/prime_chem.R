# Load required libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(nlme)
library(car)
library(ggplot2)
library(viridis)

#----------------------------------------
# ID Parsing Functions
#----------------------------------------

parse_chem_id <- function(id) {
  # Extract components
  block <- substr(id, 1, 1)  # A or B
  row_start <- regexpr("R", id) + 1  # Position after R
  
  # Extract row number and vine group
  row_str <- substr(id, row_start, nchar(id) - 1)
  vine_group <- as.numeric(substr(id, nchar(id), nchar(id)))
  row_num <- as.numeric(row_str)
  
  # Determine treatment based on row numbers
  row_pair <- case_when(
    row_num %in% c(2,3) ~ "23",
    row_num %in% c(6,7) ~ "67",
    row_num %in% c(10,11) ~ "1011",
    TRUE ~ NA_character_
  )
  
  # Return all components
  list(
    block = block,
    row = row_num,
    row_pair = row_pair,
    vine_group = vine_group,
    full_id = id
  )
}

#----------------------------------------
# Data Loading and Initial Cleaning
#----------------------------------------

# Read and clean 2022 chemistry data
chem_2022 <- TyreePrimary_Chemistry %>%
  # Clean up variety names and create date column
  mutate(
    Variety = case_when(
      Variety == "CH" ~ "Chardonnay",
      Variety == "CS" ~ "Cabernet Sauvignon",
      TRUE ~ Variety
    ),
    Date = make_date(Year, Month, Day),
    # Create row pair identifier
    Row_Pair = case_when(
      Row %in% c(2,3) ~ "23",
      Row %in% c(6,7) ~ "67",
      Row %in% c(10,11) ~ "1011",
      TRUE ~ NA_character_
    ),
    # Create treatment groups
    Treatment = case_when(
      is.na(Irrigation) | Irrigation == "NaN" ~ "Pre-Treatment",
      TRUE ~ paste(Block, Irrigation)
    ),
    Year = 2022
  ) %>%
  # Select relevant columns
  select(Date, Year, Treatment, Block, Row, Row_Pair, Vine,
         Irrigation, Variety, Brix, pH, `TA g/L`, `Malic Acid mg/L`)

# Read and clean 2023 chemistry data
chem_2023 <- Tyree_Primary_Chem %>%
  # Parse IDs and convert numeric columns
  mutate(
    parsed_id = map(ID, parse_chem_id),
    Block = map_chr(parsed_id, "block"),
    Row = map_dbl(parsed_id, "row"),
    Row_Pair = map_chr(parsed_id, "row_pair"),
    Vine_Group = map_dbl(parsed_id, "vine_group"),
    `Brix-Aug-10` = as.numeric(`Brix-Aug-10`),
    `Brix-Aug-24` = as.numeric(`Brix-Aug-24`),
    pH = as.numeric(pH),
    Variety = "Cabernet Sauvignon",
    TA = `Raw Value TA` * 2.5
  ) %>%
  # Create separate rows for each date
  bind_rows(
    mutate(., Date = as.Date("2023-08-10"), Brix = `Brix-Aug-10`),
    mutate(., Date = as.Date("2023-08-24"), Brix = `Brix-Aug-24`)
  ) %>%
  # Clean up and select final columns
  mutate(
    Year = 2023,
    Treatment = paste(Block, "2L")
  ) %>%
  select(Date, Year, Treatment, Block, Row, Row_Pair, Vine_Group,
         Variety, Brix, pH, TA)

# Print data summaries
print("2022 Data Structure:")
print(table(chem_2022$Block, chem_2022$Row_Pair))
print(table(chem_2022$Variety, chem_2022$Treatment))

print("\n2023 Data Structure:")
print(table(chem_2023$Block, chem_2023$Row_Pair))
print(table(chem_2023$Treatment))


#----------------------------------------
# Data Integration and Feature Engineering
#----------------------------------------

# Combine all datasets
full_data <- chemistry_data %>%
  left_join(irr_data, by = "Date") %>%
  left_join(cimis_data, by = c("Date", "year" = "year"))

# Check data structure and missing values
str(full_data)
summary(full_data)
missing_values <- colSums(is.na(full_data))
print("Missing values per column:")
print(missing_values[missing_values > 0])

# Create analysis dataset with weather averages
analysis_data <- full_data %>%
  # Add sampling period identifier
  group_by(Date) %>%
  mutate(
    Sampling_Period = dense_rank(Date),
    # Create 7-day and 14-day weather averages
    across(c("Max Air Temp (F)", "Min Air Temp (F)", "Avg Air Temp (F)",
             "Avg Rel Hum (%)", "ETo (in)", "Avg Soil Temp (F)"),
           list(
             avg_7d = ~mean(lag(., 1:7), na.rm = TRUE),
             avg_14d = ~mean(lag(., 1:14), na.rm = TRUE)
           ))
  ) %>%
  ungroup()


# Calculate summary statistics
treatment_summary <- combined_data %>%
  filter(!is.na(Treatment)) %>%
  group_by(Treatment, Variety, Sampling_Period) %>%
  summarise(
    across(c(Brix, pH, `TA g/L`, `Malic Acid mg/L`),
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE),
             n = ~sum(!is.na(.))
           )),
    .groups = "drop"
  )

# Fit mixed effects models using nlme instead of lme4
fit_mixed_model <- function(data, response_var) {
  formula <- as.formula(paste(response_var, 
                              "~ Treatment * Variety + 
                             scale(Max_Air_Temp_F_avg_7d) +
                             scale(ETo_in_avg_7d)"))
  
  lme(formula, 
      random = ~1|Block,
      data = data %>% filter(!is.na(Treatment)),
      method = "REML")
}

# Fit models for each chemistry parameter
models <- list(
  brix = fit_mixed_model(combined_data, "Brix"),
  pH = fit_mixed_model(combined_data, "pH"),
  TA = fit_mixed_model(combined_data, "TA g/L"),
  malic = fit_mixed_model(combined_data, "Malic Acid mg/L")
)

# Create visualization function
plot_chemistry_by_treatment <- function(data, y_var, y_label) {
  ggplot(data, aes_string(x = "Treatment", y = y_var, fill = "Variety")) +
    geom_boxplot() +
    theme_bw() +
    labs(y = y_label, x = "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_viridis(discrete = TRUE)
}

# Create plots
brix_plot <- plot_chemistry_by_treatment(combined_data, "Brix", "Brix (°)")
pH_plot <- plot_chemistry_by_treatment(combined_data, "pH", "pH")
TA_plot <- plot_chemistry_by_treatment(combined_data, "`TA g/L`", "Titratable Acidity (g/L)")
malic_plot <- plot_chemistry_by_treatment(combined_data, "`Malic Acid mg/L`", "Malic Acid (mg/L)")

# Additional time series plots to show chemistry development
chemistry_time_series <- function(data, y_var, y_label) {
  ggplot(data, aes_string(x = "Date", y = y_var, color = "Treatment")) +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(~Variety) +
    theme_bw() +
    labs(y = y_label, x = "Date") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_viridis(discrete = TRUE)
}

# Save results
write_csv(combined_data, "cleaned_combined_data.csv")
write_csv(treatment_summary, "treatment_summary.csv")

# Save model summaries
capture.output(
  lapply(names(models), function(name) {
    cat("\nModel summary for", name, ":\n")
    print(summary(models[[name]]))
    # Add ANOVA table for fixed effects
    cat("\nANOVA for fixed effects:\n")
    print(anova(models[[name]]))
  }),
  file = "model_summaries.txt"
)