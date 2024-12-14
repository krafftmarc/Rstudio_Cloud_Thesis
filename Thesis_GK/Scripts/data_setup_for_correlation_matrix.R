# Load required libraries
library(tidyverse)
library(readxl)

# Step 1: Load and Validate Datasets
# First check data structure before processing
print_dataset_info <- function(dataset, name) {
  cat("\nStructure for", name, ":\n")
  print(str(dataset))
  cat("\nColumn names for", name, ":\n")
  print(colnames(dataset))
}

# Load datasets
env_data_2022 <- cimis_2022_season
env_data_2023 <- CIMIS_2023
water_potential_2022 <- Tyree_2022_WaterPotentials
licor_2022 <- licor_combined_2022
water_potential_2023 <- Tyree_Water_Potentials_2023
licor_2023 <- X2023_comb_Licor
primary_chem_2022 <- TyreePrimaryChem_Cleaned
harvest_2022 <- Tyree_CS_Harvest_9_16_22_2022
primary_chem_2023 <- Tyree_Primary_Chem
pruning_weights_2023 <- Pruning_Weights_Tyree_2023_xlsx_Sheet1
yield_2023 <- Tyree_2023_Yield_xlsx_Cabernet_Sauvignon_9212023_

# Print structure of all datasets
datasets <- list(
  env_data_2022 = env_data_2022,
  env_data_2023 = env_data_2023,
  water_potential_2022 = water_potential_2022,
  licor_2022 = licor_2022,
  water_potential_2023 = water_potential_2023,
  licor_2023 = licor_2023,
  primary_chem_2022 = primary_chem_2022,
  harvest_2022 = harvest_2022,
  primary_chem_2023 = primary_chem_2023,
  pruning_weights_2023 = pruning_weights_2023,
  yield_2023 = yield_2023
)

for(name in names(datasets)) {
  print_dataset_info(datasets[[name]], name)
}

# Step 2: Process Environmental Data
process_env_data <- function(data) {
  tryCatch({
    data %>%
      rename_with(
        ~case_when(
          . == "Date" ~ "Date",
          grepl("Max.*Temp", ., ignore.case = TRUE) ~ "Max_Temp",
          grepl("Min.*Temp", ., ignore.case = TRUE) ~ "Min_Temp",
          grepl("Vap.*Pres", ., ignore.case = TRUE) ~ "VPD",
          grepl("ETo", ., ignore.case = TRUE) ~ "ET0",
          TRUE ~ .
        )
      ) %>%
      mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  }, error = function(e) {
    message("Error processing environmental data: ", e$message)
    return(NULL)
  })
}

env_data_2022_processed <- process_env_data(env_data_2022)
env_data_2023_processed <- process_env_data(env_data_2023)

# Step 3: Process Physiological Data
process_phys_data <- function(water_potential, licor) {
  tryCatch({
    # Standardize column names in water potential data
    wp <- water_potential %>%
      rename_with(
        ~case_when(
          grepl("Block.*ID", ., ignore.case = TRUE) ~ "Block",
          grepl("Row.*ID", ., ignore.case = TRUE) ~ "Row",
          grepl("Vine.*ID", ., ignore.case = TRUE) ~ "Vine",
          grepl("PSI", ., ignore.case = TRUE) ~ "PSI",
          TRUE ~ .
        )
      )
    
    # Standardize column names in licor data
    lc <- licor %>%
      rename_with(
        ~case_when(
          grepl("date", ., ignore.case = TRUE) ~ "Date",
          grepl("Tleaf", ., ignore.case = TRUE) ~ "Tleaf",
          grepl("^A$", .) ~ "Photosynthesis_Rate",
          grepl("gsw", ., ignore.case = TRUE) ~ "Stomatal_Conductance",
          TRUE ~ .
        )
      )
    
    # Ensure consistent data types
    wp <- wp %>%
      mutate(across(c(Block, Row, Vine), as.character))
    
    lc <- lc %>%
      mutate(across(c(Block, Row, Vine), as.character))
    
    # Join the datasets
    left_join(wp, lc, by = c("Block", "Row", "Vine"))
  }, error = function(e) {
    message("Error processing physiological data: ", e$message)
    return(NULL)
  })
}

phys_data_2022 <- process_phys_data(water_potential_2022, licor_2022)
phys_data_2023 <- process_phys_data(water_potential_2023, licor_2023)

# Step 4: Process Quality/Yield Data
process_quality_yield <- function(harvest, chemistry, pruning = NULL) {
  tryCatch({
    # Process harvest data
    h <- harvest %>%
      rename_with(
        ~case_when(
          grepl("Weight.*Kg", ., ignore.case = TRUE) ~ "Weight_Kg",
          grepl("Cluster.*Count", ., ignore.case = TRUE) ~ "Cluster_Count",
          TRUE ~ .
        )
      ) %>%
      mutate(across(c(Block, Row, Vine), as.character))
    
    # Process chemistry data
    c <- chemistry %>%
      select(matches("Block|Row|Vine|Brix|pH|TA")) %>%
      mutate(across(c(Block, Row, Vine), as.character))
    
    # Join harvest and chemistry
    result <- left_join(h, c, by = c("Block", "Row", "Vine"))
    
    # Add pruning data if provided
    if (!is.null(pruning)) {
      p <- pruning %>%
        rename_with(
          ~case_when(
            grepl("Weight.*kg", ., ignore.case = TRUE) ~ "Pruning_Weight",
            TRUE ~ .
          )
        ) %>%
        mutate(across(c(Block, Row, Vine), as.character))
      
      result <- left_join(result, p, by = c("Block", "Row", "Vine"))
    }
    
    return(result)
  }, error = function(e) {
    message("Error processing quality/yield data: ", e$message)
    return(NULL)
  })
}

quality_yield_2022 <- process_quality_yield(harvest_2022, primary_chem_2022)
quality_yield_2023 <- process_quality_yield(yield_2023, primary_chem_2023, pruning_weights_2023)

# Step 5: Combine All Data
combine_year_data <- function(env_data, phys_data, quality_yield_data, year) {
  tryCatch({
    env_data %>%
      left_join(phys_data, by = "Date") %>%
      left_join(quality_yield_data, by = c("Block", "Row", "Vine")) %>%
      mutate(Year = year)
  }, error = function(e) {
    message("Error combining data for year ", year, ": ", e$message)
    return(NULL)
  })
}

full_data_2022 <- combine_year_data(env_data_2022_processed, phys_data_2022, quality_yield_2022, 2022)
full_data_2023 <- combine_year_data(env_data_2023_processed, phys_data_2023, quality_yield_2023, 2023)

# Step 6: Combine Years and Export
if (!is.null(full_data_2022) && !is.null(full_data_2023)) {
  full_data_combined <- bind_rows(full_data_2022, full_data_2023)
  
  # Validate final dataset
  cat("\nFinal combined dataset summary:\n")
  print(summary(full_data_combined))
  
  # Check for missing values
  cat("\nMissing values by column:\n")
  print(colSums(is.na(full_data_combined)))
  
  # Export data
  write_csv(full_data_combined, "Full_Data_2022_2023_Combined.csv")
  cat("\nData exported successfully to Full_Data_2022_2023_Combined.csv\n")
} else {
  message("Error: Unable to create combined dataset due to processing errors")
}
