# Load required libraries
library(tidyverse)
library(readxl)

# Step 1: Process Environmental Data
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

# Step 2: Process Water Potential Data
process_water_potential <- function(data, year) {
  tryCatch({
    if(year == 2022) {
      wp <- data %>%
        select(Month, Day, Year, Block_ID, Row_ID, Vine_ID, PSI) %>%
        mutate(
          Date = as.Date(paste(Year, Month, Day, sep="-")),
          Block = Block_ID,
          Row = as.character(Row_ID),
          Vine = as.character(Vine_ID)
        ) %>%
        select(Date, Block, Row, Vine, PSI)
    } else { # 2023
      wp <- data %>%
        mutate(
          Date = as.Date(as.numeric(date) - 25569, origin = "1970-01-01"), # Convert Excel date
          Block = as.character(block),
          Row = as.character(row),
          Vine = as.character(vine_id),
          PSI = as.numeric(Ψ)
        ) %>%
        select(Date, Block, Row, Vine, PSI)
    }
    return(wp)
  }, error = function(e) {
    message("Error processing water potential data: ", e$message)
    return(NULL)
  })
}

# Step 3: Process Licor Data
process_licor <- function(data, year) {
  tryCatch({
    if(year == 2022) {
      lc <- data %>%
        select(date, block, row, vine, A, gsw, Tleaf) %>%
        mutate(
          Date = as.Date(date, format = "%m/%d/%y"),
          Block = as.character(block),
          Row = as.character(row),
          Vine = as.character(vine),
          Photosynthesis_Rate = A,
          Stomatal_Conductance = gsw,
          Tleaf = Tleaf
        ) %>%
        select(Date, Block, Row, Vine, Photosynthesis_Rate, Stomatal_Conductance, Tleaf)
    } else { # 2023
      lc <- data %>%
        select(date, Block, Row, Vine, A, gsw, Tleaf) %>%
        mutate(
          Date = as.Date(substr(date, 1, 8), format = "%Y%m%d"),
          Block = as.character(Block),
          Row = as.character(Row),
          Vine = as.character(Vine),
          Photosynthesis_Rate = A,
          Stomatal_Conductance = gsw,
          Tleaf = Tleaf
        ) %>%
        select(Date, Block, Row, Vine, Photosynthesis_Rate, Stomatal_Conductance, Tleaf)
    }
    return(lc)
  }, error = function(e) {
    message("Error processing licor data: ", e$message)
    return(NULL)
  })
}

# Step 4: Process Chemistry Data
# Modify the chemistry processing function for 2023
process_chemistry <- function(data, year) {
  tryCatch({
    if(year == 2022) {
      chem <- data %>%
        select(Block, Row, Brix, pH, `TA g/L`) %>%
        mutate(
          Block = as.character(Block),
          Row = as.character(Row),
          TA = as.numeric(`TA g/L`)
        ) %>%
        select(Block, Row, Brix, pH, TA)
    } else { # 2023
      # First, let's look at the structure
      print("2023 Chemistry Data Structure:")
      print(str(data))
      
      # Extract block and row from ID column if possible
      chem <- data %>%
        mutate(
          # Assuming ID format like "Block-Row" or similar
          Block = substr(ID, 1, 1),  # Extract first character
          Row = substr(ID, 3, 4),    # Extract after hyphen
          Brix = `Brix-Aug-10`,
          TA = as.numeric(TA)
        ) %>%
        select(Block, Row, Brix, pH, TA)
    }
    return(chem)
  }, error = function(e) {
    message("Error processing chemistry data: ", e$message)
    return(NULL)
  })
}

# Let's examine the data at each step
print("Processing 2023 Chemistry Data:")
chem_2023 <- process_chemistry(primary_chem_2023, 2023)
print("2023 Chemistry Data Result:")
print(str(chem_2023))

# Create final datasets with error checking
create_final_dataset <- function(env_data, phys_data, chem_data, yield_data, year) {
  tryCatch({
    # Check data availability
    if(is.null(env_data)) stop("Environmental data is NULL")
    if(is.null(phys_data)) stop("Physiological data is NULL")
    if(is.null(chem_data)) stop("Chemistry data is NULL")
    if(is.null(yield_data)) stop("Yield data is NULL")
    
    # Print debugging info
    cat(sprintf("\nProcessing Year %d:\n", year))
    cat("Environmental data columns:", paste(names(env_data), collapse=", "), "\n")
    cat("Physiological data columns:", paste(names(phys_data), collapse=", "), "\n")
    cat("Chemistry data columns:", paste(names(chem_data), collapse=", "), "\n")
    cat("Yield data columns:", paste(names(yield_data), collapse=", "), "\n")
    
    # Perform joins with error checking
    result <- env_data %>%
      full_join(phys_data, by = "Date") %>%
      {
        if(all(c("Block", "Row", "Vine") %in% names(.))) {
          full_join(., yield_data, by = c("Block", "Row", "Vine"))
        } else {
          message("Missing required columns for yield join")
          .
        }
      } %>%
      {
        if(all(c("Block", "Row") %in% names(.))) {
          full_join(., chem_data, by = c("Block", "Row"))
        } else {
          message("Missing required columns for chemistry join")
          .
        }
      } %>%
      mutate(Year = year)
    
    return(result)
  }, error = function(e) {
    message("Error creating final dataset for year ", year, ": ", e$message)
    return(NULL)
  })
}

# Process the data with debugging info
cat("\nProcessing 2022 data:\n")
data_2022 <- create_final_dataset(env_data_2022_processed, phys_2022, chem_2022, yield_2022, 2022)
cat("\nProcessing 2023 data:\n")
data_2023 <- create_final_dataset(env_data_2023_processed, phys_2023, chem_2023, yield_2023_processed, 2023)

# Combine years with error checking
final_data <- if(!is.null(data_2022) || !is.null(data_2023)) {
  bind_rows(
    if(!is.null(data_2022)) data_2022,
    if(!is.null(data_2023)) data_2023
  )
} else {
  stop("No valid data available for either year")
}

# Export with checks
if(!is.null(final_data) && nrow(final_data) > 0) {
  write_csv(final_data, "Vineyard_Data_Combined_2022_2023.csv")
  cat("\nData exported successfully\n")
  
  # Print summary statistics
  cat("\nSummary of final dataset:\n")
  print(summary(final_data))
  
  cat("\nMissing values by column:\n")
  print(colSums(is.na(final_data)))
} else {
  stop("No data available to export")
}

# Step 5: Process Yield Data
process_yield <- function(harvest_data, year) {
  tryCatch({
    if(year == 2022) {
      yld <- harvest_data %>%
        rename(
          Weight_Kg = `Weight (Kg)`,
          Cluster_Count = `Cluster Count`
        ) %>%
        mutate(
          Block = as.character(Block),
          Row = as.character(Row),
          Vine = as.character(Vine)
        )
    } else { # 2023
      yld <- harvest_data %>%
        rename(
          Weight_Kg = `Wt(Kg)`,
          Cluster_Count = `Cluster Count`
        ) %>%
        mutate(
          Block = as.character(Block),
          Row = as.character(`Row Vine`),
          Vine = as.character(Vine),
          Weight_Kg = Weight_Kg - Tare # Adjust for tare weight
        ) %>%
        select(Block, Row, Vine, Weight_Kg, Cluster_Count)
    }
    return(yld)
  }, error = function(e) {
    message("Error processing yield data: ", e$message)
    return(NULL)
  })
}

# Process 2022 data
wp_2022 <- process_water_potential(water_potential_2022, 2022)
licor_2022_processed <- process_licor(licor_2022, 2022)
chem_2022 <- process_chemistry(primary_chem_2022, 2022)
yield_2022 <- process_yield(harvest_2022, 2022)

# Process 2023 data
wp_2023 <- process_water_potential(water_potential_2023, 2023)
licor_2023_processed <- process_licor(licor_2023, 2023)
chem_2023 <- process_chemistry(primary_chem_2023, 2023)
yield_2023_processed <- process_yield(yield_2023, 2023)

# Merge physiological data
merge_phys_data <- function(wp, licor) {
  full_join(wp, licor, by = c("Date", "Block", "Row", "Vine"))
}

phys_2022 <- merge_phys_data(wp_2022, licor_2022_processed)
phys_2023 <- merge_phys_data(wp_2023, licor_2023_processed)

# Create final datasets
create_final_dataset <- function(env_data, phys_data, chem_data, yield_data, year) {
  env_data %>%
    full_join(phys_data, by = "Date") %>%
    full_join(yield_data, by = c("Block", "Row", "Vine")) %>%
    full_join(chem_data, by = c("Block", "Row")) %>%
    mutate(Year = year)
}

data_2022 <- create_final_dataset(env_data_2022_processed, phys_2022, chem_2022, yield_2022, 2022)
data_2023 <- create_final_dataset(env_data_2023_processed, phys_2023, chem_2023, yield_2023_processed, 2023)

# Combine years
final_data <- bind_rows(data_2022, data_2023)

# Export
write_csv(final_data, "Vineyard_Data_Combined_2022_2023.csv")

# Print summary statistics
cat("\nSummary of final dataset:\n")
print(summary(final_data))

cat("\nMissing values by column:\n")
print(colSums(is.na(final_data)))

# Step 1: Filter numerical columns
numeric_columns <- final_data %>%
  select(where(is.numeric))

# Step 2: Compute the correlation matrix
correlation_matrix <- cor(numeric_columns, use = "complete.obs")

# Step 3: Print the correlation matrix
cat("\nCorrelation Matrix:\n")
print(correlation_matrix)

# Step 4: Save correlation values as a separate table for publication
# Convert correlation matrix to a tidy format
correlation_table <- as.data.frame(as.table(correlation_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

# Save the table to a CSV file
write_csv(correlation_table, "Correlation_Table.csv")
cat("\nCorrelation values saved to Correlation_Table.csv\n")

# Step 5: Visualize the correlation matrix without numerical values
# Install ggcorrplot if not already installed
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}

library(ggcorrplot)

# Visualize using ggcorrplot without numerical values
ggcorrplot(correlation_matrix, 
           method = "circle", 
           lab = FALSE, # Remove numerical labels
           title = "Correlation Matrix for Vineyard Data", 
           legend.title = "Correlation")
