library(dplyr)
library(readr)
library(tools)

# Set the folder path
folder_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockA/2022/6_48/6_48_avg/"

# Get the list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store the modified CSV data
modified_data <- list()

# Process each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read_csv(file)
  
  # Get the column names
  col_names <- colnames(data)
  
  # Add a column with the CSV file name without the file path
  csv_file_name <- tools::file_path_sans_ext(basename(file))
  data$CSV_File <- csv_file_name
  
  # Store the modified data
  modified_data[[file]] <- data
}

# Combine the modified data into a single data frame
combined_data <- bind_rows(modified_data)

# Export the combined data to a new CSV file
write_csv(combined_data, "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/output.csv")
