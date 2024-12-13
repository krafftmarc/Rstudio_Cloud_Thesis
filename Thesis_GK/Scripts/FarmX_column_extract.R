library(readr)
library(dplyr)

# Set the source folder and destination folder paths
source_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockB/SM/2022/6_48_avg/"
destination_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockB/SM/2022/6_48_extract/"

# Create the destination folder if it doesn't exist
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)
}

# List  all CSV files in the source folder
csv_files <- list.files(path = source_folder, pattern = ".csv", full.names = TRUE)

# Function to extract the desired columns
extract_columns <- function(file) {
  data <- read_csv(file)
  num_columns <- ncol(data)
  
  if (num_columns >= 7) {
    selected_columns <- c(1:4, seq(7, num_columns, by = 3))
    extracted_data <- data[, selected_columns]
  } else {
    # Handle the case when there are not enough columns (e.g., use the first four columns)
    extracted_data <- data[, 1:4]
  }
  
  return(extracted_data)
}


# Process each CSV file and save the extracted data to the destination folder
for (file in csv_files) {
  extracted_data <- extract_columns(file)
  destination_file <- file.path(destination_folder, basename(file))
  write_csv(extracted_data, destination_file)
}

