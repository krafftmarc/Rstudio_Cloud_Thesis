# Set the path to the input folder containing CSV files
input_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockA/2023/cleaned_data/"

# Set the path to the output folder where you want to save the extracted data
output_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockA/2023/cleaned_data_extracted/"

# Get a list of all CSV files in the input folder
csv_files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

# Create the output folder if it doesn't exist
if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

# Function to extract the specified columns and save to the output folder
extract_and_save_columns <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Extract the first four columns and the third column
  extracted_data <- data[, c(1, 4)]
  
  # Create the output file path
  output_file <- file.path(output_folder, paste0("extracted_", basename(file_path)))
  
  # Write the extracted data to the output folder
  write.csv(extracted_data, file = output_file, row.names = FALSE)
  
  cat("Extraction complete for:", file_path, "\n")
}

# Apply the extraction function to each CSV file in the input folder
lapply(csv_files, extract_and_save_columns)
