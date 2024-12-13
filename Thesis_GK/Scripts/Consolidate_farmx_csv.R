# Load necessary library
library(dplyr)

# Set the working directory to where your CSV files are located
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/FarmX_data/FarmX_blockA/2022/6_48/")

# List all CSV files in the directory
file_list <- list.files(pattern = "*.csv")

# Function to read each file and return as a data frame
read_and_return_df <- function(file_name) {
  read.csv(file_name, stringsAsFactors = FALSE)
}

# Read each file and combine
all_data <- lapply(file_list, read_and_return_df) %>% bind_rows()

# Export to a new CSV file
write.csv(all_data, "Consolidated_File.csv", row.names = FALSE)
