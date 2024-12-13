# Load the readxl package
library(readxl)

# Load the xlsx file
xlsx_file <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/clean_licor_2023.xlsx")

# Select the columns to extract
selected_cols <- xlsx_file[, c("date", "block", "row", "vine", "Tx", "Var", "E", "A", "Ci", "gsw", "Tleaf", "Tair", "ETR")]

# Export the selected columns to a csv file
write.csv(selected_cols, file = "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/clean_licor_2023_extract.csv", row.names = FALSE)
