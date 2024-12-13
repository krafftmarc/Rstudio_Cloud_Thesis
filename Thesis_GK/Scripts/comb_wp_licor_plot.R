library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)

data <- readxl::read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/updated_aligned_psi_values.xlsx")
gasx <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/Baseline -WatPot _ LiCor/clean_licor_2023_extract.csv")
# Assuming your original date-time column is called 'datetime_column' in a data frame 'your_dataframe'
gasx$date <- as.POSIXct(gasx$date, format = "%Y-%m-%d %H:%M:%S")

# Now convert this to just a date
gasx$date <- as.Date(gasx$date)
data <- data %>% mutate(vine = NA)
data <- data %>% mutate(vine = vine_id) %>% select(-vine_id)

data_selected <- data %>% select(Ψ_leaf, c(date, block, row, vine))
gasx_selected <- gasx %>% select(c(E, Tx), c(date, block, row, vine))
combined_df <- left_join(data_selected, gasx_selected, by = c("date", "block", "row", "vine"))

plot(combined_df$Ψ_leaf, combined_df$E)
write_csv(combined_df, "~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/2023/2023/combined_df.csv")
