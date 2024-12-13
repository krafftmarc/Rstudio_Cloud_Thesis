library(tidyverse)
farmX <- read_csv("~/Desktop/FarmX_macbook/FarmXDataExport (PR6-1B).csv")
# Load the lubridate package for handling dates if not already loaded
# library(lubridate)

library(dplyr)

# Assuming your data frame is named 'farmX'
farmX <- farmX %>% mutate(date = format(`Date (PST)`, format = "%m-%d-%Y"))
library(dplyr)

# Assuming 'farmX' already contains a column named 'date' with the date in "m-d-y" format
result <- farmX %>% group_by(date) %>% summarize(avg_result = mean(`Water Pressure (psi) - 57:0a:cb:52:da:9c`))


#farmX_avg <- farmX %>% mutate(month = month(farmX$`Date (PST)`), day = day(farmX$`Date (PST)`), year = year(farmX$`Date (PST)`)) %>% group_by(year, month, day) %>% summarize(avg_result = mean(farmX$`Water Pressure (psi) - 57:0a:cb:52:da:9c`)) %>% unite(MDY, month, day, year, sep = "_", remove = FALSE) 
# Set the folder path where you want to save the CSV file
folder_path <- "~/Desktop/FarmX_macbook/"

# Create the full file path including the file name
file_path <- file.path(folder_path, "PR6_1B_AVG.csv")

# Export the data frame to a CSV file
write.csv(result, file_path, row.names = FALSE)
