closeAllConnections()

source("/cloud/project/Thesis_GK/Scripts/functions.R")
source("/cloud/project/Thesis_GK/Scripts/run_analysis.R")
results <- run_complete_analysis()
warnings()
lifecycle::last_lifecycle_warnings()
rm(list = ls())
conflicts <- intersect(ls(), ls(baseenv()))
print(conflicts)

# Check the contents of run_analysis.R
readLines("/cloud/project/Thesis_GK/Scripts/run_analysis.R")
