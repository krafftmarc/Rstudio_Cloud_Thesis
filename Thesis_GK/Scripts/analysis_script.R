# Clear any existing connections and workspace
closeAllConnections()

# Source required libraries and functions
message("Loading required functions...")
source("/cloud/project/Thesis_GK/Scripts/functions.R")

# Check for required functions
required_functions <- c(
  "create_comparison_plots",
  "process_cimis",
  "load_cimis_2022",
  "load_cimis_2023"
)

# Validate all required functions exist
missing_functions <- required_functions[!sapply(required_functions, exists)]
if (length(missing_functions) > 0) {
  message("Warning: The following required functions are missing:")
  print(missing_functions)
  message("\nAvailable functions:")
  print(ls(pattern = "^create"))
}

# Source analysis script
message("Loading analysis script...")
source("/cloud/project/Thesis_GK/Scripts/run_analysis.R")

# Add debugging before running the analysis
message("\nStarting analysis with available functions:")
print(ls(pattern = "^create"))

# Run analysis with error handling
message("\nExecuting complete analysis...")
results <- withCallingHandlers({
  tryCatch({
    run_complete_analysis()
  }, error = function(e) {
    message("\nError in analysis: ", e$message)
    message("\nStack trace:")
    print(sys.calls())
    return(NULL)
  })
}, warning = function(w) {
  message("\nWarning: ", w$message)
})

# Check results
tryCatch({
  if(!is.null(results)) {
    message("\nAnalysis completed successfully")
    message("Results contain:")
    print(names(results))
  } else {
    message("\nAnalysis failed - check error messages above")
  }
}, error = function(e) {
  message("\nError checking results: ", e$message)
})

# Print session info for debugging
message("\nSession Info:")
print(sessionInfo())
