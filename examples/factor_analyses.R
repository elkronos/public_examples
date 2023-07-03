## This script is in development.

# Load required libraries
load_libraries <- function() {
  required_packages <- c("psych", "lavaan", "semPlot", "ggplot2")
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  if(length(missing_packages) > 0) {
    stop(paste("These required packages are missing:", paste(missing_packages, collapse=", ")))
  }
  
  lapply(required_packages, library, character.only = TRUE)
}

# Function to select data based on pattern
select_data <- function(data, pattern) {
  selected_vars <- grepl(pattern, names(data))
  
  if(all(!selected_vars)) {
    stop(paste("Pattern", pattern, "not found in data."))
  }
  
  scale(data[, selected_vars])
}

# Function to check reliability
check_reliability <- function(data) {
  if(any(is.na(data))) {
    stop("Data contains missing values. Please clean the data before checking reliability.")
  }
  
  psych::alpha(data)$total$raw_alpha
}

# Function to perform EFA
perform_EFA <- function(data, n_factors, rotation = "varimax"){
  if(any(is.na(data))) {
    stop("Data contains missing values. Please clean the data before performing EFA.")
  }
  
  if(n_factors <= 0 || n_factors > ncol(data)) {
    stop("Invalid number of factors for EFA.")
  }
  
  if(!(rotation %in% c("varimax", "quartimax", "promax", "oblimin", "none"))) {
    stop("Invalid rotation method for EFA.")
  }
  
  efa_res <- fa(r = data, nfactors = n_factors, rotate = rotation)
  return(efa_res)
}

# Function to perform CFA
perform_CFA <- function(data, model){
  if(any(is.na(data))) {
    stop("Data contains missing values. Please clean the data before performing CFA.")
  }
  
  cfa_res <- cfa(model, data = data)
  return(summary(cfa_res, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
}

# Function to perform EFA, CFA and reliability checks for each combination of parameters for a scale
factor_analysis_scale <- function(data, params) {
  # Select data
  data_selected <- select_data(data, as.character(params['pattern']))
  
  # Check reliability
  reliability <- check_reliability(data_selected)
  print(paste0("Reliability for ", params['pattern'], ": ", reliability))
  
  # EFA
  efa_res <- perform_EFA(data_selected, as.integer(params['n_factors']), as.character(params['rotation']))
  print(efa_res)
  
  # Scree plot
  png(paste0(params['pattern'], "_scree_plot.png"))
  scree(efa_res, main="Scree Plot", sub="")
  dev.off()
  
  # Factor loadings plot
  png(paste0(params['pattern'], "_factor_loadings_plot.png"))
  fa.diagram(efa_res)
  dev.off()
  
  # CFA
  cfa_res <- perform_CFA(data_selected, as.character(params['model']))
  print(cfa_res)
  
  # Path diagram
  png(paste0(params['pattern'], "_path_diagram.png"))
  semPaths(cfa_res, "model", "std", curveAdjacent = T, style="lisrel")
  dev.off()
}

# Load required libraries
load_libraries()

# Load your data
data <- tryCatch({
  read.csv("yourdata.csv")
}, error = function(e) {
  stop("Failed to load data: ", e$message)
})

# User inputs: each row represents a unique analysis
# Specify item name pattern, number of factors, rotation method and CFA model for each analysis
user_inputs <- data.frame(
  pattern = rep(c("scale1_pattern", "scale2_pattern"), each = 2),  # item name patterns
  n_factors = c(3, 4, 2, 3),  # number of factors for EFA
  rotation = rep(c("varimax", "promax"), 2),  # rotation methods for EFA
  model = rep(c('F1 =~ x1 + x2 + x3', 'F1 =~ x1 + x2 + x3 + x4'), each = 2)  # models for CFA
)

# Perform EFA, CFA and reliability checks for each row in the user inputs
apply(user_inputs, 1, function(params) {
  tryCatch({
    factor_analysis_scale(data, params)
  }, error = function(e) {
    message("Failed to perform factor analysis on scale ", params['pattern'], ": ", e$message)
  })
})
