#' Selects the best predictive model using Bayesian methods
#'
#' Given a dataset, the function generates all possible combinations of predictor columns
#' and fits a Bayesian linear regression model for each combination. The function then selects 
#' the best model based on its WAIC (Watanabe-Akaike information criterion) and LOO (leave-one-out) 
#' cross-validation estimates, and returns the fitted model and modified data.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param response_col A string indicating the name of the column that contains the response variable.
#' @param predictor_cols A vector of strings indicating the names of the columns that contain the predictor variables.
#' @param date_col A string indicating the name of the column that contains the date information. If provided,
#'                 the function will add date-related features to the dataset.
#' @param prior A list specifying the prior distribution for the Bayesian model. Default is NULL, which sets
#'              a weakly informative prior by default.
#' @param brm_args A list specifying additional arguments to pass to the brm function. Default is an empty list.
#' 
#' @return A list containing the best model and the modified data.
#' 
#' @examples
#' set.seed(123)
#' n <- 1000
#' date_seq <- seq(as.Date("2023-01-01"), by = "day", length.out = n)
#' sales <- rnorm(n, mean = 100, sd = 20)
#' price <- rnorm(n, mean = 50, sd = 10)
#' advertising <- rnorm(n, mean = 10, sd = 5)
#' region <- sample(c("North", "South", "East", "West"), n, replace = TRUE)
#' 
#' data <- tibble(
#'   date = date_seq,
#'   sales = sales,
#'   price = price,
#'   advertising = advertising,
#'   region = region
#' )
#' 
#' # Test the function with the synthetic data
#' result <- bayesian_feature_selection(data, "sales", c("price", "advertising", "region"), "date")
#' 
#' # Access the best model and modified data
#' best_model <- result$Model
#' data_with_fitted_values <- result$Data
#' 
#' # Display best model
#' print(best_model)
#' 
#' # Display data with fitted values and residuals
#' head(data_with_fitted_values)
#'
#' @import brms
#' @importFrom tidyr mutate
#' @importFrom dplyr select
#' @importFrom stats gaussian
#' @importFrom parallel detectCores
#' @importFrom loo loo
#' @importFrom stats format as.formula
#' 
#' @export

# Save packages
library(brms)
library(tidyverse)
library(parallel)
library(loo)

# Function
bayesian_feature_selection <- function(data, response_col, predictor_cols, date_col,
                                                prior = NULL, brm_args = list()) {
  # Add date-related features if provided
  if (!is.null(date_col)) {
    data <- data %>% mutate(week = as.integer(format(!!sym(date_col), "%U")))
    predictor_cols <- c(predictor_cols, "week")
  }
  
  # Generate all possible combinations of predictor columns
  predictor_combinations <- unlist(lapply(seq_along(predictor_cols),
                                          function(i) combn(predictor_cols, i, simplify = FALSE)), recursive = FALSE)
  
  best_model <- NULL
  best_waic <- Inf
  
  # Number of cores for parallel processing
  n_cores <- detectCores(logical = FALSE)
  
  # Iterate through models with different combinations of predictor variables
  for (predictor_comb in predictor_combinations) {
    predictors <- paste(predictor_comb, collapse = " + ")
    formula_str <- paste0(response_col, " ~ ", predictors)
    formula_sales <- as.formula(formula_str)
    
    # Fit the Bayesian model with increased number of iterations
    model <- tryCatch({
      do.call(brm, modifyList(list(
        formula = formula_sales,
        data = data,
        family = gaussian(),
        prior = prior,
        cores = n_cores,
        iter = 4000
      ), brm_args))
    }, warning = function(w) {
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(model)) {
      cat("Model:", formula_str, "encountered a warning or error.\n\n")
      next
    }
    
    # Calculate WAIC and LOO for model comparison
    loo_est <- loo(model)
    waic_est <- waic(model)
    
    cat("Model:", formula_str, "\n")
    cat("WAIC:", waic_est$estimates["waic"], "\n")
    cat("LOO:", loo_est$estimates["loo"], "\n")
    cat("SE LOO:", loo_est$estimates["se_loo"], "\n\n")
    
    # Update the best model if the current one has a lower WAIC and is not NA
    if (!is.na(waic_est$estimates["waic"]) && waic_est$estimates["waic"] < best_waic) {
      best_model <- model
      best_waic <- waic_est$estimates["waic"]
    }
  }
  
  cat("Best Model:", as.character(formula(best_model)), "\n")
  cat("Best WAIC:", best_waic, "\n")
  
  # Extract the fitted values and residuals from the best model
  fitted_values <- fitted(best_model)
  data$residuals <- residuals
  
  # Calculate model quality metrics
  data$abs_residuals <- abs(residuals)
  data$residuals <- residuals
  
  # Calculate model quality metrics
  data$abs_residuals <- abs(residuals)
  data$squared_residuals <- residuals^2
  mae <- mean(data$abs_residuals)
  rmse <- sqrt(mean(data$squared_residuals))
  
  cat("MAE:", mae, "\n")
  cat("RMSE:", rmse, "\n")
  
  return(list("Model" = best_model, "Data" = data))
}

# Generate synthetic data

set.seed(123)
n <- 1000
date_seq <- seq(as.Date("2023-01-01"), by = "day", length.out = n)
sales <- rnorm(n, mean = 100, sd = 20)
price <- rnorm(n, mean = 50, sd = 10)
advertising <- rnorm(n, mean = 10, sd = 5)
region <- sample(c("North", "South", "East", "West"), n, replace = TRUE)

data <- tibble(
  date = date_seq,
  sales = sales,
  price = price,
  advertising = advertising,
  region = region
)


# Test the function with the synthetic data
result <- bayesian_forecast_feature_selection(data, "sales", c("price", "advertising", "region"), "date")

# Access the best model and modified data
best_model <- result$Model
data_with_fitted_values <- result$Data

# Display best model
print(best_model)

# Display data with fitted values and residuals
head(data_with_fitted_values)