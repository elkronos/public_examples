# Load necessary libraries
library(tidymodels)
library(parsnip)
library(stacks)
library(dplyr)
library(tryCatchLog)


#' Create a Regression Model
#'
#' This function creates a regression model based on the specified model type
#' and optional additional parameters.
#'
#' @param model_type A character string specifying the type of regression model.
#'                   Must be one of "OLS" (Ordinary Least Squares), "Ridge", "Lasso",
#'                   or "Elastic Net".
#' @param extra_params A list of additional parameters to be passed to the model
#'                    constructor function.
#'
#' @return A regression model object.
#'
#' @importFrom tidymodels linear_reg
#' @importFrom yardstick set_engine
#' @importFrom glmnet set_engine
#'
#' @examples
#' # Create an OLS regression model
#' ols_model <- define_model("OLS")
#'
#' # Create a Ridge regression model with custom parameters
#' ridge_params <- list(penalty = 0.05, mixture = 0.3)
#' ridge_model <- define_model("Ridge", extra_params = ridge_params)
#'
#' # Create a Lasso regression model with custom parameters
#' lasso_params <- list(penalty = 0.1, mixture = 1)
#' lasso_model <- define_model("Lasso", extra_params = lasso_params)
#'
#' # Create an Elastic Net regression model
#' enet_model <- define_model("Elastic Net")
#'
#' @seealso \code{\link{linear_reg}}, \code{\link{set_engine}}
#'
#' @export
define_model <- function(model_type, extra_params = list()) {
  valid_types <- c("OLS", "Ridge", "Lasso", "Elastic Net")
  model_type <- match.arg(model_type, choices = valid_types)
  
  if (model_type == "OLS") {
    model <- linear_reg() %>% set_engine("lm")
  } else {
    penalty_value <- ifelse(model_type == "Ridge", 0.1, ifelse(model_type == "Lasso", 0.1, 0.1))
    mixture_value <- ifelse(model_type == "Lasso", 1, ifelse(model_type == "Ridge", 0, 0.5))
    model <- linear_reg(penalty = penalty_value, mixture = mixture_value) %>% set_engine("glmnet")
  }
  if (length(extra_params) > 0) {
    model <- do.call(model, extra_params)
  }
  return(model)
}


#' Create a workflow
#'
#' This function creates a workflow for modeling tasks by combining a model and a formula.
#'
#' @param model A modeling object, such as a regression model or a machine learning model.
#' @param formula A formula specifying the modeling formula to be used.
#'
#' @return A workflow object that can be further customized and used for modeling tasks.
#'
#' @import {tidymodels}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' model <- linear_reg()  # Replace with your chosen model
#' formula <- formula(mpg ~ wt + hp)  # Replace with your formula
#' wf <- create_workflow(model, formula)
#' }
#'
#' @seealso \code{\link{add_model}}, \code{\link{add_formula}}
#'
#' @export
create_workflow <- function(model, formula) {
  workflow() %>%
    add_model(model) %>%
    add_formula(formula)
}


#' Fit a single model with error handling
#'
#' This function fits a single model to the provided data with error handling. It creates a workflow, defines a model,
#' and fits the model to the data.
#'
#' @param model_type A character string specifying the type of model to be used, e.g., "linear" or "tree".
#' @param data A data frame containing the dataset for modeling.
#' @param response A character string specifying the response variable.
#' @param predictors A character vector specifying the predictor variables.
#'
#' @return A fitted model object if successful, or NULL if an error occurs during the modeling process.
#'
#' @import {tidymodels}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' model_type <- "linear"  # Replace with your chosen model type
#' data <- your_data_frame  # Replace with your dataset
#' response <- "target_variable"  # Replace with your response variable
#' predictors <- c("predictor1", "predictor2")  # Replace with your predictor variables
#' result <- fit_model(model_type, data, response, predictors)
#' if (!is.null(result)) {
#'   # Model fitting was successful, you can further analyze the model here
#' } else {
#'   # Handle the error as needed
#' }
#' }
#'
#' @export
fit_model <- function(model_type, data, response, predictors) {
  tryCatch({
    model <- define_model(model_type)
    formula <- reformulate(predictors, response)
    create_workflow(model, formula) %>%
      fit(data)
  }, error = function(e) {
    cat("Error in fit_model with", model_type, ":", e$message, "\n")
    NULL
  })
}


#' Tune a model with customizable metrics
#'
#' This function tunes a predictive model with customizable evaluation metrics using resampling techniques.
#'
#' @param model A modeling algorithm to be tuned.
#' @param train_data The training dataset.
#' @param response The name of the response variable.
#' @param predictors A character vector of predictor variable names.
#' @param resamples A resampling object, such as a cross-validation object created with `vfold_cv()`.
#' @param metric The evaluation metric to optimize. Default is "rmse".
#'
#' @return A tuned workflow that represents the best model based on the specified metric.
#'
#' @import dplyr
#' @import recipes
#' @import tune
#' @import workflows
#'
#' @examples
#' \dontrun{
#' # Example usage
#' train_data <- data.frame(
#'   x1 = rnorm(100),
#'   x2 = rnorm(100),
#'   response = rnorm(100)
#' )
#'
#' resamples <- vfold_cv(train_data, v = 5)
#' tuned_workflow <- tune_model(
#'   model = lm(),
#'   train_data = train_data,
#'   response = "response",
#'   predictors = c("x1", "x2"),
#'   resamples = resamples,
#'   metric = "rmse"
#' )
#' }
#'
#' @seealso \code{\link{create_workflow}}, \code{\link{tune_grid}}, \code{\link{select_best}}, \code{\link{finalize_workflow}}
#'
#' @export
tune_model <- function(model, train_data, response, predictors, resamples, metric = "rmse") {
  formula <- reformulate(predictors, response)
  workflow <- create_workflow(model, formula)
  
  # Define a tuning grid
  grid <- grid_regular(penalty(range = c(-5, -1)), 
                       mixture(),
                       levels = 5)
  
  # Perform the tuning
  tuned_results <- tune_grid(
    workflow, 
    resamples = resamples, 
    grid = grid,
    metrics = metric_set(metric)
  )
  
  # Select the best model
  best_model <- select_best(tuned_results, metric)
  return(finalize_workflow(workflow, best_model))
}


#' Average Ensemble
#'
#' Performs an ensemble by averaging predictions from a list of models on the provided test data.
#' Supports mean, median, and weighted average methods.
#'
#' @param models A list of trained models for which predictions will be averaged.
#' @param test_data The data on which to make predictions.
#' @param method The method of averaging to use: "mean" or "median". 
#'               Defaults to "mean". If "mean" is selected and weights are provided,
#'               a weighted average is computed.
#' @param weights Optional vector of weights for a weighted average. 
#'               If provided, the length must match the number of models.
#'               Weights are normalized to sum to 1.
#'
#' @return A vector of predictions obtained by the specified method of averaging predictions 
#'         from the input models. If "mean" is selected, returns the (weighted) mean of predictions.
#'         If "median" is selected, returns the median of predictions.
#'
#' @seealso \code{\link{ensemble_models}}, \code{\link{weighted_average_ensemble}}, 
#'         \code{\link{bagging_ensemble}}}
#'
#' @examples
#' \dontrun{
#'   # Create example data
#'   set.seed(123)
#'   test_data <- data.frame(x = rnorm(50))
#'   models <- list(model1 = lm(y ~ x, data = train_data), model2 = glm(y ~ x, data = train_data))
#'   
#'   # Ensemble using mean method
#'   average_ensemble(models, test_data)
#'
#'   # Ensemble using median method
#'   average_ensemble(models, test_data, method = "median")
#'
#'   # Ensemble using weighted mean method
#'   weights <- c(0.5, 0.5)  # Example weights
#'   average_ensemble(models, test_data, method = "mean", weights = weights)
#' }
#'
#' @export
average_ensemble <- function(models, test_data, method = "mean", weights = NULL) {
  predictions <- lapply(models, predict, new_data = test_data)
  pred_matrix <- do.call(cbind, predictions)
  
  # Check for custom weights
  if (!is.null(weights)) {
    if (length(weights) != length(models)) {
      stop("The length of weights must match the number of models.")
    }
    # Normalizing weights
    normalized_weights <- weights / sum(weights)
  } else {
    normalized_weights <- rep(1 / length(models), length(models))
  }
  
  # Apply specified method
  if (method == "mean") {
    return(rowSums(pred_matrix * normalized_weights) / sum(normalized_weights))
  } else if (method == "median") {
    return(apply(pred_matrix, 1, median))
  } else {
    stop("Invalid method specified. Choose 'mean' or 'median'.")
  }
}


#' Calculate Model Weights Based on Performance
#'
#' Computes weights for a set of models based on their performance. The function calculates 
#' the Root Mean Squared Error (RMSE) for each model on the test data, then uses the inverse 
#' of these RMSE values as weights. Weights are normalized so that they sum to 1. Models with 
#' lower RMSE will have higher weights.
#'
#' @param model_results A list of trained models.
#' @param test_data The test data used for making predictions and calculating RMSE.
#' @param response The name of the response variable in the test data.
#'
#' @return A numeric vector of normalized weights for the models.
#'
#' @examples
#' \dontrun{
#'   # Assume model_results is a list of trained models
#'   # and test_data is the dataset for testing
#'   # response is the name of the response variable in test_data
#'   weights <- calculate_weights(model_results, test_data, "response")
#' }
#'
#' @export
calculate_weights <- function(model_results, test_data, response) {
  # Calculate RMSE for each model
  model_rmse <- sapply(model_results, function(model) {
    if (!is.null(model)) {
      predictions <- predict(model, new_data = test_data)$.pred
      sqrt(mean((predictions - test_data[[response]])^2))
    } else {
      NA_real_
    }
  })
  
  # Inverse RMSE for weights (handling NA values)
  inverse_rmse <- ifelse(is.na(model_rmse), 0, 1 / model_rmse)
  
  # Normalize weights
  normalized_weights <- inverse_rmse / sum(inverse_rmse)
  return(normalized_weights)
}


#' Weighted Average Ensemble
#'
#' Computes a weighted average of predictions from a list of models on the provided test data.
#' If weights are not specified, equal weights are assigned to all models. The function 
#' normalizes the weights so that they sum to 1, ensuring a proper weighted average calculation.
#'
#' @param models A list of trained models for which predictions will be weighted and averaged.
#' @param test_data The data on which to make predictions.
#' @param weights An optional numeric vector of weights for the models when computing 
#'        the weighted average. If not provided, equal weights are used. The length 
#'        of the weights vector should match the number of models.
#'
#' @return A vector of predictions obtained by taking a weighted average of predictions 
#'         from the input models. If weights are not provided, it defaults to an 
#'         equal-weighted average.
#'
#' @seealso \code{\link{ensemble_models}}, \code{\link{average_ensemble}}, 
#'         \code{\link{bagging_ensemble}}}
#'
#' @examples
#' \dontrun{
#'   # Create example data
#'   set.seed(123)
#'   test_data <- data.frame(x = rnorm(50))
#'   models <- list(model1 = lm(y ~ x, data = train_data), model2 = glm(y ~ x, data = train_data))
#'   
#'   # Ensemble using weighted average method with custom weights
#'   weights <- c(0.6, 0.4)  # Example weights
#'   weighted_average_ensemble(models, test_data, weights)
#' }
#'
#' @export
weighted_average_ensemble <- function(models, test_data, weights) {
  predictions <- lapply(models, predict, new_data = test_data)
  pred_matrix <- do.call(cbind, predictions)
  return(rowSums(pred_matrix * weights) / sum(weights))
}

#' Bagging Ensemble
#'
#' This function performs an ensemble by taking the simple average of predictions from a list of models on the provided test data.
#'
#' @param models A list of trained models for which predictions will be averaged.
#' @param test_data The data on which to make predictions.
#'
#' @return A vector of predictions obtained by taking the average of predictions from the input models.
#'
#' @seealso \code{\link{ensemble_models}}, \code{\link{average_ensemble}}, \code{\link{weighted_average_ensemble}}
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' test_data <- data.frame(x = rnorm(50))
#' models <- list(model1 = lm(y ~ x, data = train_data), model2 = glm(y ~ x, data = train_data))
#' 
#' # Ensemble using bagging method
#' bagging_ensemble(models, test_data)
#'
#' @export
bagging_ensemble <- function(models, test_data) {
  predictions <- lapply(models, predict, new_data = test_data)
  return(rowMeans(do.call(cbind, predictions)))
}


#' Ensemble Models
#'
#' This function ensembles multiple models using the specified method and returns predictions on test data.
#'
#' @param models A list of trained models to be ensembled.
#' @param method The ensemble method to use. Options include "average", "weighted_average", and "bagging".
#' @param test_data The data on which to make predictions.
#' @param train_data Optional. The training data used for some ensemble methods.
#' @param response Optional. The response variable used for some ensemble methods.
#' @param weights Optional. Weights for models when using "weighted_average".
#'
#' @return A vector of predictions based on the specified ensemble method.
#'
#' @importFrom package_name1 function1 function2
#' @importFrom package_name2 function3 function4
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' train_data <- data.frame(x = rnorm(100), y = rnorm(100))
#' test_data <- data.frame(x = rnorm(50))
#' models <- list(model1 = lm(y ~ x, data = train_data), model2 = glm(y ~ x, data = train_data))
#' 
#' # Ensemble using weighted_average method
#' ensemble_models(models, method = "weighted_average", test_data, train_data, response = "y", weights = c(0.6, 0.4))
#' 
#' # Ensemble using bagging method
#' ensemble_models(models, method = "bagging", test_data)
#'
#' @seealso \code{\link{average_ensemble}}, \code{\link{weighted_average_ensemble}}, \code{\link{bagging_ensemble}}
#'
#' @export
ensemble_models <- function(models, method, test_data, train_data = NULL, response = NULL, weights = NULL) {
  switch(method,
         "average" = average_ensemble(models, test_data),
         "weighted_average" = weighted_average_ensemble(models, test_data, weights),
         "bagging" = bagging_ensemble(models, test_data),
         stop("Invalid method specified"))
}


#' Run Models with Logging and Progress
#'
#' This function trains a set of machine learning models on the provided data and returns the results. It also supports model tuning and ensembling.
#'
#' @param data The dataset containing predictor variables.
#' @param response The response variable to predict.
#' @param predictors The names of predictor variables.
#' @param models A character vector of model types to be trained.
#' @param v_fold The number of folds for cross-validation (default is 5).
#' @param grid_size The number of hyperparameter combinations to search when tuning models (default is 10).
#' @param method The ensemble method to use if multiple models are trained (default is NULL).
#'
#' @return A list of model results or, if an ensemble method is specified, a vector of ensemble predictions.
#'
#' @importFrom package_name1 function1 function2
#' @importFrom package_name2 function3 function4
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' predictors <- c("predictor1", "predictor2")
#' models <- c("lm", "rf", "xgboost")
#'
#' # Train models without ensembling
#' run_models(data, response = "response", predictors, models)
#'
#' # Train models with ensembling
#' run_models(data, response = "response", predictors, models, method = "bagging")
#'
#' @seealso \code{\link{ensemble_models}}, \code{\link{define_model}}, \code{\link{tune_model}}, \code{\link{fit_model}}
#'
#' @export
run_models <- function(data, response, predictors, models, v_fold = 5, grid_size = 10, method = NULL) {
  set.seed(123)
  data_split <- initial_split(data)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  cv_splits <- vfold_cv(train_data, v = v_fold)
  
  model_results <- list()
  
  for (model_type in models) {
    cat("Fitting model:", model_type, "\n")
    model <- define_model(model_type)
    
    if (model_type %in% c("Ridge", "Lasso", "Elastic Net")) {
      cat("Tuning model:", model_type, "\n")
      tuned_model <- tune_model(model, train_data, response, predictors, cv_splits, "rmse", grid_size)
      model_results[[model_type]] <- fit_model(tuned_model, train_data, response, predictors)
    } else {
      model_results[[model_type]] <- fit_model(model_type, train_data, response, predictors)
    }
  }
  
  if (!is.null(method) && length(models) > 1) {
    cat("Implementing", method, "method\n")
    ensemble_results <- ensemble_models(model_results, method, test_data)
    return(ensemble_results)
  }
  
  return(model_results)
}


#' @title Example Code for Model Training and Testing (Do not run)
#' @description This code generates an example dataset, defines models and ensembling methods to test, and runs various tests for model training and evaluation. These tests include individual model training, ensembling, error handling, and performance metric calculation.
#' @details This is example code and is not meant to be executed as a script.
#' @seealso \code{\link{fit_model}}, \code{\link{ensemble_models}}, \code{\link{rmse}}
#' @importFrom tibble tibble
#' @importFrom stats set.seed
#' @importFrom tidymodels initial_split training testing vfold_cv
#' @importFrom caret tryCatch
#'
#' @examples
#' \dontrun{
#' # Generate an example dataset
#' set.seed(123) # For reproducibility
#' data <- tibble(
#'   response = rnorm(100),
#'   predictor1 = rnorm(100),
#'   predictor2 = rnorm(100),
#'   predictor3 = rnorm(100)
#' )
#'
#' # Define response and predictors
#' response <- "response"
#' predictors <- c("predictor1", "predictor2", "predictor3")
#'
#' # Define models and ensembling methods to test
#' models_to_test <- c("OLS", "Ridge", "Lasso", "Elastic Net")
#' ensembling_methods <- c("average", "weighted_average", "bagging")
#'
#' # Function to test error handling
#' test_error_handling <- function() {
#'   # Intentionally causing an error
#'   tryCatch({
#'     fit_model("InvalidModelType", data, response, predictors)
#'   }, error = function(e) {
#'     return(e$message)
#'   })
#' }
#'
#' # Function to test performance metrics
#' test_performance_metrics <- function(model, data, response) {
#'   # Check if the model is not NULL
#'   if (is.null(model)) {
#'     return(NA_real_)
#'   }
#'
#'   # Make predictions using the model
#'   predictions <- predict(model, new_data = data)
#'
#'   # Create a new dataframe with actuals and predictions
#'   results_df <- data.frame(
#'     actual = data[[response]],
#'     predicted = predictions$.pred
#'   )
#'
#'   # Calculate RMSE
#'   rmse_val <- rmse(results_df, truth = actual, estimate = predicted)
#'   return(rmse_val)
#' }
#'
#' # Running tests
#' test_results <- list()
#'
#' # Test each model individually
#' test_results$Individual_Models <- lapply(models_to_test, function(model_type) {
#'   fit_model(model_type, data, response, predictors)
#' })
#'
#' # Test each model with each ensembling method
#' test_results$Ensembled_Models <- lapply(ensembling_methods, function(method) {
#'   ensemble_models(test_results$Individual_Models, method, data)
#' })
#'
#' # Test error handling
#' test_results$Error_Handling <- test_error_handling()
#'
#' # Running the performance metrics test
#' test_results$Performance_Metrics <- lapply(test_results$Individual_Models, function(model) {
#'   test_performance_metrics(model, data, response)
#' })
#'
#' # Output test results
#' test_results
#' }