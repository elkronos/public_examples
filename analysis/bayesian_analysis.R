#' ########################### Table of Contents and Description
#' 
#' # LINE 077 - bayesian_analysis: Fits Bayesian regression and classification.
#' # LINE 198 - bayesian_plot: Plots the coefficients and other relevant information of a given Bayesian model
#' # LINE 237 - bayesian_summary: Summarizes the information, priors, and convergence diagnostics of a given Bayesian model
#' # LINE 287 - bayesian_kfold: Prints the k-fold cross-validation results for a given Bayesian model
#' # LINE 326 - bayesian_save: Saves the summary and plot of a given Bayesian model to specified files
#' # LINE 383 - bayesian_predict: Generates predictions from a given Bayesian model for new data
#' # LINE 460 - bayesian_grid: Performs a grid search over multiple sets of parameters to find the best fitting Bayesian model
#' # LINE 533 - bayesian_compare: Compares the performance of multiple Bayesian models using WAIC or LOO criterion
#' # LINE 579 - bayesian_check: Performs posterior predictive checks on a given Bayesian model
#' # LINE 634 - bayesian_tuning: Conducts Bayesian hyperparameter tuning using the rBayesianOptimization package
#' 
#' # Roxygen is provided for each function before the function.

#' Bayesian Analysis with brm
#'
#' This function fits a Bayesian model using the brm function from the brms package.
#' It allows for both regression and classification tasks, as well as k-fold cross-validation.
#'
#' @importFrom stats is_formula
#' @importFrom utils is_data_frame
#' @importFrom brms brm gaussian binomial categorical bernoulli poisson multinomial kfold
#' @importFrom parallel detectCores
#' @importFrom base modifyList
#'
#' @param formula A formula object specifying the model.
#' @param data A data frame containing the variables in the model.
#' @param family A character or brms family object specifying the distribution family of the response variable.
#' @param prior A character string, named list, or data frame specifying the prior distribution(s) of the model parameters.
#' @param chains The number of Markov chains for MCMC sampling.
#' @param cores The number of CPU cores to be used for parallel computation.
#' @param iter The total number of iterations per chain, including warmup.
#' @param warmup The number of warmup iterations per chain.
#' @param seed A positive integer specifying the random seed for reproducibility.
#' @param adapt_delta A value between 0 and 1 controlling the target acceptance rate for MCMC sampling.
#' @param thin The thinning rate for the MCMC samples.
#' @param control A named list of additional settings to control the sampler behavior.
#' @param k_fold An optional positive integer specifying the number of folds for k-fold cross-validation.
#' @param ... Additional arguments passed to the brm function.
#' 
#' @return If k_fold is specified, the function returns a list containing the model and the cross-validation results. Otherwise, it returns the fitted model.
#' 
#' @examples
#' # Regression example
#' \dontrun{
#' library(brms)
#' data(mtcars)
#' 
#' model_regression <- bayesian_analysis(mpg ~ wt + hp, data = mtcars)
#' summary(model_regression)
#' }
#' 
#' # Classification example
#' \dontrun{
#' library(brms)
#' data(iris)
#' 
#' model_classification <- bayesian_analysis(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, family = "categorical")
#' summary(model_classification)
#' }
#' 
#' @export

# Load packages
library(brms)
library(tidyverse)
library(posterior)
library(rBayesianOptimization)

# Helper functions for input validation
is_formula <- function(x) inherits(x, "formula")
is_data_frame <- function(x) inherits(x, "data.frame")
is_brmsfit <- function(x) inherits(x, "brmsfit")

# Save function
bayesian_analysis <- function(formula, data, family = "gaussian", prior = NULL, 
                              chains = 4, cores = parallel::detectCores(), 
                              iter = 2000, warmup = 1000, seed = 1234, 
                              adapt_delta = 0.9, thin = 1, control = list(), k_fold = NULL, ...) {
  
  # Validate inputs
  if (!is_formula(formula)) {
    stop(sprintf("The 'formula' input must be a formula object."))
  }
  
  if (!is_data_frame(data)) {
    stop(sprintf("The 'data' input must be a data frame."))
  }
  
  if (!is.character(family) && !is_brms_family(family)) {
    stop("The 'family' input must be either a character or a brms family object.")
  }
  
  if (!is.null(prior) && !(is.character(prior) || is.list(prior) || is.data.frame(prior))) {
    stop("The 'prior' input must be either a character string, a named list, or a data frame.")
  }
  
  if (!is.numeric(iter) || iter < 1 || round(iter) != iter) {
    stop("The 'iter' input must be a positive integer.")
  }
  
  if (!is.numeric(warmup) || warmup < 1 || round(warmup) != warmup) {
    stop("The 'warmup' input must be a positive integer.")
  }
  
  if (!is.numeric(seed) || seed < 1 || round(seed) != seed) {
    stop("The 'seed' input must be a positive integer.")
  }
  
  if (!is.numeric(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    stop("The 'adapt_delta' input must be a value between 0 and 1.")
  }
  
  if (!is.numeric(thin) || thin < 1 || round(thin) != thin) {
    stop("The 'thin' input must be a positive integer.")
  }
  
  if (!is.null(k_fold) && (!is.numeric(k_fold) || k_fold < 1 || round(k_fold) != k_fold)) {
    stop("The 'k_fold' input must be a positive integer.")
  }
  
  # Define the family object based on the input
  if (is.character(family)) {
    family_obj <- switch(family,
                         "gaussian" = gaussian(),
                         "binomial" = binomial(),
                         "categorical" = categorical(),
                         "bernoulli" = bernoulli(),
                         "poisson" = poisson(),
                         "multinomial" = multinomial(),
                         stop(sprintf("The 'family' input must be one of the following: 'gaussian', 'binomial', 'categorical', 'bernoulli', 'poisson', or 'multinomial'."))
    )
  } else {
    family_obj <- family
  }
  
  # Update control settings if adapt_delta is provided
  if (!is.null(adapt_delta)) {
    control <- modifyList(control, list(adapt_delta = adapt_delta))
  }
  
  # Fit the Bayesian model
  model <- brm(
    formula = formula,
    data = data,
    family = family_obj,
    prior = prior,
    chains = chains,
    cores = cores,
    iter = iter,
    warmup = warmup,
    thin = thin,
    seed = seed,
    control = control,
    ...
  )
  
  # Perform K-fold cross-validation if requested
  if (!is.null(k_fold)) {
    cv_result <- kfold(model, K = k_fold, cores = cores)
    return(list(model = model, cv_result = cv_result))
  }
  
  # Return the model
  return(model)
}


#' Bayesian Plot
#'
#' This function plots a Bayesian model using the brms package. It accepts a brmsfit object
#' and returns a plot with an option to ask the user before proceeding with the next plot.
#'
#' @param model A brmsfit object from the brms package containing the fitted Bayesian model.
#' @return A plot of the Bayesian model.
#' @importFrom brms is_brmsfit plot.brmsfit
#' @examples
#' \dontrun{
#' # Fit a simple Bayesian model using the brms package
#' library(brms)
#' 
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' 
#' # Fit a linear regression model
#' model <- brm(y ~ x, data = data)
#'
#' # Use the bayesian_plot function to visualize the model
#' bayesian_plot(model)
#' }
#' @export

# Function for plotting the Bayesian model
bayesian_plot <- function(model) {
  if (!is_brmsfit(model)) {
    stop("The 'model' input must be a brmsfit object.")
  }
  
  p <- plot(model, ask = TRUE)
  
  return(p)
}


#' Bayesian Model Summary
#'
#' This function provides a detailed summary of a Bayesian model fitted using the brms package. 
#' It prints the model summary, family, priors, and convergence diagnostics.
#'
#' @param model A brmsfit object from the brms package containing the fitted Bayesian model.
#' @importFrom brms is_brmsfit get_prior
#' @importFrom posterior summarize_draws
#' @examples
#' \dontrun{
#' # Fit a simple Bayesian model using the brms package
#' library(brms)
#' 
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' 
#' # Fit a linear regression model
#' model <- brm(y ~ x, data = data)
#'
#' # Use the bayesian_summary function to print the model summary
#' bayesian_summary(model)
#' }
#' @export

# Function for summarizing the Bayesian model
bayesian_summary <- function(model) {
  if (!is_brmsfit(model)) {
    stop("The 'model' input must be a brmsfit object.")
  }
  
  message("Model summary:")
  print(summary(model))
  
  message("\nFamily:")
  print(model$family)
  
  message("\nPriors:")
  print(get_prior(formula = model$formula, data = model$data))
  
  message("\nConvergence diagnostics:")
  print(posterior::summarize_draws(model))
}


#' Bayesian K-Fold Cross-Validation Results
#'
#' This function prints the cross-validation results of a Bayesian model fitted using the brms package
#' and validated using the kfold method from the loo package.
#'
#' @param cv_result A kfold object from the loo package containing the cross-validation results.
#' @importFrom loo kfold
#' @examples
#' \dontrun{
#' # Fit a simple Bayesian model using the brms package
#' library(brms)
#' library(loo)
#'
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#'
#' # Fit a linear regression model
#' model <- brm(y ~ x, data = data)
#'
#' # Perform K-fold cross-validation
#' cv_result <- kfold(model)
#'
#' # Use the bayesian_kfold function to print the cross-validation results
#' bayesian_kfold(cv_result)
#' }
#' @export

# Get validation results
bayesian_kfold <- function(cv_result) {
  if (!inherits(cv_result, "kfold")) {
    stop("The 'cv_result' input must be a kfold object.")
  }
  
  message("\nCross-validation results:")
  print(cv_result)
}


#' Save Bayesian Model Output
#'
#' This function saves the summary and plot of a Bayesian model fitted using the brms package
#' to separate files. The summary is saved as a text file, while the plot is saved as a PDF.
#'
#' @param model A brmsfit object from the brms package containing the fitted Bayesian model.
#' @param summary_filename The name of the text file to save the model summary.
#' @param plot_filename The name of the PDF file to save the model plot.
#' @importFrom brms is_brmsfit
#' @examples
#' \dontrun{
#' # Fit a simple Bayesian model using the brms package
#' library(brms)
#'
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#'
#' # Fit a linear regression model
#' model <- brm(y ~ x, data = data)
#'
#' # Use the bayesian_save function to save the summary and plot
#' bayesian_save(model, "model_summary.txt", "model_plot.pdf")
#' }
#' @export

# Save output
bayesian_save <- function(model, summary_filename, plot_filename) {
  if (!is_brmsfit(model)) {
    stop("The 'model' input must be a brmsfit object.")
  }
  
  # Save summary information to a text file
  sink(file = summary_filename)
  bayesian_summary(model)
  sink()
  
  # Save the plot to a PDF
  pdf(file = plot_filename, width = 8, height = 11)
  p <- plot_bayesian_model(model)
  print(p)
  dev.off()
}


#' Generate Predictions from Bayesian Model
#'
#' This function generates predictions from a Bayesian model fitted using the brms package.
#' The predictions can be returned as a response or linear predictions, and can be optionally saved to a CSV file.
#'
#' @param model A brmsfit object from the brms package containing the fitted Bayesian model.
#' @param newdata A data frame containing the new data for prediction.
#' @param predict_type The type of prediction to return, either 'response' or 'linear' (default: 'response').
#' @param save_to_csv Logical, whether to save predictions to a CSV file (default: FALSE).
#' @param csv_filename The name of the CSV file to save the predictions, if save_to_csv is TRUE.
#' @return A matrix of predictions.
#' @importFrom brms is_brmsfit
#' @importFrom tibble is_data_frame
#' @importFrom posterior quantile2 mean
#' @examples
#' \dontrun{
#' # Fit a simple Bayesian model using the brms package
#' library(brms)
#'
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#'
#' # Fit a linear regression model
#' model <- brm(y ~ x, data = data)
#'
#' # Generate new data for prediction
#' newdata <- data.frame(
#'   x = rnorm(20)
#' )
#'
#' # Use the bayesian_predict function to get predictions
#' predictions <- bayesian_predict(model, newdata, predict_type = "response", save_to_csv = TRUE, csv_filename = "predictions.csv")
#' }
#' @export

# Function for generating predictions from the Bayesian model
bayesian_predict <- function(model, newdata, predict_type = "response", save_to_csv = FALSE, csv_filename = "predictions.csv") {
  if (!is_brmsfit(model)) {
    stop("The 'model' input must be a brmsfit object.")
  }
  
  if (!is_data_frame(newdata)) {
    stop(sprintf("The 'newdata' input must be a data frame."))
  }
  
  if (!is.character(predict_type) || !(predict_type %in% c("response", "linear"))) {
    stop("The 'predict_type' input must be either 'response' or 'linear'.")
  }
  
  predictions <- predict(model, newdata = newdata, summary = FALSE, allow_new_levels = TRUE)
  
  if (predict_type == "response") {
    if (inherits(model$family, "categorical")) {
      predictions <- apply(predictions, 2, function(x) posterior::quantile2(x, probs = c(0.5)))
    } else {
      predictions <- apply(predictions, 1, function(x) posterior::quantile2(x, probs = c(0.5)))
    }
  } else {
    predictions <- apply(predictions, 1, function(x) posterior::mean(x))
  }
  
  if (save_to_csv) {
    write.csv(predictions, file = csv_filename)
  }
  
  return(predictions)
}


#' Bayesian Grid Search
#'
#' This function performs a grid search over multiple sets of parameters for fitting Bayesian models
#' using the brms package. The grid search evaluates different combinations of priors, chains, iterations,
#' warmup periods, seeds, and more.
#'
#' @param formula A formula describing the model structure.
#' @param data A data frame containing the data for model fitting.
#' @param family The response distribution family (default: 'gaussian').
#' @param search_params A list of named lists or data frames containing parameters for bayesian_analysis().
#' @param k_fold The number of folds for k-fold cross-validation (default: 5).
#' @param cores The number of CPU cores to use for parallel processing (default: number of available cores).
#' @return A list containing the results of the grid search.
#' @importFrom parallel detectCores
#' @examples
#' \dontrun{
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#'
#' # Define the model formula
#' formula <- y ~ x
#'
#' # Define the search parameters
#' search_params <- list(
#'   list(prior = set_prior("normal(0, 1)", class = "b")),
#'   list(prior = set_prior("student_t(3, 0, 1)", class = "b")),
#'   list(iter = 3000, warmup = 1500)
#' )
#'
#' # Perform the Bayesian grid search
#' grid_search_results <- bayesian_grid(
#'   formula = formula,
#'   data = data,
#'   family = "gaussian",
#'   search_params = search_params,
#'   k_fold = 5
#' )
#' }
#' @export

# Function for performing a grid search over multiple sets of parameters
bayesian_grid <- function(formula, data, family = "gaussian", search_params, k_fold = 5, cores = parallel::detectCores()) {
  
  if (!is.list(search_params)) {
    stop("The 'search_params' input must be a list of named lists or data frames containing parameters for bayesian_analysis().")
  }
  
  # Check if each element in search_params is a list or a data frame
  if (!all(unlist(lapply(search_params, function(x) is.list(x) || is.data.frame(x))))) {
    stop("Each set of parameters in 'search_params' must be a named list or a data frame.")
  }
  
  models <- list()
  counter <- 1
  total_params <- length(search_params)
  
  for (params in search_params) {
    message(sprintf("Fitting model %d of %d", counter, total_params))
    
    current_result <- bayesian_analysis(
      formula = formula,
      data = data,
      family = family,
      prior = ifelse(is.null(params$prior), NULL, params$prior),
      chains = ifelse(is.null(params$chains), 4, params$chains),
      cores = cores,
      iter = ifelse(is.null(params$iter), 2000, params$iter),
      warmup = ifelse(is.null(params$warmup), 1000, params$warmup),
      seed = ifelse(is.null(params$seed), 1234, params$seed),
      adapt_delta = ifelse(is.null(params$adapt_delta), 0.9, params$adapt_delta),
      thin = ifelse(is.null(params$thin), 1, params$thin),
      control = ifelse(is.null(params$control), list(), params$control),
      k_fold = k_fold
    )
    
    models[[counter]] <- current_result
    counter <- counter + 1
  }
  
  return(models)
}


#' Bayesian Model Comparison
#'
#' This function compares multiple Bayesian models fitted using the brms package based on
#' the Watanabe-Akaike Information Criterion (WAIC) or the Leave-One-Out (LOO) Cross-Validation criterion.
#'
#' @param models A list of brmsfit objects to compare.
#' @param comparison_criterion The criterion for model comparison, either 'WAIC' or 'LOO' (default: 'WAIC').
#' @return A data frame containing the comparison results.
#' @importFrom loo waic_compare loo_compare
#' @importFrom brms is_brmsfit
#' @examples
#' \dontrun{
#' # Fit a couple of Bayesian models using the brms package
#' library(brms)
#'
#' # Prepare some example data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#'
#' # Fit two linear regression models with different priors
#' model1 <- brm(y ~ x, data = data, prior = set_prior("normal(0, 1)", class = "b"))
#' model2 <- brm(y ~ x, data = data, prior = set_prior("student_t(3, 0, 1)", class = "b"))
#'
#' # Use the bayesian_compare function to compare models
#' comparison <- bayesian_compare(models = list(model1, model2), comparison_criterion = "WAIC")
#' }
#' @export

# Function for comparing Bayesian models
bayesian_compare <- function(models, comparison_criterion = "WAIC") {
  if (!is.list(models) || !all(sapply(models, is_brmsfit))) {
    stop("The 'models' input must be a list of brmsfit objects.")
  }
  
  if (!is.character(comparison_criterion) || !(comparison_criterion %in% c("WAIC", "LOO"))) {
    stop("The 'comparison_criterion' input must be either 'WAIC' or 'LOO'.")
  }
  
  if (comparison_criterion == "WAIC") {
    model_comparison <- loo::waic_compare(models)
  } else {
    model_comparison <- loo::loo_compare(models)
  }
  
  return(model_comparison)
}

#' Perform posterior predictive checks on a Bayesian model
#'
#' This function generates a posterior predictive check plot for a given Bayesian model
#' using the 'bayesplot' package.
#'
#' @param model A 'brmsfit' object representing the Bayesian model to check.
#' @importFrom brms is_brmsfit
#' @importFrom bayesplot ppc_dens_overlay
#' @importFrom stats as.character
#' @return A 'bayesplot' object representing the posterior predictive check plot.
#' @examples
#' ## Generate some fake data
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2*x + rnorm(100)
#' 
#' ## Fit a Bayesian linear regression model
#' library(brms)
#' model <- brm(y ~ x, chains = 2, iter = 2000)
#'
#' ## Perform a posterior predictive check
#' bayesian_check(model)
#'
#' @seealso \code{\link{posterior_predict}}
#' @keywords Bayesian, posterior predictive check
#' @export

# Function for performing posterior predictive checks
bayesian_check <- function(model) {
  if (!is_brmsfit(model)) {
    stop("The 'model' input must be a brmsfit object.")
  }
  
  # Extract the response variable from the model's formula
  response_variable <- as.character(model$formula[[2]])
  
  # Generate the posterior predictive check plot
  ppc_plot <- bayesplot::ppc_dens_overlay(y = model$data[[response_variable]], yrep = posterior_predict(model))
  
  return(ppc_plot)
}


#' Perform Bayesian hyperparameter tuning using cross-validation
#'
#' This function performs Bayesian hyperparameter tuning for a given Bayesian model 
#' using the 'rBayesianOptimization' package and cross-validation.
#'
#' @param formula A 'formula' object specifying the model to fit.
#' @param data A 'data.frame' containing the data used for model fitting.
#' @param family A character string specifying the distribution family to use in the model.
#' @param bounds A named list of numeric vectors containing the lower and upper bounds for each hyperparameter to tune.
#' @param k_fold An integer specifying the number of folds to use in cross-validation.
#' @param n_iter An integer specifying the maximum number of iterations to run the Bayesian optimization algorithm.
#' @param n_init An integer specifying the number of initial random points to use in the Bayesian optimization algorithm.
#' @param ... Additional arguments to be passed to the 'BayesianOptimization' function.
#' @importFrom rBayesianOptimization BayesianOptimization
#' @return A list object containing the results of the Bayesian hyperparameter tuning.
#' @examples
#' ## Generate some fake data
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2*x + rnorm(100)
#' data <- data.frame(x = x, y = y)
#'
#' ## Perform Bayesian hyperparameter tuning
#' library(brms)
#' bounds <- list(c(1, 100), c(1e-10, 0.5))
#' result <- bayesian_tuning(
#'   formula = y ~ x,
#'   data = data,
#'   family = "gaussian",
#'   bounds = bounds,
#'   k_fold = 3,
#'   n_iter = 10,
#'   n_init = 5
#' )
#' result
#'
#' @keywords Bayesian, hyperparameter tuning, cross-validation
#' @export

# Function for Bayesian hyperparameter tuning
bayesian_tuning <- function(formula, data, family = "gaussian", bounds, k_fold = 5, n_iter = 30, n_init = 10, ...) {
  
  # Define the objective function
  bayesian_objective_function <- function(iter, adapt_delta) {
    result <- bayesian_analysis(
      formula = formula,
      data = data,
      family = family,
      iter = as.integer(iter),
      adapt_delta = adapt_delta,
      k_fold = k_fold
    )
    
    return(list(Score = -result$cv_result$elpd_loo, Pred = result$cv_result$elpd_loo_se))
  }
  
  if (!is.list(bounds)) {
    stop("The 'bounds' input must be a named list of numeric vectors containing the lower and upper bounds for each hyperparameter.")
  }
  
  result <- rBayesianOptimization::BayesianOptimization(
    FUN = bayesian_objective_function,
    bounds = bounds,
    init_points = n_init,
    n_iter = n_iter,
    ...
  )
  
  return(result)
}