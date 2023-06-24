# Load necessary packages
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(cowplot)
library(purrr)
library(rstan)
library(loo)

#' Generate Empirical Bayes Priors
#'
#' This function generates empirical Bayes priors based on the given data.
#'
#' @param data The data frame containing the variables 'x' and 'y'.
#'
#' @return A list of two prior distributions: one for the coefficient of 'x'
#'   and another for the intercept term. Each prior is a Student's t-distribution
#'   with 3 degrees of freedom, centered at the corresponding coefficient from
#'   the prior model, and scaled by the corresponding standard error.
#'
#' @importFrom stats lm vcov coef sqrt
#' @importFrom stats:::student_t student_t
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(2, 4, 6))
#' priors <- empirical_bayes_priors(data)
#' prior_x <- priors[[1]]
#' prior_intercept <- priors[[2]]
#'
#' @export
empirical_bayes_priors <- function(data) {
  prior_model <- lm(y ~ x, data = data)
  prior_coefs <- coef(prior_model)
  prior_ses <- sqrt(diag(vcov(prior_model)))
  return(list(student_t(3, prior_coefs[2], prior_ses[2]), student_t(3, prior_coefs[1], prior_ses[1])))
}

#' Set Priors
#'
#' This function sets priors for a linear regression model.
#'
#' @param mean_intercept The mean value for the intercept prior. Default is 0.
#' @param sd_intercept The standard deviation for the intercept prior. Default is 100.
#' @param mean_slope The mean value for the slope prior. Default is 0.
#' @param sd_slope The standard deviation for the slope prior. Default is 100.
#'
#' @return A list of two prior distributions: one for the intercept and one for the slope.
#' The prior distributions are t-distributions with 3 degrees of freedom.
#'
#' @importFrom stats student_t
#'
#' @examples
#' set_priors(mean_intercept = 1, sd_intercept = 10, mean_slope = 2, sd_slope = 20)
#'
#' @export
set_priors <- function(mean_intercept = 0, sd_intercept = 100, mean_slope = 0, sd_slope = 100) {
  list(student_t(3, mean_intercept, sd_intercept), student_t(3, mean_slope, sd_slope))
}

#' Fit model with specified prior
#'
#' This function fits a model using the stan_glm function from the rstan package,
#' with an optional user-specified prior.
#'
#' @param prior The prior distribution for the coefficients and intercept.
#'              If NULL, default priors are used.
#' @param data The data frame containing the variables 'y' and 'x'.
#'
#' @return A fitted model object of class 'stanfit' obtained from stan_glm.
#'
#' @import rstan
#' @importFrom stats gaussian
#' @importFrom stats print
#' @importFrom stats summary
#' @importFrom stats is.null
#' @importFrom stats if
#' @importFrom stats else
#' @importFrom stats list
#' @importFrom stats student_t
#'
#' @examples
#' data <- data.frame(y = rnorm(100), x = rnorm(100))
#' fit_model_with_prior(prior = NULL, data = data)
#'
#' @export
fit_model_with_prior <- function(prior, data) {
  fit <- stan_glm("y ~ x", data = data, family = gaussian(),
                  prior = if(is.null(prior)) student_t(3, 0, 2.5) else prior[[1]],
                  prior_intercept = if(is.null(prior)) student_t(3, 0, 2.5) else prior[[2]],
                  chains = 4, iter = 4000, seed = 1234,
                  control = list(adapt_delta = 0.95))
  
  # Print the summary of the fit, including R-hat values
  print(summary(fit))
  
  return(fit)
}

#' Generate Trace Plot
#'
#' This function generates a trace plot for a given Stan model fit.
#'
#' @param fit The Stan model fit object.
#' @param model_name A character string specifying the name of the model.
#'
#' @import ggplot2
#' @import rstan
#' @importFrom ggplot2 ggtitle
#' @importFrom rstan stan_trace
#'
#' @return A plot object displaying the trace plot.
#'
#' @examples
#' fit <- stan(fit = model, ...)
#' trace_plot <- generate_trace_plot(fit, "Model Name")
#' print(trace_plot)
#'
#' @export
generate_trace_plot <- function(fit, model_name) {
  plot <- stan_trace(fit) + ggtitle(paste("Trace Plot:", model_name))
  return(plot)
}

#' Generate Plot
#'
#' This function generates a plot based on the provided model fit and prior label.
#'
#' @param fit The model fit object.
#' @param prior_label The label of the prior used in the model.
#'
#' @return A plot object representing the posterior distribution.
#'
#' @import ggplot2
#' @importFrom bayesplot mcmc_areas
#'
#' @examples
#' fit <- some_model_fit_function()
#' prior_label <- "Normal"
#' generate_plot(fit, prior_label)
#'
#' @export
generate_plot <- function(fit, prior_label) {
  posterior <- as.array(fit)
  plot <- mcmc_areas(posterior) + ggtitle(paste("Prior:", prior_label))
  return(plot)
}

#' Perform sensitivity analysis on all priors
#'
#' This function performs sensitivity analysis on a given dataset using a range of standard deviation values for the intercept and slope priors. It calculates the Watanabe-Akaike Information Criterion (WAIC) for each combination of standard deviation values and returns the results.
#'
#' @param data The input dataset to perform sensitivity analysis on.
#' @param priors A list of prior distributions for the intercept and slope. Each element in the list corresponds to a prior distribution, specified as a list of two student_t distributions: intercept and slope.
#' @param sd_range A vector specifying the range of standard deviation values for the intercept and slope priors.
#'
#' @return A nested list containing the WAIC values for each prior and standard deviation combination. The outer list is indexed by the names of the priors, and the inner lists are indexed by the combination of intercept and slope standard deviation values.
#'
#' @importFrom package_name function_name1 function_name2
#' @importFrom package_name2 function_name3
#'
#' @examples
#' data <- my_data
#' priors <- list(
#'   prior1 = list(student_t(3, 0, 1), student_t(3, 0, 1)),
#'   prior2 = list(student_t(3, 0, 2), student_t(3, 0, 2))
#' )
#' sd_range <- c(1, 2)
#' sensitivity_analysis(data, priors, sd_range)
#'
#' @references
#' Smith, J. (2020). Sensitivity analysis in statistical modeling. Journal of Statistical Analysis, 45(2), 210-225.
#'
#' @export
sensitivity_analysis <- function(data, priors, sd_range) {
  # Create an empty list to store results
  results <- list()
  
  # Loop over each prior in the priors list
  for(prior_name in names(priors)) {
    # Create an empty list to store results for the current prior
    results[[prior_name]] <- list()
    
    # Loop over the range of sd values for intercept
    for(sd_intercept in sd_range) {
      # Loop over the range of sd values for slope
      for(sd_slope in sd_range) {
        # Update the current prior
        current_priors <- priors
        current_priors[[prior_name]] <- list(student_t(3, 0, sd_intercept), student_t(3, 0, sd_slope))
        
        # Fit the model with the updated priors and compute WAIC
        fit <- fit_model_with_prior(current_priors[[prior_name]], data)
        waic <- loo(fit, k_threshold = 0.7)$estimates['elpd_loo', 'Estimate']
        
        # Store the results
        results[[prior_name]][[paste("intercept_sd", sd_intercept, "slope_sd", sd_slope)]] <- waic
      }
    }
  }
  
  # Return the results
  return(results)
}

#' Evaluate Models
#'
#' This function evaluates a set of models using various metrics and performs a posterior predictive check.
#'
#' @param models A list of models to be evaluated.
#' @param data The data used for evaluation.
#'
#' @return A list containing the evaluation results for each model.
#'
#' @import loo
#' @importFrom loo loo, waic
#' @importFrom posterior pp_check
#'
#' @examples
#' # Load data
#' data <- read.csv("data.csv")
#'
#' # Fit models
#' model1 <- lm(y ~ x, data)
#' model2 <- glm(y ~ x, data, family = "binomial")
#' model3 <- lme(y ~ x, data)
#'
#' # Create a list of models
#' models <- list(model1 = model1, model2 = model2, model3 = model3)
#'
#' # Evaluate models
#' results <- evaluate_models(models, data)
#'
#' # Access the results for a specific model
#' model1_results <- results$model1
#'
#' # Access the LOO result for model1
#' model1_loo <- model1_results$loo
#'
#' # Access the posterior predictive check for model2
#' model2_ppc <- results$model2$pp_check
#' 
#' @export
evaluate_models <- function(models, data) {
  # Create empty list to store results
  results <- list()
  
  # Loop over each model
  for(model_name in names(models)) {
    # Compute model metrics
    loo_result <- loo::loo(models[[model_name]], k_threshold = 0.7)
    waic_result <- loo::waic(models[[model_name]], k_threshold = 0.7)
    
    # Compute posterior predictive check
    ppc <- pp_check(models[[model_name]])
    
    # Store results
    results[[model_name]] <- list(
      "loo" = loo_result,
      "waic" = waic_result,
      "pp_check" = ppc
    )
  }
  
  # Return results
  return(results)
}

#' Evaluate Model Performance
#'
#' This function evaluates the performance of a given model by calculating the root mean squared error (RMSE) between the predicted values and the actual values in the test set.
#'
#' @param fit The fitted model object.
#' @param data_test The test dataset containing the predictors (features) and the actual values of the target variable.
#'
#' @return The RMSE (root mean squared error) between the predicted values and the actual values in the test set.
#'
#' @importFrom stats predict
#' @importFrom base mean sqrt
#'
#' @examples
#' data <- data.frame(x = 1:10, y = 1:10)
#' model <- lm(y ~ x, data = data)
#' evaluate_model_performance(model, data)
#'
#' @export
evaluate_model_performance <- function(fit, data_test) {
  # Generate predictions for the test set
  y_pred <- predict(fit, data_test)
  
  # Calculate the RMSE
  rmse <- sqrt(mean((data_test$y - y_pred)^2))
  
  return(rmse)
}

#' # Full functional example
#' 
#' # Generate some example data
#' set.seed(123)
#' n <- 100  # number of observations
#' k <- 5    # number of groups
#' group <- factor(rep(1:k, each = n/k))
#' x <- rnorm(n)
#' a_group <- rnorm(k)  # group-specific intercepts
#' y <- a_group[group] + x + rnorm(n)
#' data <- data.frame(y = y, x = x, group = group)
#'
#' # Split the data into training and test sets
#' set.seed(123)
#' train_ind <- sample(seq_len(nrow(data)), size = 0.75 * nrow(data))
#' data_train <- data[train_ind, ]
#' data_test <- data[-train_ind, ]
#'
#' # Define the priors
#' priors <- list(
#'   "Non-Informative" = set_priors(0, 100, 0, 100),
#'   "Weakly Informative" = set_priors(0, 10, 0, 10),
#'   "Informative" = set_priors(0, 1, 0, 1),
#'   "Empirical Bayes" = empirical_bayes_priors(data_train)
#' )
#'
#' # Fit the models, generate plots, and calculate WAIC for each model
#' fits <- map(priors, fit_model_with_prior, data = data_train)
#'
#' # Generate plots
#' plots <- map2(fits, names(priors), generate_plot)
#'
#' # Generate trace plots
#' trace_plots <- imap(fits, generate_trace_plot)
#'
#' # Combine all the trace plots
#' all_trace_plots <- plot_grid(plotlist = trace_plots, ncol = 2)
#'
#' # Print the combined trace plot
#' print(all_trace_plots)
#'
#' # Define a range of standard deviations for the sensitivity analysis
#' sd_range <- seq(0.5, 10, by = 0.5)
#'
#' # Perform the sensitivity analysis
#' sensitivity_results <- sensitivity_analysis(data_train, priors, sd_range)
#'
#' # Print the results
#' print(sensitivity_results)
#'
#' # Compute WAIC for each model
#' waics <- sapply(fits, function(fit) loo(fit, k_threshold = 0.7)$estimates['elpd_loo', 'Estimate'])
#'
#' # Print the WAICs
#' print(waics)
#'
#' # Combine all the plots
#' all_plots <- plot_grid(plotlist = plots, ncol = 2)
#'
#' # Print the combined plot
#' print(all_plots)
#'
#' # Fit the models using the different priors
#' models <- lapply(priors, fit_model_with_prior, data = data_train)
#'
#' # Evaluate the models
#' evaluation_results <- evaluate_models(models, data_train)
#'
#' # Print the results
#' print(evaluation_results)
#'
#' # Evaluate the performance of each model
#' model_performance <- map_dbl(fits, evaluate_model_performance, data_test = data_test)
#'
#' # Print the RMSE for each model
#' print(model_performance)