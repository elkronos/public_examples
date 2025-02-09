#############################################
# Adstock Transformation Module Code
#############################################

# Load required libraries for functionality
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(ggplot2))

# 1. Transformation and Helper Functions -----------------------------------

#' Compute Adstock Weights
#'
#' @param lag Positive integer representing the lag period.
#' @param decay Numeric value between 0 and 1 representing the decay rate.
#' @return A normalized numeric vector of weights.
compute_adstock_weights <- function(lag, decay) {
  if (!is.numeric(lag) || lag < 1 || (lag %% 1 != 0))
    stop("`lag` must be a positive integer.")
  if (!is.numeric(decay) || decay < 0 || decay > 1)
    stop("`decay` must be a numeric value between 0 and 1.")
  
  weights <- decay^(0:(lag - 1))
  weights / sum(weights)
}

#' Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lag Positive integer for the duration over which the adstock effect is spread.
#' @param decay Numeric decay rate between 0 and 1.
#' @param fill_na Logical; if TRUE (default) uses zoo::rollapply (resulting in NA for the first lag-1 positions),
#'   otherwise uses an embed-based approach returning a full-length vector.
#'
#' @return A numeric vector of adstock-transformed values.
adstock_transform <- function(x, lag, decay, fill_na = TRUE) {
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  
  weights <- compute_adstock_weights(lag, decay)
  
  if (fill_na) {
    # Use rollapply: first lag - 1 observations will be NA.
    rollapply(x, width = lag, FUN = function(v) sum(v * weights),
              align = "right", fill = NA)
  } else {
    if (length(x) < lag)
      stop("Length of `x` must be at least equal to `lag` when fill_na = FALSE.")
    # Prepend zeros and use rowSums so that the result is a full-length vector.
    rowSums(embed(c(rep(0, lag - 1), x), lag) * weights)
  }
}

# 2. Error Metric Functions --------------------------------------------------

#' Calculate Sum of Squared Errors (SSE)
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values.
#' @return The SSE between actual and predicted.
calculate_sse <- function(actual, predicted) {
  if (length(actual) != length(predicted))
    stop("Lengths of `actual` and `predicted` must match.")
  sum((actual - predicted)^2)
}

#' Calculate Mean Absolute Error (MAE)
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values.
#' @return The MAE between actual and predicted.
calculate_mae <- function(actual, predicted) {
  if (length(actual) != length(predicted))
    stop("Lengths of `actual` and `predicted` must match.")
  mean(abs(actual - predicted))
}

# 3. Modeling Functions -------------------------------------------------------

#' Train a Linear Model and Predict
#'
#' @param x_train Numeric vector for training (original data).
#' @param x_test Numeric vector for testing (original data).
#' @param adstocked_train Numeric vector of adstock-transformed training data.
#'
#' @return A numeric vector of predictions for x_test.
train_and_predict <- function(x_train, x_test, adstocked_train) {
  if (length(x_train) != length(adstocked_train))
    stop("Length of `x_train` and `adstocked_train` must be equal.")
  
  time_train <- seq_along(x_train)
  model <- lm(adstocked_train ~ time_train)
  
  # Create time indices for the test set (offset by the training length)
  time_test <- seq_along(x_test) + length(x_train)
  predict(model, newdata = data.frame(time_train = time_test))
}

# 4. Cross-Validation Functions ----------------------------------------------

#' K-fold Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lag Positive integer for the adstock transformation lag.
#' @param decay Numeric decay rate between 0 and 1.
#' @param k Integer number of folds (default is 10).
#'
#' @return Mean SSE across folds.
k_fold_cv <- function(x, lag, decay, k = 10) {
  n <- length(x)
  fold_size <- n %/% k
  errors <- numeric(k)
  
  for (i in seq_len(k)) {
    test_indices <- ((i - 1) * fold_size + 1):min(i * fold_size, n)
    train_indices <- setdiff(seq_len(n), test_indices)
    
    x_train <- x[train_indices]
    x_test <- x[test_indices]
    
    # Use adstock_transform with fill_na = FALSE to get full-length transformed data
    adstocked_train <- adstock_transform(x_train, lag, decay, fill_na = FALSE)
    
    predictions <- train_and_predict(x_train, x_test, adstocked_train)
    errors[i] <- calculate_sse(x_test, predictions)
  }
  mean(errors)
}

#' Walk Forward Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lag Positive integer for the adstock transformation lag.
#' @param decay Numeric decay rate between 0 and 1.
#' @param min_train_size Minimum training size (default is 50% of x length).
#' @param step Integer step size for the moving window (default is 1).
#'
#' @return Mean MAE over all iterations.
walk_forward_cv <- function(x, lag, decay, min_train_size = 0.5 * length(x), step = 1) {
  n <- length(x)
  start_index <- round(min_train_size)
  if (start_index >= n)
    stop("`min_train_size` must be less than the length of `x`.")
  
  errors <- numeric()
  # Get full-length transformation using fill_na = FALSE
  adstocked_full <- adstock_transform(x, lag, decay, fill_na = FALSE)
  
  while ((start_index + step) <= n) {
    test_indices <- (start_index + 1):(start_index + step)
    x_test <- x[test_indices]
    adstocked_test <- adstocked_full[test_indices]
    
    errors <- c(errors, calculate_mae(x_test, adstocked_test))
    start_index <- start_index + step
  }
  mean(errors)
}

# 5. Grid Search Functions ----------------------------------------------------

#' Grid Search with K-fold Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lags Vector of candidate lag values.
#' @param decays Vector of candidate decay values.
#' @param cores Number of CPU cores to use (default is 1).
#' @param k Integer number of folds (default is 10).
#'
#' @return A list with optimal lag, optimal decay, and the CV error matrix.
grid_search_cv <- function(x, 
                           lags = seq(1, 20, by = 1), 
                           decays = seq(0.1, 0.9, by = 0.05), 
                           cores = 1, 
                           k = 10) {
  available_cores <- parallel::detectCores()
  cores <- min(cores, available_cores)
  
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  cv_errors <- foreach::foreach(lag = lags, .combine = "cbind",
                                .export = c("k_fold_cv", "compute_adstock_weights", 
                                            "train_and_predict", "calculate_sse", "adstock_transform"),
                                .packages = "zoo") %:%
    foreach::foreach(decay = decays, .combine = "c") %dopar% {
      k_fold_cv(x, lag, decay, k)
    }
  
  parallel::stopCluster(cl)
  
  cv_errors <- matrix(cv_errors, nrow = length(lags), ncol = length(decays))
  min_error_index <- which(cv_errors == min(cv_errors), arr.ind = TRUE)
  
  list(optimal_lag = lags[min_error_index[1]],
       optimal_decay = decays[min_error_index[2]],
       cv_errors = cv_errors)
}

#' Grid Search with Walk Forward Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lags Vector of candidate lag values.
#' @param decays Vector of candidate decay values.
#' @param cores Number of CPU cores to use (default is 1).
#' @param min_train_size Minimum training size (default is 50% of x length).
#' @param step Step size for the rolling window (default is 1).
#'
#' @return A list with optimal lag, optimal decay, and the CV error matrix.
grid_search_walk_forward_cv <- function(x, 
                                        lags = seq(1, 20, by = 1), 
                                        decays = seq(0.1, 0.9, by = 0.05), 
                                        cores = 1, 
                                        min_train_size = 0.5 * length(x), 
                                        step = 1) {
  available_cores <- parallel::detectCores()
  cores <- min(cores, available_cores)
  
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  cv_errors <- foreach::foreach(lag = lags, .combine = "cbind",
                                .export = c("walk_forward_cv", "adstock_transform", "compute_adstock_weights", "calculate_mae"),
                                .packages = "zoo") %:%
    foreach::foreach(decay = decays, .combine = "c") %dopar% {
      walk_forward_cv(x, lag, decay, min_train_size, step)
    }
  
  parallel::stopCluster(cl)
  
  cv_errors <- matrix(cv_errors, nrow = length(lags), ncol = length(decays))
  min_error_index <- which(cv_errors == min(cv_errors), arr.ind = TRUE)
  
  list(optimal_lag = lags[min_error_index[1]],
       optimal_decay = decays[min_error_index[2]],
       cv_errors = cv_errors)
}

#############################################
# End of Adstock Transformation Module Code
#############################################

#############################################
# UAT Test Code using testthat
#############################################

# Load testthat (if not already loaded)
suppressPackageStartupMessages(library(testthat))

context("Adstock Transformation Functions")

test_that("compute_adstock_weights returns correct normalized weights", {
  lag <- 3
  decay <- 0.5
  weights <- compute_adstock_weights(lag, decay)
  expected <- 0.5^(0:(lag - 1))
  expected <- expected / sum(expected)
  expect_equal(weights, expected)
})

test_that("adstock_transform works correctly with fill_na = TRUE", {
  x <- 1:10
  lag <- 3
  decay <- 0.5
  transformed <- adstock_transform(x, lag, decay, fill_na = TRUE)
  
  # Expect the first lag - 1 elements to be NA
  expect_true(all(is.na(transformed[1:(lag - 1)])))
  expect_equal(length(transformed), length(x))
})

test_that("adstock_transform works correctly with fill_na = FALSE", {
  x <- 1:10
  lag <- 3
  decay <- 0.5
  transformed <- adstock_transform(x, lag, decay, fill_na = FALSE)
  
  # With fill_na = FALSE, there should be no NAs and length should equal length(x)
  expect_false(any(is.na(transformed)))
  expect_equal(length(transformed), length(x))
})

context("Error Metric Functions")

test_that("calculate_sse computes the correct value", {
  actual <- c(1, 2, 3)
  predicted <- c(1, 3, 2)
  sse_val <- calculate_sse(actual, predicted)
  expected <- sum((actual - predicted)^2)
  expect_equal(sse_val, expected)
})

test_that("calculate_mae computes the correct value", {
  actual <- c(1, 2, 3)
  predicted <- c(1, 3, 2)
  mae_val <- calculate_mae(actual, predicted)
  expected <- mean(abs(actual - predicted))
  expect_equal(mae_val, expected)
})

context("Model Training and Prediction")

test_that("train_and_predict returns predictions of correct length", {
  x_train <- 1:20
  x_test <- 21:30
  lag <- 3
  decay <- 0.5
  # Use fill_na = FALSE so that the transformation returns a full-length vector
  adstocked_train <- adstock_transform(x_train, lag, decay, fill_na = FALSE)
  predictions <- train_and_predict(x_train, x_test, adstocked_train)
  
  expect_equal(length(predictions), length(x_test))
})

context("Cross-Validation Functions")

test_that("k_fold_cv returns a numeric error value", {
  x <- 1:50
  lag <- 3
  decay <- 0.5
  error <- k_fold_cv(x, lag, decay, k = 5)
  expect_true(is.numeric(error))
  expect_length(error, 1)
})

test_that("walk_forward_cv returns a numeric error value", {
  x <- 1:50
  lag <- 3
  decay <- 0.5
  error <- walk_forward_cv(x, lag, decay, min_train_size = 20, step = 5)
  expect_true(is.numeric(error))
  expect_length(error, 1)
})

context("Grid Search Functions")

test_that("grid_search_cv returns correct list structure", {
  x <- 1:50
  # Use a small grid for testing
  lags <- 2:4
  decays <- seq(0.1, 0.5, by = 0.2)
  result <- grid_search_cv(x, lags = lags, decays = decays, cores = 1, k = 3)
  
  expect_true(is.list(result))
  expect_true("optimal_lag" %in% names(result))
  expect_true("optimal_decay" %in% names(result))
  expect_true("cv_errors" %in% names(result))
  expect_equal(dim(result$cv_errors), c(length(lags), length(decays)))
})

test_that("grid_search_walk_forward_cv returns correct list structure", {
  x <- 1:50
  lags <- 2:4
  decays <- seq(0.1, 0.5, by = 0.2)
  result <- grid_search_walk_forward_cv(x, lags = lags, decays = decays,
                                        cores = 1, min_train_size = 20, step = 5)
  
  expect_true(is.list(result))
  expect_true("optimal_lag" %in% names(result))
  expect_true("optimal_decay" %in% names(result))
  expect_true("cv_errors" %in% names(result))
  expect_equal(dim(result$cv_errors), c(length(lags), length(decays)))
})

context("Demonstration: Plotting and Predictions")

test_that("The plotting example produces a valid ggplot object", {
  # Create a simple synthetic time-series
  set.seed(123)
  x <- 10 * sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, 0, 3)
  
  # Run grid search to determine optimal parameters
  results_cv <- grid_search_cv(x, cores = 1)
  
  # Use the optimal parameters to transform the data
  adstocked_x <- adstock_transform(x, results_cv$optimal_lag, results_cv$optimal_decay)
  adstocked_x[is.na(adstocked_x)] <- x[is.na(adstocked_x)]  # Replace NAs for plotting
  
  # Offset the transformed series for visualization purposes
  offset <- 1
  adstocked_x_offset <- adstocked_x + offset
  
  # Create a data frame for plotting
  df <- data.frame(Time = 1:length(x),
                   Original = x,
                   Transformed = adstocked_x_offset)
  
  # Generate the ggplot object using 'linewidth' to avoid deprecation warnings
  p <- ggplot(df, aes(Time)) +
    geom_line(aes(y = Original), colour = "#56B4E9", linewidth = 1.2) +
    geom_line(aes(y = Transformed), colour = "#D55E00", linewidth = 1.2) +
    labs(x = "Time",
         y = "Value",
         title = "Original vs. Adstock Transformed Time Series") +
    theme_minimal()
  
  expect_true(inherits(p, "ggplot"))
})
