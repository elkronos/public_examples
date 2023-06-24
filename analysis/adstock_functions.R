library(zoo)
library(parallel)
library(memoise)
library(doParallel)
library(ggplot2)

#' Adstock Transformation (Vectorized)
#'
#' This function applies an adstock transformation to a time-series data vector with a specified lag and decay factor. 
#' The adstock transformation is often used in marketing data science to model the delayed effect of advertising 
#' on some performance metric.
#'
#' @param x A numeric vector representing the time-series data to be transformed.
#' @param lag An integer representing the lag period. This is the duration over which the adstock effect should be spread.
#' @param decay A numeric value between 0 and 1 representing the decay rate. This is the rate at which the adstock effect decreases over time.
#' 
#' @return A numeric vector of the same length as `x`, representing the adstock-transformed data.
#' @importFrom zoo rollapply
#' @export
#' @examples
#' \dontrun{
#'   x <- c(0, 1, 2, 3, 4, 5, 6)
#'   lag <- 3
#'   decay <- 0.5
#'   adstock_vectorized(x, lag, decay)
#' }
adstock_vectorized <- function(x, lag, decay) {
  weights <- decay^(seq(0, lag - 1))
  weights <- weights / sum(weights)
  rollapply(x, width = lag, FUN = function(x) sum(x * weights), align = "right", fill = NA)
}

#' Sum of Squared Errors (SSE) Calculation
#'
#' This function calculates the sum of squared errors (SSE) between actual and predicted values.
#' SSE is a common metric used to measure the accuracy of a model in statistics and machine learning.
#'
#' @param actual A numeric vector representing the actual values.
#' @param predicted A numeric vector representing the predicted values. 
#' The length of 'actual' and 'predicted' vectors must be the same.
#' 
#' @return A single numeric value representing the sum of squared errors (SSE) between the actual and predicted values.
#' @export
#' @examples
#' \dontrun{
#'   actual <- c(1, 2, 3, 4, 5)
#'   predicted <- c(1.1, 2.1, 3, 4, 4.9)
#'   sse(actual, predicted)
#' }
sse <- function(actual, predicted) {
  # Calculate the sum of squared errors between the predicted and actual values
  sum((actual - predicted)^2)
}

#' Mean Absolute Error (MAE) Calculation
#'
#' This function calculates the mean absolute error (MAE) between actual and predicted values.
#' MAE is a common metric used to measure the accuracy of a model in statistics and machine learning.
#'
#' @param actual A numeric vector representing the actual values.
#' @param predicted A numeric vector representing the predicted values. 
#' The length of 'actual' and 'predicted' vectors must be the same.
#' 
#' @return A single numeric value representing the mean absolute error (MAE) between the actual and predicted values.
#' @export
#' @examples
#' \dontrun{
#'   actual <- c(1, 2, 3, 4, 5)
#'   predicted <- c(1.1, 2.1, 3, 4, 4.9)
#'   mae(actual, predicted)
#' }
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

#' Train a Linear Model and Predict
#'
#' This function fits a linear regression model on the training data and then predicts 
#' the values for the test data. It assumes that the response variable is 'adstocked_train'
#' and the predictor variable is the time sequence.
#'
#' @param x_train A numeric vector representing the training data.
#' @param x_test A numeric vector representing the test data. 
#' @param adstocked_train A numeric vector representing the adstock-transformed training data.
#' 
#' @return A numeric vector representing the predicted values for 'x_test' based on the trained linear model.
#' @export
#' @importFrom stats lm predict
#' @examples
#' \dontrun{
#'   x_train <- 1:50
#'   x_test <- 51:100
#'   adstocked_train <- rnorm(50)
#'   train_and_predict(x_train, x_test, adstocked_train)
#' }
train_and_predict <- function(x_train, x_test, adstocked_train) {
  time_train <- seq_along(x_train)
  model <- lm(adstocked_train ~ time_train)
  
  time_test <- seq_along(x_test) + length(x_train)
  predictions <- predict(model, newdata = data.frame(time_train = time_test))
  return(predictions)
}

#' K-fold Cross-Validation for Adstock Transformation
#'
#' This function performs k-fold cross-validation on a time-series data vector using an adstock transformation.
#' It fits a model on the training set, makes predictions on the test set, and computes the sum of squared errors (SSE) for each fold.
#' Finally, it returns the mean SSE across all folds.
#'
#' @param x A numeric vector representing the time-series data to be cross-validated.
#' @param lag An integer representing the lag period for the adstock transformation.
#' @param decay A numeric value between 0 and 1 representing the decay rate for the adstock transformation.
#' @param k An integer specifying the number of folds for the cross-validation. Default is 10.
#' 
#' @return A single numeric value representing the mean sum of squared errors (SSE) across all k folds.
#' @importFrom stats setdiff embed colSums
#' @export
#' @examples
#' \dontrun{
#'   x <- c(0, 1, 2, 3, 4, 5, 6)
#'   lag <- 3
#'   decay <- 0.5
#'   k_fold_cv(x, lag, decay)
#' }
k_fold_cv <- function(x, lag, decay, k = 10) {
  fold_size <- length(x) %/% k
  errors <- numeric(k)
  
  # Pre-calculate weights
  weights <- decay^(seq(0, lag - 1))
  weights <- weights / sum(weights)
  
  for (i in seq_len(k)) {
    test_indices <- ((i - 1) * fold_size + 1):min(i * fold_size, length(x))
    train_indices <- setdiff(seq_len(length(x)), test_indices)
    
    x_test <- x[test_indices]
    
    # Calculate adstock transformation using base R functions
    adstocked_full <- colSums(embed(c(rep(0, lag - 1), x), lag) * weights)
    adstocked_test <- adstocked_full[test_indices]
    
    predictions <- train_and_predict(x_train, x_test, adstocked_train)
    errors[i] <- sse(x_test, predictions)
    
  }
  
  mean(errors)
}

#' Grid Search with K-fold Cross-Validation for Adstock Transformation
#'
#' This function performs a grid search with k-fold cross-validation to find the optimal combination of lag and decay parameters
#' for an adstock transformation applied to a time-series data vector. It evaluates the performance of different parameter combinations
#' by calculating the mean sum of squared errors (SSE) across all folds.
#'
#' @param x A numeric vector representing the time-series data to be transformed and cross-validated.
#' @param lags A vector of integers specifying the range of lag periods to consider in the grid search.
#' @param decays A vector of numeric values between 0 and 1 specifying the range of decay rates to consider in the grid search.
#' @param cores An integer specifying the number of CPU cores to use for parallel processing. Default is 1.
#' @param k An integer specifying the number of folds for the cross-validation. Default is 10.
#' 
#' @return A list with the following components:
#' \describe{
#'   \item{optimal_lag}{The lag period that resulted in the lowest mean SSE across all folds.}
#'   \item{optimal_decay}{The decay rate that resulted in the lowest mean SSE across all folds.}
#'   \item{cv_errors}{A matrix of mean SSE values for different lag and decay combinations.}
#' }
#' @importFrom parallel makeCluster detectCores
#' @importFrom foreach foreach %:% %dopar% %:% %dopar% arrayInd stopCluster
#' @importFrom zoo rollapply
#' @export
#' @examples
#' \dontrun{
#'   x <- c(0, 1, 2, 3, 4, 5, 6)
#'   lags <- seq(1, 5)
#'   decays <- seq(0.1, 0.5, by = 0.1)
#'   adstock_grid_search_cv(x, lags, decays)
#' }
adstock_grid_search_cv <- function(x, lags = seq(1, 20, by = 1), decays = seq(0.1, 0.9, by = 0.05), cores = 1, k = 10) {
  cores <- min(cores, detectCores())
  
  cl <- makeCluster(cores)
  clusterEvalQ(cl, {
    library(zoo)
    adstock_vectorized <- function(x, lag, decay) {
      weights <- decay^(seq(0, lag - 1))
      weights <- weights / sum(weights)
      rollapply(x, width = lag, FUN = function(x) sum(x * weights), align = "right", fill = NA)
    }
    sse <- function(actual, predicted) {
      sum((actual - predicted)^2)
    }
    k_fold_cv <- function(x, lag, decay, k = 10) {
      fold_size <- length(x) %/% k
      errors <- numeric(k)
      
      for (i in seq_len(k)) {
        test_indices <- ((i - 1) * fold_size + 1):min(i * fold_size, length(x))
        train_indices <- setdiff(seq_len(length(x)), test_indices)
        
        x_train <- x[train_indices]
        x_test <- x[test_indices]
        
        adstocked_train <- adstock_vectorized(x_train, lag, decay)
        adstocked_full <- adstock_vectorized(x, lag, decay)
        adstocked_test <- adstocked_full[test_indices]
        
        errors[i] <- sse(x_test, adstocked_test)
      }
      
      mean(errors)
    }
  })
  registerDoParallel(cl)
  
  cv_errors <- foreach(lag = lags, .combine = "cbind") %:%
    foreach(decay = decays, .combine = "c") %dopar% {
      k_fold_cv(x, lag, decay, k)
    }
  stopCluster(cl)
  
  cv_errors <- matrix(cv_errors, nrow = length(lags), ncol = length(decays))
  min_error_index <- arrayInd(which.min(cv_errors), dim(cv_errors))  # Use arrayInd() to get the correct indices
  
  list(optimal_lag = lags[min_error_index[1]], optimal_decay = decays[min_error_index[2]], cv_errors = cv_errors)
}

#' Optimized Walk Forward Cross-Validation for Adstock Transformation
#'
#' This function performs optimized walk forward cross-validation on a time-series data vector using an adstock transformation.
#' It calculates the mean absolute error (MAE), mean squared error (MSE), or sum of squared errors (SSE) between the actual
#' and adstock-transformed values in a rolling window fashion. The window size is determined by the minimum training size and step size.
#'
#' @param x A numeric vector representing the time-series data to be cross-validated.
#' @param lag An integer representing the lag period for the adstock transformation.
#' @param decay A numeric value between 0 and 1 representing the decay rate for the adstock transformation.
#' @param min_train_size A numeric value representing the minimum training size as a fraction of the total length of 'x'. Default is 0.5.
#' @param step An integer representing the step size or the number of time points to move forward in each iteration. Default is 1.
#' 
#' @return A single numeric value representing the mean error (MAE, MSE, or SSE) across all iterations of the walk forward cross-validation.
#' @export
#' @importFrom stats round
#' @examples
#' \dontrun{
#'   x <- c(0, 1, 2, 3, 4, 5, 6)
#'   lag <- 3
#'   decay <- 0.5
#'   min_train_size <- 0.5 * length(x)
#'   step <- 1
#'   walk_forward_cv_optimized(x, lag, decay, min_train_size, step)
#' }
walk_forward_cv_optimized <- function(x, lag, decay, min_train_size = 0.5 * length(x), step = 1) {
  start_index <- round(min_train_size)
  errors <- numeric()
  
  # Pre-calculate weights
  weights <- decay^(seq(0, lag - 1))
  weights <- weights / sum(weights)
  
  while (start_index + step - 1 <= length(x)) {
    test_indices <- (start_index + 1):(start_index + step)
    x_test <- x[test_indices]
    
    # Calculate adstock transformation using base R functions
    adstocked_full <- colSums(embed(c(rep(0, lag - 1), x), lag) * weights)
    adstocked_test <- adstocked_full[test_indices]
    
    errors <- c(errors, mae(x_test, adstocked_test))  # You can use mae, mse or sse, depending on the metric you want to use
    start_index <- start_index + step
  }
  
  mean(errors)
}

#' Grid Search with Walk Forward Cross-Validation for Adstock Transformation
#'
#' This function performs a grid search with walk forward cross-validation to find the optimal combination of lag and decay parameters
#' for an adstock transformation applied to a time-series data vector. It evaluates the performance of different parameter combinations
#' by calculating the mean absolute error (MAE) in a rolling window fashion. The window size is determined by the minimum training size and step size.
#'
#' @param x A numeric vector representing the time-series data to be transformed and cross-validated.
#' @param lags A vector of integers specifying the range of lag periods to consider in the grid search.
#' @param decays A vector of numeric values between 0 and 1 specifying the range of decay rates to consider in the grid search.
#' @param cores An integer specifying the number of CPU cores to use for parallel processing. Default is 1.
#' @param min_train_size A numeric value representing the minimum training size as a fraction of the total length of 'x'. Default is 0.5.
#' @param step An integer representing the step size or the number of time points to move forward in each iteration. Default is 1.
#' 
#' @return A list with the following components:
#' \describe{
#'   \item{optimal_lag}{The lag period that resulted in the lowest mean absolute error (MAE) in the walk forward cross-validation.}
#'   \item{optimal_decay}{The decay rate that resulted in the lowest mean absolute error (MAE) in the walk forward cross-validation.}
#'   \item{cv_errors}{A matrix of mean absolute error (MAE) values for different lag and decay combinations.}
#' }
#' @importFrom parallel makeCluster detectCores
#' @importFrom foreach foreach %:% %dopar% %:% %dopar% arrayInd stopCluster
#' @importFrom zoo rollapply
#' @export
#' @examples
#' \dontrun{
#'   x <- c(0, 1, 2, 3, 4, 5, 6)
#'   lags <- seq(1, 5)
#'   decays <- seq(0.1, 0.5, by = 0.1)
#'   adstock_grid_search_cv_walk_forward(x, lags, decays)
#' }
adstock_grid_search_cv_walk_forward <- function(x, lags = seq(1, 20, by = 1), decays = seq(0.1, 0.9, by = 0.05), cores = 1, min_train_size = 0.5 * length(x), step = 1) {
  cores <- min(cores, detectCores())
  
  cl <- makeCluster(cores)
  clusterEvalQ(cl, {
    library(zoo)
    adstock_vectorized <- function(x, lag, decay) {
      weights <- decay^(seq(0, lag - 1))
      weights <- weights / sum(weights)
      rollapply(x, width = lag, FUN = function(x) sum(x * weights), align = "right", fill = NA)
    }
    mae <- function(actual, predicted) {
      mean(abs(actual - predicted))
    }
    walk_forward_cv <- function(x, lag, decay, min_train_size = 0.5 * length(x), step = 1) {
      start_index <- round(min_train_size)
      errors <- numeric()
      
      while (start_index + step - 1 <= length(x)) {
        train_indices <- 1:start_index
        test_indices <- (start_index + 1):(start_index + step)
        
        x_train <- x[train_indices]
        x_test <- x[test_indices]
        
        adstocked_train <- adstock_vectorized(x_train, lag, decay)
        adstocked_full <- adstock_vectorized(x, lag, decay)
        adstocked_test <- adstocked_full[test_indices]
        
        errors <- c(errors, mae(x_test, adstocked_test))
        start_index <- start_index + step
      }
      
      mean(errors)
    }
  })
  registerDoParallel(cl)
  
  cv_errors <- foreach(lag = lags, .combine = "cbind") %:%
    foreach(decay = decays, .combine = "c") %dopar% {
      walk_forward_cv(x, lag, decay, min_train_size, step)
    }
  stopCluster(cl)
  
  cv_errors <- matrix(cv_errors, nrow = length(lags), ncol = length(decays))
  min_error_index <- arrayInd(which.min(cv_errors), dim(cv_errors))  # Use arrayInd() to get the correct indices
  
  list(optimal_lag = lags[min_error_index[1]], optimal_decay = decays[min_error_index[2]], cv_errors = cv_errors)
}

#' Demonstration of functions in script with visual
#' 
#' @examples
#' \dontrun{
#'   # Generate new input data
#'   set.seed(123)
#'   x <- 10 * sin(seq(0, 4*pi, length.out = 100)) + rnorm(100, mean = 0, sd = 3)
#'   
#'   # Run the grid search and transformation with cross-validation
#'   results_cv <- adstock_grid_search_cv(x, cores = 2)
#'   
#'   cat("Optimal lag with CV:", results_cv$optimal_lag, "\n")
#'   cat("Optimal decay with CV:", results_cv$optimal_decay, "\n")
#'   
#'   # Apply adstock transformation using the optimal parameters
#'   adstocked_x_cv <- adstock_vectorized(x, results_cv$optimal_lag, results_cv$optimal_decay)
#'   
#'   # Replace the NA values in the transformed series with the original ones for the sake of the plot
#'   adstocked_x_cv[is.na(adstocked_x_cv)] <- x[is.na(adstocked_x_cv)]
#'   
#'   # Offset the transformed series slightly for visualization
#'   offset <- 1  # change this value to get a better visualization if needed
#'   adstocked_x_cv_offset <- adstocked_x_cv + offset
#'   
#'   # Create a data frame with your series
#'   df <- data.frame(Time = 1:length(x), Original = x, Transformed = adstocked_x_cv_offset)
#'   
#'   # Plot using ggplot2
#'   ggplot(df, aes(Time)) +
#'     geom_line(aes(y = Original), colour = "#56B4E9", linewidth = 1.2) +  # Light blue color
#'     geom_line(aes(y = Transformed), colour = "#D55E00", linewidth = 1.2) +  # Red color
#'     labs(x = "Time", 
#'          y = "Value", 
#'          title = "Original vs. Adstock Transformed Time Series with Cross-Validation", 
#'          colour = "Legend") +
#'     theme_minimal() +
#'     theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center the title, make it bold, and increase the size
#'           axis.title = element_text(face = "bold", size = 12),  # Make axis titles bold and larger
#'           legend.position = "top",  # Position the legend at the top
#'           legend.title = element_text(face = "bold", size = 10),  # Make legend title bold and slightly larger
#'           legend.text = element_text(size = 8)) +  # Make legend text a bit smaller
#'     scale_color_manual(values = c("#56B4E9", "#D55E00"),  # Specify the colors for the lines
#'                        labels = c("Original", "Adstock Transformed"))  # Specify the labels for the legend
#'   
#'   # Split your data into a training set and a test set
#'   train_proportion <- 0.7
#'   train_size <- round(train_proportion * length(x))
#'   x_train <- x[1:train_size]
#'   x_test <- x[(train_size + 1):length(x)]
#'   
#'   # Transform the training data with the optimal parameters
#'   optimal_lag <- results_cv$optimal_lag
#'   optimal_decay <- results_cv$optimal_decay
#'   adstocked_train <- adstock_vectorized(x_train, optimal_lag, optimal_decay)
#'   
#'   # Transform the training data with the optimal parameters
#'   adstocked_train <- adstock_vectorized(x_train, optimal_lag, optimal_decay)
#'   
#'   # Train the model and make predictions on the test set
#'   predictions <- train_and_predict(x_train, x_test, adstocked_train)
#'   
#'   # Print the predictions
#'   print(predictions)
#' }