# -----------------------
# UAT for exponential_smoother()
# -----------------------

# Install and load ggplot2 if not already available
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", dependencies = TRUE)
}
library(ggplot2)

#' Double Exponential Smoothing and l-Step Ahead Forecasting
#'
#' This function applies Brown’s double exponential (second-order) smoothing to a univariate
#' time series and produces forecasts l-steps ahead. It first fits a linear trend model on the first
#' \code{k} observations to initialize the smoothing series. It then tests a grid of alpha values,
#' computing for each:
#' \itemize{
#'   \item The smoothed series \eqn{S1_t} and second-order smoothed series \eqn{S2_t},
#'   \item The forecast at time \eqn{t} for time \eqn{t+l} via:
#'     \deqn{a_t = 2 S1_t - S2_t, \quad b_t = \frac{\alpha}{1-\alpha}(S1_t - S2_t), \quad \hat{P}_{t+l} = a_t + b_t \times l,}
#'   \item The forecast errors (compared to the actual value at \eqn{t+l}) for time indices \eqn{t=k,\ldots,n-l}.
#' }
#'
#' The optimal alpha is selected as the one minimizing the sum of squared forecast errors.
#'
#' @param Pt A numeric vector of time series data.
#' @param k An integer specifying the number of initial points to fit the linear model (must be at least 2).
#' @param alpha_range A numeric vector of length 2 specifying the range \code{[min, max]} for candidate alpha values.
#'   The lower bound must be positive (to avoid division by zero) and the upper bound is capped at 0.99.
#' @param l A positive integer specifying the forecast horizon (i.e. l-steps ahead forecast).
#' @param plot Logical; if \code{TRUE} a ggplot2-based plot of the actual series and forecasts is displayed.
#' @param alpha_step Optional numeric step size for the alpha grid (default is 0.01).
#'
#' @return A list with the following components:
#' \item{alpha_opt}{The optimal alpha value selected.}
#' \item{forecasts}{A numeric vector of l-steps ahead forecasts (forecast at time \eqn{t} is for \eqn{t+l}).}
#' \item{metrics}{A data frame with the candidate alpha values, their SSE, and RMSE computed over the forecast period.}
#' \item{optimal_S1}{The level series (S1) computed for the optimal alpha.}
#' \item{optimal_S2}{The second-order series (S2) computed for the optimal alpha.}
#' \item{residuals}{The vector of forecast residuals (actual minus forecast) for the optimal alpha.}
#' \item{results_by_alpha}{A list of results for each candidate alpha value.}
exponential_smoother <- function(Pt, k, alpha_range, l, plot = FALSE, alpha_step = 0.01) {
  # -------------------------
  # 1. Input Validation
  # -------------------------
  n <- length(Pt)
  if (!is.numeric(Pt) || n < 3) {
    stop("Pt must be a numeric vector with at least 3 elements.")
  }
  if (!is.numeric(k) || k < 2 || k >= n) {
    stop("k must be an integer between 2 and length(Pt) - 1.")
  }
  if (!is.numeric(l) || l < 1) {
    stop("l (forecast horizon) must be a positive integer.")
  }
  if (n < k + l) {
    stop("Insufficient data: ensure that length(Pt) >= k + l.")
  }
  if (!is.numeric(alpha_range) || length(alpha_range) != 2) {
    stop("alpha_range must be a numeric vector of length 2.")
  }
  if (alpha_range[1] <= 0) {
    warning("alpha_range lower bound adjusted from <=0 to 0.01 to avoid division by zero.")
    alpha_range[1] <- 0.01
  }
  if (alpha_range[2] > 0.99) {
    alpha_range[2] <- 0.99
  }
  
  # -------------------------
  # 2. Initialization via Linear Model
  # -------------------------
  model <- lm(Pt[1:k] ~ seq_len(k))
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  
  # -------------------------
  # 3. Define Candidate Alpha Grid
  # -------------------------
  alpha_grid <- seq(alpha_range[1], alpha_range[2], by = alpha_step)
  
  # -------------------------
  # 4. Loop Over Alpha Values to Compute Forecasts and Errors
  # -------------------------
  results <- vector("list", length(alpha_grid))
  sse_vec <- numeric(length(alpha_grid))
  
  # Evaluate forecast errors over indices where the l-step ahead forecast is available:
  valid_idx <- seq(from = k, to = n - l)
  n_valid <- length(valid_idx)
  
  for (i in seq_along(alpha_grid)) {
    alpha_val <- alpha_grid[i]
    
    # Compute initial S1 and S2 using the provided formulas
    S1_0 <- beta0 - ((1 - alpha_val) / alpha_val) * beta1
    S2_0 <- beta0 - ((2 * (1 - alpha_val)) / alpha_val) * beta1
    
    S1 <- numeric(n)
    S2 <- numeric(n)
    S1[1] <- S1_0
    S2[1] <- S2_0
    
    # forecasts[t] is the forecast for time t+l (based on data up to time t)
    forecasts <- rep(NA, n)
    
    for (t in 2:n) {
      S1[t] <- alpha_val * Pt[t] + (1 - alpha_val) * S1[t - 1]
      S2[t] <- alpha_val * S1[t] + (1 - alpha_val) * S2[t - 1]
      
      a_t <- 2 * S1[t] - S2[t]
      b_t <- (alpha_val / (1 - alpha_val)) * (S1[t] - S2[t])
      forecasts[t] <- a_t + b_t * l
    }
    
    # Compute forecast errors: forecast at time t (for t+l) vs. actual Pt[t+l]
    errors <- Pt[valid_idx + l] - forecasts[valid_idx]
    sse <- sum(errors^2, na.rm = TRUE)
    sse_vec[i] <- sse
    
    results[[i]] <- list(
      alpha      = alpha_val,
      forecasts  = forecasts,
      S1         = S1,
      S2         = S2,
      SSE        = sse,
      residuals  = errors
    )
  }
  
  # -------------------------
  # 5. Select the Optimal Alpha
  # -------------------------
  opt_index <- which.min(sse_vec)
  alpha_opt <- alpha_grid[opt_index]
  optimal_results <- results[[opt_index]]
  
  # Compute additional metrics: RMSE over the valid forecast period
  rmse_vec <- sqrt(sse_vec / n_valid)
  metrics_df <- data.frame(alpha = alpha_grid, SSE = sse_vec, RMSE = rmse_vec)
  
  # -------------------------
  # 6. Optional Plotting
  # -------------------------
  if (plot) {
    # forecasts[t] is for time t+l, so shift forecasts forward by l for plotting.
    forecast_series <- rep(NA, n)
    idx_plot <- (k + l):n
    forecast_series[idx_plot] <- optimal_results$forecasts[idx_plot - l]
    
    plot_data <- data.frame(Time = 1:n,
                            Actual = Pt,
                            Forecast = forecast_series)
    
    p <- ggplot(plot_data, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.2) +
      geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(color = guide_legend(title = "Legend", title.position = "top", title.hjust = 0.5)) +
      labs(title = paste("Exponential Smoothing Forecast (l =", l, "steps ahead)"),
           y = "Value")
    
    print(p)
  }
  
  # -------------------------
  # 7. Return Results
  # -------------------------
  return(list(
    alpha_opt       = alpha_opt,
    forecasts       = optimal_results$forecasts,  # forecast[t] is for time t+l.
    metrics         = metrics_df,
    optimal_S1      = optimal_results$S1,
    optimal_S2      = optimal_results$S2,
    residuals       = optimal_results$residuals,
    results_by_alpha = results
  ))
}

# -----------------------
# Begin UAT (User Acceptance Testing)
# -----------------------

cat("Running User Acceptance Tests for exponential_smoother()\n")
cat("-----------------------------------------------------------\n")

# Test 1: Valid input with plotting enabled
cat("Test 1: Valid input test with forecast plotting\n")
test_Pt <- c(12, 15, 14, 16, 19, 20, 22, 25, 24, 23)
test_k <- 3
test_alpha_range <- c(0.01, 0.99)
test_l <- 2

cat("Executing exponential_smoother() with valid inputs and plot enabled...\n")
result1 <- exponential_smoother(Pt = test_Pt, k = test_k, alpha_range = test_alpha_range, l = test_l, plot = TRUE)
cat("Optimal alpha (Test 1):", result1$alpha_opt, "\n")
cat("Forecast metrics (Test 1):\n")
print(result1$metrics)

# Test 2: Valid input with no plotting
cat("\nTest 2: Valid input test without plotting\n")
result2 <- exponential_smoother(Pt = test_Pt, k = test_k, alpha_range = test_alpha_range, l = test_l, plot = FALSE)
cat("Optimal alpha (Test 2):", result2$alpha_opt, "\n")

# Test 3: Expect error for insufficient data (Pt too short)
cat("\nTest 3: Testing error handling for insufficient data\n")
tryCatch({
  exponential_smoother(Pt = c(10, 20), k = 2, alpha_range = c(0.01, 0.99), l = 2)
}, error = function(e) {
  cat("Caught expected error (Test 3):", e$message, "\n")
})

# Test 4: Expect error when k is not in the valid range (k too large)
cat("\nTest 4: Testing error handling for invalid k (k too large)\n")
tryCatch({
  exponential_smoother(Pt = test_Pt, k = length(test_Pt), alpha_range = test_alpha_range, l = 1)
}, error = function(e) {
  cat("Caught expected error (Test 4):", e$message, "\n")
})

cat("\nAll tests completed.\n")
