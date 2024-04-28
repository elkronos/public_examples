#' Exponential Smoothing and Forecasting for Time Series Data
#'
#' This function applies second-order exponential smoothing to time series data and computes forecasts l-steps ahead from each point in the series. 
#' It fits a linear model to the initial set of data points to derive starting values for smoothing. The function tests a range of alpha values,
#' computes smoothing and forecasts for each, and selects the optimal alpha that minimizes the sum of squared errors. Optionally, it can plot the 
#' original data points alongside the forecasts for visual comparison.
#'
#' @param Pt Numeric vector of time series data points.
#' @param k Integer, number of initial data points to fit the linear model for starting values.
#' @param alpha_range Numeric vector of length 2, specifying the range [min, max] of alpha values to test.
#' @param l Integer, number of future points to forecast from each data point.
#' @param plot Logical, whether to plot the original data and forecasts (default = FALSE).
#'
#' @return A list containing the optimal alpha value (`alpha_opt`) and the forecasts (`Forecasts`) l-steps ahead from each time point using the optimal alpha.
#' @importFrom stats lm coef
#' @importFrom ggplot2 ggplot geom_line aes scale_color_manual theme_minimal theme guides guide_legend print
#' @examples
#' \dontrun{
#' Pt <- c(12, 15, 14, 16, 19, 20, 22, 25, 24, 23)
#' k <- 3
#' alpha_range <- c(0, 0.99) # Avoiding alpha = 1 to prevent division by zero
#' l <- 2  # Forecasting 2 steps ahead
#' result <- exponential_smoother(Pt, k, alpha_range, l, plot = TRUE)
#' print(result)
#' }
# Load library
library(ggplot2)

# Load function
exponential_smoother <- function(Pt, k, alpha_range, l, plot = FALSE) {
  # Fit a linear model to the first k points to get initial estimates
  model <- lm(Pt[1:k] ~ seq_len(k))
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  
  # Generate alphas
  alpha <- seq(alpha_range[1], min(alpha_range[2], 0.99), by = 0.01)
  
  # Compute initial values for smoothing
  S1_0 <- beta0 - ((1 - alpha) / alpha) * beta1
  S2_0 <- beta0 - ((2 * (1 - alpha)) / alpha) * beta1
  
  # Prepare to store errors for each alpha
  errors_list <- numeric(length(alpha))
  forecasts_list <- list()
  
  # Loop through each alpha to find the one that minimizes the error
  for (a in seq_along(alpha)) {
    S1_t <- numeric(length(Pt))
    S2_t <- numeric(length(Pt))
    S1_t[1] <- S1_0[a]
    S2_t[1] <- S2_0[a]
    
    # Initialize forecast list for current alpha
    forecasts <- numeric(length(Pt))
    
    for (i in 2:length(Pt)) {
      S1_t[i] <- alpha[a] * Pt[i] + (1 - alpha[a]) * S1_t[i - 1]
      S2_t[i] <- alpha[a] * S1_t[i] + (1 - alpha[a]) * S2_t[i - 1]
      
      # Calculate forecast for l-steps ahead
      a_val <- 2 * S1_t[i] - S2_t[i]
      b_val <- (alpha[a] / (1 - alpha[a])) * (S1_t[i] - S2_t[i])
      forecasts[i] <- a_val + b_val * l
    }
    
    errors <- numeric(length(Pt) - k)
    for (j in (k+1):length(Pt)) {
      errors[j - k] <- (Pt[j] - (2 * S1_t[j] - S2_t[j]))^2
    }
    errors_list[a] <- sum(errors)
    forecasts_list[[a]] <- forecasts
  }
  
  # Determine the optimal alpha
  alpha_opt_index <- which.min(errors_list)
  alpha_opt <- alpha[alpha_opt_index]
  optimal_forecasts <- forecasts_list[[alpha_opt_index]]
  
  # Optionally plot the results
  if (plot) {
    data <- data.frame(Time = 1:length(Pt), Actual = Pt, Forecast = optimal_forecasts)
    p <- ggplot(data, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.2) +
      geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(color = guide_legend(title = "Legend", title.position = "top", title.hjust = 0.5))
    print(p)
  }
  
  return(list(alpha_opt = alpha_opt, Forecasts = optimal_forecasts))
}
