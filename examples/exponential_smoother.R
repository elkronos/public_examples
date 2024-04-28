#' Exponential Smoothing for Time Series Data
#'
#' This function (exponential_smoother) applies exponential smoothing to time series data to forecast future points. 
#' It fits a linear model to the initial set of data points to derive starting values for smoothing, computes errors for a range of alpha values, and determines the optimal alpha
#' that minimizes the sum of squared errors. 
#' The function can also plot the original and smoothed series for visual comparison.
#'
#' @param Pt Numeric vector of time series data points.
#' @param k Integer, number of initial data points to fit the linear model for starting values.
#' @param alpha_range Numeric vector of length 2, specifying the range [min, max] of alpha values to test.
#' @param plot Logical, whether to plot the original and smoothed data (default = FALSE).
#'
#' @return A list containing the optimal alpha (`alpha_opt`) and the smoothed time series data (`P_n_opt_l`).
#' @importFrom stats lm coef
#' @importFrom ggplot2 ggplot geom_line aes scale_color_manual theme_minimal theme guides guide_legend print
#' @examples
#' \dontrun{
#' Pt <- c(12, 15, 14, 16, 19, 20, 22, 25, 24, 23)
#' k <- 3
#' alpha_range <- c(0, 1)
#' result <- exponential_smoother(Pt, k, alpha_range, plot = TRUE)
#' print(result)
#' }
# Load library
library(ggplot2)

# Load function
exponential_smoother <- function(Pt, k, alpha_range, plot = FALSE) {
  # Fit a linear model to the first k points to get initial estimates
  model <- lm(Pt[1:k] ~ seq_len(k))
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  
  # Generate alphas
  alpha <- seq(alpha_range[1], alpha_range[2], by = 0.01)
  
  # Compute initial values for smoothing
  S1_0 <- beta0 - ((1 - alpha) / alpha) * beta1
  S2_0 <- beta0 - ((2 * (1 - alpha)) / alpha) * beta1
  
  # Prepare to store errors for each alpha
  errors_list <- numeric(length(alpha))
  
  # Loop through each alpha to find the one that minimizes the error
  for (a in seq_along(alpha)) {
    S1_t <- numeric(length(Pt))
    S2_t <- numeric(length(Pt))
    S1_t[1] <- S1_0[a]
    S2_t[1] <- S2_0[a]
    
    for (i in 2:length(Pt)) {
      S1_t[i] <- alpha[a] * Pt[i] + (1 - alpha[a]) * S1_t[i - 1]
      S2_t[i] <- alpha[a] * S1_t[i] + (1 - alpha[a]) * S2_t[i - 1]
    }
    
    errors <- numeric(length(Pt) - k)
    for (j in (k+1):length(Pt)) {
      errors[j - k] <- (Pt[j] - (2 * S1_t[j] - S2_t[j]))^2
    }
    errors_list[a] <- sum(errors)
  }
  
  # Determine the optimal alpha
  alpha_opt_index <- which.min(errors_list)
  alpha_opt <- alpha[alpha_opt_index]
  
  # Calculate the final smoothed values using the optimal alpha
  S1_t <- numeric(length(Pt))
  S2_t <- numeric(length(Pt))
  S1_t[1] <- S1_0[alpha_opt_index]
  S2_t[1] <- S2_0[alpha_opt_index]
  for (i in 2:length(Pt)) {
    S1_t[i] <- alpha_opt * Pt[i] + (1 - alpha_opt) * S1_t[i - 1]
    S2_t[i] <- alpha_opt * S1_t[i] + (1 - alpha_opt) * S2_t[i - 1]
  }
  Pt_l <- 2 * S1_t - S2_t
  
  # Optionally plot the results
  if (plot) {
    data <- data.frame(Time = 1:length(Pt), Actual = Pt, Smoothed = Pt_l)
    p <- ggplot(data, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
      geom_line(aes(y = Smoothed, color = "Smoothed"), size = 1.2, linetype = "dashed") +
      scale_color_manual(values = c("Actual" = "blue", "Smoothed" = "red")) +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(color = guide_legend(title = "Legend", title.position = "top", title.hjust = 0.5))
    print(p)
  }
  
  return(list(alpha_opt = alpha_opt, P_n_opt_l = Pt_l))
}