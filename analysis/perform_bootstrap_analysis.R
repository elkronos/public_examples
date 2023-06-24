library(pROC)
library(purrr)
library(ggplot2)

#' Perform Bootstrap Analysis
#'
#' This function performs bootstrap analysis on two variables in a dataset based on a grouping variable. It calculates p-values and ROC curves for each variable and performs a comparison between them.
#'
#' @param data A data frame containing the dataset.
#' @param group_var A character string specifying the name of the grouping variable in the dataset.
#' @param var1 A character string specifying the name of the first variable.
#' @param var2 A character string specifying the name of the second variable.
#' @param seed An integer specifying the random seed for reproducibility. Default is 2022.
#' @param n_bootstrap An integer specifying the number of bootstrap samples. Default is 1000.
#' @param alpha A numeric value specifying the significance level for hypothesis testing. Default is 0.05.
#'
#' @return A list containing the results of the bootstrap analysis. The list includes p-values, confidence bounds, proportions of p-values below the significance level, and ROC curves for each variable as well as the comparison results.
#'
#' @import pROC
#' @import purrr
#' @import ggplot2
#'
#' @references
#' For more details on the interpretation of p-values and recent discussions, see the following publication:
#'   Verykouki, E., & Nakas, C. T. (2023). Adaptations on the Use of p-Values for Statistical Inference: An Interpretation of Messages from Recent Public Discussions. Stats, 6(2), 539-551. \url{https://www.mdpi.com/2571-905X/6/2/35}
#'
#' @examples
#' library(pROC)
#' library(purrr)
#' library(ggplot2)
#'
#' set.seed(2023)
#'
#' # Create a synthetic dataset
#' synthetic_data <- data.frame(
#'   group_var = c(rep(0, 100), rep(1, 100)),
#'   var1 = c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 1, sd = 1)),
#'   var2 = c(rnorm(100, mean = 0, sd = 2), rnorm(100, mean = 2, sd = 2))
#' )
#'
#' # Test the function with the synthetic data
#' results <- perform_bootstrap_analysis(data = synthetic_data, group_var = "group_var", var1 = "var1", var2 = "var2")
perform_bootstrap_analysis <- function(data, group_var, var1, var2, seed = 2022, n_bootstrap = 1000, alpha = 0.05) {
  
  bootstrap_analysis <- function(x, y) {
    xbt <- sample(x,replace=T)
    ybt <- sample(y,replace=T)
    p_value <- wilcox.test(xbt,ybt)$p.value
    roc <- roc(controls=xbt,cases=ybt)
    list(p_value = p_value, roc = roc)
  }
  
  compare_roc <- function(roc1, roc2) {
    p_value <- roc.test(roc1, roc2)$p.value
    list(p_value = p_value, roc_test = roc.test(roc1, roc2))
  }
  
  data_basic <- data
  
  x_var1 <- data_basic[, var1][data_basic[, group_var]==0]
  y_var1 <- data_basic[, var1][data_basic[, group_var]==1]
  
  x_var2 <- data_basic[, var2][data_basic[, group_var]==0]
  y_var2 <- data_basic[, var2][data_basic[, group_var]==1]
  
  set.seed(seed)
  
  bootstrap_results_var1 <- map(1:n_bootstrap, ~bootstrap_analysis(x_var1, y_var1))
  bootstrap_results_var2 <- map(1:n_bootstrap, ~bootstrap_analysis(x_var2, y_var2))
  
  roc_var1 <- bootstrap_results_var1[[1]]$roc
  roc_var2 <- bootstrap_results_var2[[1]]$roc
  
  comp_results <- map(1:n_bootstrap, ~compare_roc(roc_var1, roc_var2))
  
  #Reporting
  list(
    var1_result = compute_results(bootstrap_results_var1, alpha),
    var2_result = compute_results(bootstrap_results_var2, alpha),
    comp_result = compute_results(comp_results, alpha)
  )
}

#' Compute Results
#'
#' This function computes various results based on the bootstrap analysis results.
#'
#' @param results A list of bootstrap analysis results.
#' @param alpha A numeric value specifying the significance level for hypothesis testing.
#'
#' @return A list containing the computed results including p-values, confidence bounds, proportion of p-values below the significance level, and the ROC curve.
#'
#' @examples
#' # Assuming you have the results from the perform_bootstrap_analysis function
#' results <- perform_bootstrap_analysis(data = synthetic_data, group_var = "group_var", var1 = "var1", var2 = "var2")
#' compute_results(results$var1_result, alpha = 0.05)
compute_results <- function(results, alpha) {
  p_values <- sapply(results, function(x) x$p_value)
  list(
    p_values = p_values,
    lower_bound = -log2(sort(p_values))[25],
    upper_bound = -log2(sort(p_values))[975],
    proportion = sum(p_values<alpha)/length(p_values),
    roc = results[[1]]$roc
  )
}

#' Plot Results
#'
#' This function plots the results of the bootstrap analysis.
#'
#' @param results A list containing the computed results including p-values and the ROC curve.
#'
#' @examples
#' # Assuming you have the results from the perform_bootstrap_analysis function
#' results <- perform_bootstrap_analysis(data = synthetic_data, group_var = "group_var", var1 = "var1", var2 = "var2")
#' plot_results(results$var1_result)
#' plot_results(results$var2_result)
plot_results <- function(results) {
  hist(results$p_values, main = "Histogram of p-values", xlab = "p-value")
  plot(results$roc, main = "ROC curve")
}