# Load required libraries
library(BayesFactor)
library(data.table)
library(ggplot2)

#' Perform Bayes Factor Analysis
#'
#' This function performs a Bayesian t-test to compare the means of two groups using the BayesFactor package. 
#' The input data, group variable, and response variable should be provided. 
#' The function checks for errors, performs the Bayesian t-test and prints the Bayes factor value.
#'
#' @param data A data frame or data table that contains the group_var and response_var.
#' @param group_var The name of the column in data that contains the group variable. It should be a factor or character variable with at least two levels.
#' @param response_var The name of the column in data that contains the response variable. It should be a numeric variable.
#'
#' @return This function prints the Bayes factor value for the performed Bayesian t-test.
#' @references Morey, R. D., & Rouder, J. N. (2018). BayesFactor: Computation of Bayes Factors for Common Designs. R package version 0.9.12-4.2.
#'
#' @examples
#' data(mtcars)
#' mtcars$group_var <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 2)
#' bayes_factor(mtcars, "group_var", "mpg")
#'
#' @importFrom data.table data.table
#' @importFrom BayesFactor ttestBF
#' @importFrom stats is.numeric
#' @importFrom utils stop
#' @export
bayes_factor <- function(data, group_var, response_var) {
  
  # Error check: validate input parameters
  if (!inherits(data, "data.frame") && !inherits(data, "data.table")) {
    stop("The 'data' parameter should be a data frame or a data.table.")
  }
  
  if (!all(c(group_var, response_var) %in% names(data))) {
    stop("Please ensure both 'group_var' and 'response_var' are in 'data'.")
  }
  
  # Check for missing values
  if (any(is.na(data[[group_var]]) | is.na(data[[response_var]]))) {
    stop("The data contains missing values. Please handle them before proceeding.")
  }
  
  # Check for non-numeric response variable
  if (!is.numeric(data[[response_var]])) {
    stop("The 'response_var' should be numeric.")
  }
  
  # Convert the data frame to a data table for efficient processing
  data <- data.table(data)
  
  # Check there are at least two distinct groups
  if (length(unique(data[[group_var]])) < 2) {
    stop("There must be at least two distinct groups in 'group_var'.")
  }
  
  # Perform Bayesian t-test
  bf_result <- ttestBF(formula = as.formula(paste(response_var, "~", group_var)), data = data)
  
  # For large datasets, print just the Bayes factor value, converted from logarithm
  print(exp(bf_result@bayesFactor$bf))
}

#' Plot Group Means
#'
#' This function uses ggplot2 to create a boxplot of the group means. 
#' The input data, group variable, and response variable should be provided.
#'
#' @param data A data frame or data table that contains the group_var and response_var.
#' @param group_var The name of the column in data that contains the group variable. It should be a factor or character variable.
#' @param response_var The name of the column in data that contains the response variable. It should be a numeric variable.
#'
#' @return This function returns a ggplot object that displays the boxplot of group means.
#' 
#' @examples
#' data(mtcars)
#' mtcars$group_var <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 2)
#' plot_group_means(mtcars, "group_var", "mpg")
#'
#' @importFrom ggplot2 ggplot aes_string geom_boxplot labs theme_minimal
#' @export
plot_group_means <- function(data, group_var, response_var) {
  data[[group_var]] <- as.factor(data[[group_var]])
  ggplot(data, aes_string(x = group_var, y = response_var)) +
    geom_boxplot() +
    labs(title = "Group Means", x = "Group", y = "Response") +
    theme_minimal()
}