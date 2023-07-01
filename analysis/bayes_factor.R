# Load required libraries
library(BayesFactor)
library(data.table)
library(ggplot2)
library(rlang)

#' Perform Bayes Factor Analysis
#'
#' This function performs a Bayesian t-test to compare the means of two groups using the BayesFactor package. 
#' The input data, group variable, and response variable should be provided. 
#' The function checks for errors, performs the Bayesian t-test and returns the Bayes factor value.
#'
#' @param data A data frame or data table that contains the group_var and response_var.
#' @param group_var The name of the column in data that contains the group variable. It should be a factor or character variable with at least two levels.
#' @param response_var The name of the column in data that contains the response variable. It should be a numeric variable.
#' @param rm_na Logical, if TRUE missing values are removed from the data. Default is FALSE.
#' @param bf_type A character string specifying the type of Bayes Factor to compute. Default is 'bf'. Options include:
#' \itemize{
#'   \item{'bf'}{Standard Bayes factor}
#'   \item{'logbf'}{Natural logarithm of the Bayes factor}
#'   \item{'lrt'}{Likelihood ratio test statistic}
#'   \item{'loglrt'}{Natural logarithm of the likelihood ratio test statistic}
#' }
#'
#' @return The Bayes factor value for the performed Bayesian t-test.
#' @references Morey, R. D., & Rouder, J. N. (2018). BayesFactor: Computation of Bayes Factors for Common Designs. R package version 0.9.12-4.2.
#'
#' @examples
#' data(mtcars)
#' mtcars$group_var <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 2)
#' bayes_factor(mtcars, "group_var", "mpg", rm_na = TRUE, bf_type = "bf")
#'
#' @importFrom data.table data.table
#' @importFrom BayesFactor ttestBF
#' @importFrom stats is.numeric
#' @importFrom utils stop
#' @export
library(data.table)

bayes_factor <- function(data, group_var, response_var, rm_na = FALSE, bf_type = "bf") {
  
  # Convert the data frame to a data table for efficient processing
  if (!inherits(data, "data.table")) {
    data <- data.table(data)
  }
  
  # Error check: validate input parameters
  if (!all(c(group_var, response_var) %in% names(data))) {
    stop("Please ensure both 'group_var' and 'response_var' are in 'data'.")
  }
  
  # Handle missing data
  if (rm_na) {
    data[, (c(group_var, response_var)) := lapply(.SD, na.omit), .SDcols = c(group_var, response_var)]
  }
  else if (any(data[, sum(is.na(.SD)), .SDcols = c(group_var, response_var)] > 0)) {
    stop("The data contains missing values. Please handle them before proceeding.")
  }
  
  # Check for non-numeric response variable
  if (!is.numeric(data[[response_var]])) {
    stop("The 'response_var' should be numeric.")
  }
  
  # Check there are at least two distinct groups
  if (uniqueN(data[[group_var]]) < 2) {
    stop("There must be at least two distinct groups in 'group_var'.")
  }
  
  # Perform Bayesian t-test
  bf_result <- BayesFactor::ttestBF(formula = as.formula(paste(response_var, "~", group_var)), data = data)
  
  # Return Bayes factor result based on the chosen type
  return(exp(bf_result@bayesFactor[[bf_type]]))
}


#' Plot Group Means
#'
#' This function uses ggplot2 to create a boxplot of the group means. 
#' The input data, group variable, and response variable should be provided.
#'
#' @param data A data frame or data table that contains the group_var and response_var.
#' @param group_var The name of the column in data that contains the group variable. It should be a factor or character variable.
#' @param response_var The name of the column in data that contains the response variable. It should be a numeric variable.
#' @param title A character string for the plot title. Default is "Group Means".
#' @param x_label A character string for the x-axis label. Default is "Group".
#' @param y_label A character string for the y-axis label. Default is "Response".
#' @param fill_color A character string specifying the fill color for the boxplot. Default is "steelblue".
#' @param notch Logical, if TRUE makes the box plot a notched box plot. Default is FALSE.
#'
#' @return This function returns a ggplot object that displays the boxplot of group means.
#' 
#' @examples
#' data(mtcars)
#' mtcars$group_var <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 2)
#' plot_group_means(mtcars, "group_var", "mpg", title = "My Plot", fill_color = "orange", notch = TRUE)
#'
#' @importFrom ggplot2 ggplot aes_string geom_boxplot labs theme_minimal
#' @export
plot_group_means <- function(data, group_var, response_var, title = "Group Means", 
                             x_label = "Group", y_label = "Response", fill_color = "steelblue", 
                             notch = FALSE, rm_na = FALSE, bf_type = "bf", show_bf = TRUE) {
  
  # Convert the group variable to factor if it's not already
  if(!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }
  
  # Create the boxplot
  p <- ggplot(data, aes(x = .data[[sym(group_var)]], y = .data[[sym(response_var)]], fill = .data[[sym(group_var)]])) +
    geom_boxplot(notch = notch) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    scale_fill_manual(values = fill_color)
  
  # Calculate Bayes Factor for each group comparison
  if(show_bf) {
    bf <- bayes_factor(data, group_var, response_var, rm_na, bf_type)
    
    # Add Bayes Factor as annotation to the plot
    p <- p + geom_text(x = 1.5, y = max(data[[response_var]]), 
                       label = paste("Bayes Factor =", round(bf, 2)), hjust = 0)
  }
  
  return(p)
}