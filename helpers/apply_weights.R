#' Apply demographic weights to numeric columns in a data frame
#'
#' The `apply_weights` function applies demographic weights to the numeric columns in a data frame.
#' It takes as input a data frame, demographic variables, demographic weights, and optional parameters
#' to customize the output.
#'
#' @param data A data frame.
#' @param demographic_vars A character vector specifying the names of the demographic variables to weight.
#' @param demographic_weights A list of named numeric vectors, where each named numeric vector represents
#'   the weights for a demographic variable.
#' @param columns_to_weight An optional character vector specifying the names of the numeric columns to weight.
#'   If not provided, all numeric columns will be weighted.
#' @param new_column_names An optional character vector specifying new column names. If not provided, the
#'   weighted column names will be created by appending a suffix to the original column names.
#' @param weight_suffix A character string indicating the suffix to be appended to the weighted column names
#'   for each demographic variable. Defaults to "_weight".
#' @param value_suffix A character string indicating the suffix to be appended to the total weighted column names.
#'   Defaults to "_weighted".
#' @param verbose A boolean value indicating whether to include the weights for each demographic variable in the
#'   output data frame. Defaults to FALSE.
#'
#' @return A data frame with weighted columns for each demographic variable.
#'
#' @importFrom stats complete.cases
#'
#' @examples
#' # Create a sample data frame
#' data <- data.frame(
#'   x = c(1, 2, 3, 4, 5),
#'   y = c(6, 7, 8, 9, 10),
#'   gender = c("male", "female", "male", "male", "female"),
#'   race = c("white", "black", "white", "black", "white"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Define demographic weights
#' demographic_weights <- list(
#'   gender = c(male = 0.5, female = 1.5),
#'   race = c(white = 1.2, black = 0.8)
#' )
#'
#' # Apply weights without verbose
#' result_no_verbose <- apply_weights(data, c("gender", "race"), demographic_weights)
#'
#' # View weighted data without verbose
#' print(result_no_verbose)
#'
#' # Apply weights with verbose
#' result_verbose <- apply_weights(data, c("gender", "race"), demographic_weights, verbose = TRUE)
#'
#' # View weighted data with verbose
#' print(result_verbose)
library(stats)
apply_weights <- function(data, demographic_vars, demographic_weights, columns_to_weight = NULL, 
                          new_column_names = NULL, weight_suffix = "_weight", value_suffix = "_weighted", verbose = FALSE) {
  
  # Check input types
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.character(demographic_vars)) stop("demographic_vars must be a character vector")
  if (!is.list(demographic_weights)) stop("demographic_weights must be a list of named numeric vectors")
  
  # Validate columns_to_weight and new_column_names
  if (!is.null(columns_to_weight) && !is.character(columns_to_weight)) stop("columns_to_weight must be a character vector")
  if (!is.null(new_column_names) && !is.character(new_column_names)) stop("new_column_names must be a character vector")
  
  # Convert demographic_vars to factors if necessary and validate demographic_weights
  data[demographic_vars] <- lapply(data[demographic_vars], factor)
  
  for (demographic_var in demographic_vars) {
    if (!is.null(demographic_weights[[demographic_var]])) {
      if (!is.numeric(demographic_weights[[demographic_var]])) {
        stop(paste0("demographic_weights for '", demographic_var, "' must be a named numeric vector"))
      }
      if (any(!names(demographic_weights[[demographic_var]]) %in% levels(data[[demographic_var]]))) {
        stop(paste0("demographic_weights for '", demographic_var, "' must have a name for every level of '", demographic_var, "'"))
      }
    }
  }
  
  # Select columns to weight
  if (is.null(columns_to_weight)) {
    columns_to_weight <- names(data)[sapply(data, is.numeric)]
  } else {
    columns_to_weight <- intersect(columns_to_weight, names(data))
  }
  
  # Remove missing values
  data <- data[complete.cases(data),]
  
  # Apply weights
  weighted_data <- data
  for (column_name in columns_to_weight) {
    weighted_data <- apply_demographic_weights(weighted_data, column_name, demographic_vars, demographic_weights,
                                               weight_suffix, value_suffix, verbose)
  }
  
  # Return results
  weighted_data
}

#' Apply demographic weights to a single column in a data frame
#'
#' The `apply_demographic_weights` function applies demographic weights to a single numeric column in a
#' data frame. This function is used internally by the `apply_weights` function, and it is not
#' recommended to use it directly.
#'
#' @param data A data frame.
#' @param column_name A character string specifying the name of the numeric column to weight.
#' @param demographic_vars A character vector specifying the names of the demographic variables to weight.
#' @param demographic_weights A list of named numeric vectors, where each named numeric vector represents
#'   the weights for a demographic variable.
#' @param weight_suffix A character string indicating the suffix to be appended to the weighted column names
#'   for each demographic variable.
#' @param value_suffix A character string indicating the suffix to be appended to the total weighted column names.
#' @param verbose A boolean value indicating whether to include the weights for each demographic variable in the
#'   output data frame.
#'
#' @return A data frame with the weighted column for the specified demographic variables.
#'
#' @keywords internal
apply_demographic_weights <- function(data, column_name, demographic_vars, demographic_weights, weight_suffix, value_suffix, verbose) {
  total_weights <- sapply(1:nrow(data), function(i) {
    prod(sapply(demographic_vars, function(demographic_var) {
      level <- as.character(data[i, demographic_var])
      if (!is.null(demographic_weights[[demographic_var]]) && !is.null(demographic_weights[[demographic_var]][[level]])) {
        if (verbose) data[[paste0(column_name, "_", demographic_var, weight_suffix)]][i] <<- demographic_weights[[demographic_var]][[level]]
        demographic_weights[[demographic_var]][[level]]
      } else {
        1
      }
    }))
  })
  
  data[[paste0(column_name, "_total", weight_suffix)]] <- total_weights
  data[[paste0(column_name, value_suffix)]] <- data[[column_name]] * total_weights
  
  return(data)
}