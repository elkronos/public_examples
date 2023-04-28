#' get_stats
#'
#' This function calculates various summary statistics of a data frame or its subsets based on a grouping variable.
#'
#' @param df A data frame.
#' @param group A grouping variable indicating the column in the input data frame \code{df} to group by, or \code{NULL} to calculate statistics for the whole data frame.
#' @return A list of data frames or a single data frame (if \code{group=NULL}). Each data frame contains the following columns: "Column Name", "Data Type", "Mean", "Median", "Mode", "Sample SD", "Population SD", "Kurtosis", "Skewness", "Sum", "Absolute Sum", "Min", "Max", "Num NAs", "% Non-Null Responses".
#' 
#' @details
#' This function calculates the following statistics for each column in the input data frame:
#' 
#' - "Mean": The arithmetic average of a column of numeric values.
#' - "Median": The middle value of a column of numeric values.
#' - "Mode": The most common value(s) in a column of numeric values.
#' - "Sample SD": The sample standard deviation of a column of numeric values.
#' - "Population SD": The population standard deviation of a column of numeric values.
#' - "Kurtosis": A measure of the peakedness of the distribution of a column of numeric values.
#' - "Skewness": A measure of the asymmetry of the distribution of a column of numeric values.
#' - "Sum": The sum of a column of numeric values.
#' - "Absolute Sum": The sum of the absolute values of a column of numeric values.
#' - "Min": The minimum value of a column of any type.
#' - "Max": The maximum value of a column of any type.
#' - "Num NAs": The number of missing values in a column.
#' - "% Non-Null Responses": The percentage of non-missing values in a column.
#'
#' When the \code{group} parameter is not \code{NULL}, the statistics are calculated for each subset of the input data frame, based on the unique values of the \code{group} column.
#' 
#' This function requires the \code{moments} package. The following functions from the \code{moments} package are used:
#' 
#' \describe{
#'   \item{\code{mean}:}{calculates the arithmetic mean of a numeric vector}
#'   \item{\code{median}:}{calculates the median of a numeric vector}
#'   \item{\code{mode}:}{calculates the mode of a numeric vector}
#'   \item{\code{sd}:}{calculates the standard deviation of a numeric vector}
#'   \item{\code{kurtosis}:}{calculates the kurtosis of a numeric vector}
#'   \item{\code{skewness}:}{calculates the skewness of a numeric vector}
#' }
#'
#' @importFrom moments mean median mode sd kurtosis skewness
#' @import moments
#' @examples
#' # Calculate statistics for a whole data frame
#' data(mtcars)
#' get_stats(mtcars) -> mtcars_stats
#'
#' # Calculate statistics for subsets of a data frame
#' data(mtcars)
#' get_stats(mtcars, group = "cyl") -> mtcars_stats_group_cycle
# Load library
library(moments)
# Save function
get_stats <- function(df, group = NULL) {
  
  if (is.null(group)) {
    result <- data.frame(matrix(ncol = 11, nrow = ncol(df)))
    colnames(result) <- c("Column Name", "Data Type", "Mean", "Median", "Mode",
                          "Sample SD", "Population SD", "Kurtosis", "Skewness", "Sum", "Absolute Sum")
    
    for (i in 1:ncol(df)) {
      column <- df[, i]
      column_type <- ifelse(is.numeric(column), "Numeric",
                            ifelse(is.factor(column), "Categorical",
                                   ifelse(is.character(column), "Character",
                                          ifelse(inherits(column, "Date"), "Date",
                                                 ifelse(inherits(column, "POSIXt"), "Date/Time", "Unknown")))))
      result[i, 1] <- colnames(df)[i]
      result[i, 2] <- column_type
      
      if (column_type == "Numeric") {
        result[i, 3] <- mean(column, na.rm = TRUE)
        result[i, 4] <- median(column, na.rm = TRUE)
        mode_value <- as.numeric(names(sort(-table(column), decreasing = TRUE)))[1]
        result[i, 5] <- mode_value
        result[i, 6] <- sd(column, na.rm = TRUE) / sqrt(nrow(df) - 1)
        result[i, 7] <- sd(column, na.rm = TRUE)
        result[i, 8] <- kurtosis(column, na.rm = TRUE)
        result[i, 9] <- skewness(column, na.rm = TRUE)
        result[i, 10] <- sum(column, na.rm = TRUE)
        result[i, 11] <- sum(abs(column), na.rm = TRUE)
      }
    }
    
    result <- cbind(result, min = sapply(df, function(x) 
      if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        format(min(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
      } else {
        min(x, na.rm = TRUE)
      }),
      max = sapply(df, function(x) 
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
          format(max(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
        } else {
          max(x, na.rm = TRUE)
        }),
      n_na = colSums(is.na(df)),
      perc_non_null = round(100 * (1 - colSums(is.na(df)) / nrow(df)), 2))
    colnames(result)[12:15] <- c("Min", "Max", "Num NAs", "% Non-Null Responses")
    return(result)
  }
  
  result_list <- list()
  
  levels <- unique(df[, group])
  for (level in levels) {
    level_df <- df[df[, group] == level, ]
    result <- data.frame(matrix(ncol = 11, nrow = ncol(level_df)))
    colnames(result) <- c("Column Name", "Data Type", "Mean", "Median", "Mode",
                          "Sample SD", "Population SD", "Kurtosis", "Skewness", "Sum", "Absolute Sum")
    
    for (i in 1:ncol(level_df)) {
      column <- level_df[, i]
      column_type <- ifelse(is.numeric(column), "Numeric",
                            ifelse(is.factor(column), "Categorical",
                                   ifelse(is.character(column), "Character",
                                          ifelse(inherits(column, "Date"), "Date",
                                                 ifelse(inherits(column, "POSIXt"), "Date/Time", "Unknown")))))
      result[i, 1] <- colnames(level_df)[i]
      result[i, 2] <- column_type
      
      if (column_type == "Numeric") {
        result[i, 3] <- mean(column, na.rm = TRUE)
        result[i, 4] <- median(column, na.rm = TRUE)
        mode_value <- as.numeric(names(sort(-table(column), decreasing = TRUE)))[1]
        result[i, 5] <- mode_value
        result[i, 6] <- sd(column, na.rm = TRUE) / sqrt(nrow(level_df) - 1)
        result[i, 7] <- sd(column, na.rm = TRUE)
        result[i, 8] <- kurtosis(column, na.rm = TRUE)
        result[i, 9] <- skewness(column, na.rm = TRUE)
        result[i, 10] <- sum(column, na.rm = TRUE)
        result[i, 11] <- sum(abs(column), na.rm = TRUE)
      }
    }
    
    result <- cbind(result, min = sapply(level_df, function(x) 
      if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        format(min(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
      } else {
        min(x, na.rm = TRUE)
      }),
      max = sapply(level_df, function(x) 
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
          format(max(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
        } else {
          max(x, na.rm = TRUE)
        }),
      n_na = colSums(is.na(level_df)),
      perc_non_null = round(100 * (1 - colSums(is.na(level_df)) / nrow(level_df)), 2))
    colnames(result)[12:15] <- c("Min", "Max", "Num NAs", "% Non-Null Responses")
    result_list[[as.character(level)]] <- result
  }
  
  return(result_list)
}