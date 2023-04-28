#' Table of Contents and Description
#' 
#' # LINE 036 - get_date_stats() - span, invalid, middle point, average distance, longest streak, shortest streak, max days between, minimum days between
#' # LINE 075 - get_factor_stats() - level counts, observation counts, observation percents, mode
#' # LINE 104 - get_gis_stats() - centroid, distance, proximity, convex hull, greatest distance
#' # LINE 149 - get_multi_stats() - nulls, percent not null, unique levels, highest value, lowest value, average length, max length, minimum length
#' # LINE 234 - get_numeric_stats() - mean, median, mode, sample standard deviation, population standard deviation, kurtosis, skewness, sum, absolute sum
#' # LINE 311 - get_science_stats() - mean, median, mode, sample standard deviation, population standard deviation, kurtosis, skewness, sum, absolute sum, minimum, max, number of nulls, percent of data not null (allows for grouping)
#'
#'
# Load library
library(moments)
#' Get statistics for dates
#'
#' This function takes a column of dates and returns statistics regarding the distribution of dates.
#'
#' @param column A vector of dates. 
#' @return A list containing the following statistics:
#' \describe{
#'   \item{span}{The number of days between the earliest and latest dates.}
#'   \item{invalid}{The number of missing values in the input column.}
#'   \item{middle_date}{The date that falls in the middle of the input date range.}
#'   \item{avg_dist}{The average number of days between consecutive dates.}
#'   \item{longest_streak}{The longest consecutive streak of days in the input column.}
#'   \item{shortest_streak}{The shortest consecutive streak of days in the input column.}
#'   \item{max_days_between}{The maximum number of days between consecutive dates in the input column.}
#'   \item{min_days_between}{The minimum number of days between consecutive dates in the input column.}
#' }
#'
#' @importFrom base is.vector as.vector mean sum diff min max sort unique rle is.na
#' @examples
#' column <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-05"))
#' get_date_stats(column)
#' @export
# Get statistics for dates
get_date_stats <- function(column) {
  if (!is.vector(column)) {
    column <- as.vector(column)
  }
  
  span <- max(column, na.rm = TRUE) - min(column, na.rm = TRUE)
  invalid <- sum(is.na(column))
  middle_date <- min(column, na.rm = TRUE) + 0.5 * span
  avg_dist <- mean(diff(sort(unique(column[!is.na(column)]))))
  streaks <- rle(diff(column))
  longest_streak <- max(streaks$lengths)
  shortest_streak <- min(streaks$lengths)
  max_days_between <- max(diff(column), na.rm = TRUE)
  min_days_between <- min(diff(column), na.rm = TRUE)
  
  return(list(span = span, invalid = invalid, middle_date = middle_date,
              avg_dist = avg_dist, longest_streak = longest_streak, shortest_streak = shortest_streak,
              max_days_between = max_days_between, min_days_between = min_days_between))
}


#' Get statistics for factors
#'
#' This function takes a column of factors and returns statistics regarding the distribution of factor levels.
#'
#' @param column A vector of factors. 
#' @return A list containing the following statistics:
#' \describe{
#'   \item{levels_count}{The number of unique levels in the input factor.}
#'   \item{obs_count}{A table of the frequency counts of each factor level.}
#'   \item{obs_perc}{The percentage of the total number of observations that each factor level represents.}
#'   \item{mode}{The most frequently occurring factor level.}
#' }
#'
#' @importFrom base length levels table round prop.table which.max
#' @examples
#' column <- factor(c("A", "B", "B", "C"))
#' get_factor_stats(column)
#' @export
get_factor_stats <- function(column) {
  levels_count <- length(levels(column))
  obs_count <- table(column)
  obs_perc <- round(100 * prop.table(obs_count), 2)
  mode_val <- levels(column)[which.max(obs_count)]
  
  return(list(levels_count = levels_count, obs_count = obs_count,
              obs_perc = obs_perc, mode = mode_val))
}


#' Get statistics for geographic coordinates
#'
#' This function takes a data frame with longitude and latitude columns, and returns various statistics related to the coordinates.
#'
#' @param column A data frame with longitude and latitude columns. 
#' @return A list containing the following statistics:
#' \describe{
#'   \item{centroid}{The centroid of the coordinates.}
#'   \item{distance}{A matrix of distances between the coordinates.}
#'   \item{proximity}{A matrix of the two nearest points for each coordinate.}
#'   \item{convex_hull}{The convex hull of the coordinates.}
#'   \item{greatest_distance}{The greatest distance between any two coordinates.}
#' }
#'
#' @importFrom sf st_as_sf st_centroid st_distance st_nearest_points st_convex_hull
#' @examples
#' coords <- data.frame(longitude = c(-122.4194, -118.2439, -122.0574), latitude = c(37.7749, 34.0522, 37.3854))
#' get_gis_stats(coords)
get_gis_stats <- function(column) {
  coords <- c("longitude", "latitude")
  if (is.data.frame(column) && all(tolower(colnames(column)) %in% coords)) {
    coordinates <- sf::st_as_sf(column, coords = coords, crs = 4326)
    centroid <- sf::st_centroid(coordinates)
    distance_matrix <- sf::st_distance(coordinates)
    proximity <- sf::st_nearest_points(coordinates, coordinates) 
    convex_hull <- sf::st_convex_hull(coordinates)
    greatest_distance <- max(as.vector(distance_matrix))
    
    return(list(centroid = centroid, distance = distance_matrix,
                proximity = proximity, convex_hull = convex_hull,
                greatest_distance = greatest_distance))
  } else {
    return(NA)
  }
}


#' Get statistics for multiple types of variables
#'
#' This function takes a data frame and returns basic statistics for each column.
#'
#' @param data A data frame with columns of various types. 
#' @return A data frame containing the following statistics for each column:
#' \describe{
#'   \item{column}{The name of the column.}
#'   \item{nulls}{The number of null values in the column.}
#'   \item{pct_non_null}{The percentage of non-null values in the column.}
#'   \item{unique_levels}{The number of unique levels in the column.}
#'   \item{highest_value}{The highest value in the column.}
#'   \item{lowest_value}{The lowest value in the column.}
#'   \item{avg_length}{The average length of values in the column.}
#'   \item{max_length}{The maximum length of values in the column.}
#'   \item{min_length}{The minimum length of values in the column.}
#' }
#'
#' @importFrom base sum is.na length unique as.character nchar min max
#' @importFrom base typeof data.frame
#' @examples
#' data <- data.frame(x = c(1, 2, 3, 1, 5),
#'                    y = c("foo", "bar", "foo", "baz", "haq"),
#'                    z = factor(c("a", "b", "c", "a", "b")))
#' get_multi_stats(data)
#' @export
get_multi_stats <- function(data) {
  
  # initialize an empty data frame to store the results
  results <- data.frame(column = character(), 
                        nulls = numeric(),
                        pct_non_null = numeric(),
                        unique_levels = numeric(),
                        highest_value = character(),
                        lowest_value = character(),
                        avg_length = numeric(),
                        max_length = numeric(),
                        min_length = numeric(),
                        stringsAsFactors = FALSE)  # add this element to prevent character columns from being converted to factors
  
  for (i in 1:ncol(data)) {
    
    col_name <- names(data)[i]
    col_data <- data[[i]]
    col_type <- typeof(col_data)
    
    # calculate the number of nulls and percentage non-null
    null_count <- sum(is.na(col_data))
    total_count <- length(col_data)
    pct_non_null <- 100 - (null_count / total_count) * 100
    
    # calculate the number of unique levels
    unique_levels <- length(unique(col_data))
    
    # calculate the highest and lowest values
    if (is.numeric(col_data)) {
      highest_value <- max(col_data, na.rm = TRUE)
      lowest_value <- min(col_data, na.rm = TRUE)
    } else if (is.character(col_data)) {
      highest_value <- max(col_data, na.rm = TRUE)
      lowest_value <- min(col_data, na.rm = TRUE)
    } else if (is.factor(col_data)) {
      highest_value <- levels(col_data)[length(levels(col_data))]
      lowest_value <- levels(col_data)[1]
    } else {
      highest_value <- NA
      lowest_value <- NA
    }
    
    # calculate the average, max, and min lengths
    avg_length <- mean(nchar(as.character(col_data)), na.rm = TRUE)
    max_length <- max(nchar(as.character(col_data)), na.rm = TRUE)
    min_length <- min(nchar(as.character(col_data)), na.rm = TRUE)
    
    # add the results to the data frame
    results[i,] <- c(col_name, null_count, pct_non_null, unique_levels, highest_value, lowest_value, 
                     avg_length, max_length, min_length)
  }
  
  # reorder the columns in the data frame to match the order in the function definition
  results <- results[, c("column", "nulls", "pct_non_null", "unique_levels", "highest_value", "lowest_value", "avg_length", "max_length", "min_length")]
  
  # return the results data frame
  return(results)
}


#' Get statistics for numerics
#'
#' This function takes a column of numeric values and returns basic statistics for the column.
#'
#' @param column A vector of numeric values. 
#' @return A vector containing the following statistics:
#' \describe{
#'   \item{mean_val}{The mean of the input numeric values.}
#'   \item{median_val}{The median of the input numeric values.}
#'   \item{mode_val}{The mode of the input numeric values.}
#'   \item{sample_sd}{The sample standard deviation of the input numeric values.}
#'   \item{pop_sd}{The population standard deviation of the input numeric values.}
#'   \item{kurtosis_val}{The kurtosis of the input numeric values.}
#'   \item{skewness_val}{The skewness of the input numeric values.}
#'   \item{sum_val}{The sum of the input numeric values.}
#'   \item{abs_sum_val}{The sum of the absolute values of the input numeric values.}
#' }
#'
#' @importFrom base mean median is.na sd sum abs
#' @importFrom moments kurtosis skewness
#' @examples
#' column <- c(1, 2, 3, 4, 5, NA)
#' get_numeric_stats(column)
#' @export
get_numeric_stats <- function(column) {
  mean_val <- if (all(is.na(column))) {
    NA
  } else {
    mean(column, na.rm = TRUE)
  }
  
  median_val <- if (all(is.na(column))) {
    NA
  } else {
    median(column, na.rm = TRUE)
  }
  mode_val <- if (length(column[!is.na(column)]) > 0) {
    as.numeric(names(sort(-table(column), decreasing = TRUE)))[1]
  } else {
    NA
  }
  sample_sd <- sd(column, na.rm = TRUE) / sqrt(length(column) - 1)
  pop_sd <- sd(column, na.rm = TRUE)
  kurtosis_val <- kurtosis(column, na.rm = TRUE)
  skewness_val <- skewness(column, na.rm = TRUE)
  sum_val <- sum(column, na.rm = TRUE)
  abs_sum_val <- sum(abs(column), na.rm = TRUE)
  
  return(c(mean_val, median_val, mode_val, sample_sd, pop_sd, kurtosis_val, skewness_val, sum_val, abs_sum_val))
}


#' get_science_stats
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
#' get_science_stats(mtcars) -> mtcars_stats
#'
#' # Calculate statistics for subsets of a data frame
#' data(mtcars)
#' get_science_stats(mtcars, group = "cyl") -> mtcars_stats_group_cycle
#' @export
get_science_stats <- function(df, group = NULL) {
  
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