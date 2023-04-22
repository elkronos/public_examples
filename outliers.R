#' Outlier detection methods and their recommended usage
#' 
#' \describe{
#' \item{\code{z_outliers}:}{When the data is normally distributed and has no significant outliers.}
#' \item{\code{tukey_outliers}:}{When the data has outliers that are beyond the extremes of the quartiles.}
#' \item{\code{mahalanobis_outliers}:}{When the data has a multivariate normal distribution with correlated features.}
#' \item{\code{grubbs_outliers}:}{When the data has a univariate normal distribution and only one outlier.}
#' \item{\code{mad_outliers}:}{When the data has a skewed distribution and a few extreme values.}
#' \item{\code{iglewicz_hoaglin_outliers}:}{When the data has a skewed distribution and a few extreme values.}
#' \item{\code{isolation_forest_outliers}:}{When the data has high-dimensional features and is not normally distributed.}
#' \item{\code{dbscan_outliers}:}{When the data has a high density of points around the outliers.}
#' \item{\code{one_class_svm_outliers}:}{When the data has a few anomalies and is not easily separable.}
#' \item{\code{elliptic_envelope_outliers}:}{When the data has a multivariate normal distribution with few outliers.}
#' \item{\code{lof_outliers}:}{When the data has a complex structure and outliers are not isolated.}
#' }
#' @export

# Load packages
library(e1071)
library(robustbase)
library(tidyverse)
library(dbscan)
library(isotree)
library(MASS)

# Set seed
set.seed(123)

###################################################### Outlier functions

# Z-score threshold method
z_outliers <- function(data, column, z_thresh) {
  z_score <- (data[, column] - mean(data[, column])) / sd(data[, column])
  return(ifelse(abs(z_score) > z_thresh, 1, 0))
}

# Tukeys Fences
tukey_outliers <- function(data, column, tukey_mult) {
  Q1 <- quantile(data[, column], 0.25)
  Q3 <- quantile(data[, column], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - tukey_mult * IQR
  upper_bound <- Q3 + tukey_mult * IQR
  return(ifelse(data[, column] < lower_bound | data[, column] > upper_bound, 1, 0))
}

# Mahalanobis Distance
mahalanobis_outliers <- function(data, column, mahalanobis_thresh) {
  cov_mat <- cov(data[, sapply(data, is.numeric) & names(data) != column])
  center <- apply(data[, sapply(data, is.numeric) & names(data) != column], 2, mean)
  md <- mahalanobis(data[, sapply(data, is.numeric) & names(data) != column], center, cov_mat, inverted = TRUE)
  return(ifelse(md > mahalanobis_thresh, 1, 0))
}

# Grubbs method
grubbs_outliers <- function(data, column, grubbs_thresh) {
  n <- nrow(data)
  g <- (abs(data[, column] - mean(data[, column])) / sd(data[, column]))
  gmax <- max(g)
  pval <- 2 * pt(gmax, n - 2, lower.tail = FALSE)
  return(ifelse(pval < grubbs_thresh / n, 1, 0))
}

# Median Absolute Deviation Method
mad_outliers <- function(data, column, mad_mult) {
  med <- median(data[, column], na.rm = TRUE)
  mad <- abs(data[, column] - med)
  mad_thresh <- mad_mult * median(mad, na.rm = TRUE)
  return(ifelse(mad > mad_thresh, 1, 0))
}

# Iglewicz and Hoaglin's Modified Z-score
iglewicz_hoaglin_outliers <- function(data, column, threshold = 3.5) {
  med <- median(data[, column], na.rm = TRUE)
  mad <- stats::mad(data[, column], na.rm = TRUE)
  modified_z <- abs((data[, column] - med) / mad)
  return(ifelse(modified_z > threshold, 1, 0))
}

# Isolation Forest
isolation_forest_outliers <- function(data, column, contamination, ndim = 1, ntree = 100, nthreads = 1) {
  # Subset the data to the column of interest
  X <- data[, column, drop = FALSE]
  
  # Build the isolation forest model
  model <- isolation.forest(X, ndim = ndim, ntree = ntree, nthreads = nthreads)
  
  # Predict the outlier scores
  scores <- predict(model, X, output_type = "score")
  
  # Return the binary outlier labels
  return(as.integer(scores > contamination))
}

# DBSCAN
dbscan_outliers <- function(data, column, eps = 0.5, minPts = 5) {
  d <- as.matrix(dist(data.frame(data[, column])))
  dbscan_result <- dbscan::dbscan(d, eps = eps, minPts = minPts)
  return(ifelse(dbscan_result$cluster == 0, 1, 0))
}

# One-Class SVM
one_class_svm_outliers <- function(data, column, nu = 0.05) {
  svm_model <- svm(data.frame(data[column]), scale = TRUE, type = "one-classification", nu = nu, kernel = "radial")
  is_outlier <- predict(svm_model, data.frame(data[column]), decision.values = TRUE) < 0
  return(as.integer(is_outlier))
}

# Elliptic Envelope
elliptic_envelope_outliers <- function(data, column, contamination = 0.1) {
  numeric_data <- data.frame(data[, sapply(data, is.numeric), drop = FALSE])
  cov_robust <- cov.rob(numeric_data)
  mahalanobis_distances <- mahalanobis(numeric_data, cov_robust$center, cov_robust$cov, inverted = TRUE)
  cutoff <- qchisq(1 - contamination, df = ncol(numeric_data))
  is_outlier <- mahalanobis_distances > cutoff
  return(as.integer(is_outlier))
}

# Local Outlier Factor (LOF)
lof_outliers <- function(data, column, minPts = 6, lof_thresh = 1.5) {
  lof_scores <- dbscan::lof(data.frame(data[, column]), minPts = minPts)
  outlier_flags <- ifelse(lof_scores > lof_thresh, 1, 0)
  return(outlier_flags)
}

#' @title Detect Outliers in a Data Frame
#' @description This function detects outliers in a data frame using various outlier detection methods.
#'
#' @param data Input data frame containing numeric columns
#' @param methods Character vector of outlier detection methods to use
#'
#' @param data a data frame containing the data to be analyzed
#' @param methods a character vector specifying the outlier detection methods to be used. Options include "z", "tukey", "mahalanobis", "grubbs", "mad", "iglewicz_hoaglin", "isolation_forest", "dbscan", "one_class_svm", "elliptic_envelope", and "lof". Default is to use all methods.
#' @param z_thresh a numeric value specifying the Z-score threshold for the "z" method. Default is 1.96 (corresponding to a 95% confidence level).
#' @param tukey_mult a numeric value specifying the Tukey multiplier for the "tukey" method. Default is 1.5.
#' @param mahalanobis_thresh a numeric value specifying the threshold for the "mahalanobis" method. Default is calculated using a 95% confidence level and the number of columns in the data frame.
#' @param grubbs_thresh a numeric value specifying the Grubbs threshold for the "grubbs" method. Default is 0.05.
#' @param mad_mult a numeric value specifying the Median Absolute Deviation multiplier for the "mad" method. Default is 3.
#' @param iglewicz_hoaglin_thresh a numeric value specifying the threshold for the "iglewicz_hoaglin" method. Default is 3.5.
#' @param isolation_forest_contamination a numeric value specifying the contamination for the "isolation_forest" method. Default is 0.1.
#' @param dbscan_eps a numeric value specifying the epsilon value for the "dbscan" method. Default is 0.5.
#' @param dbscan_minPts an integer specifying the minimum number of points for the "dbscan" method. Default is 5.
#' @param one_class_svm_nu a numeric value specifying the nu value for the "one_class_svm" method. Default is 0.05.
#' @param elliptic_envelope_contamination a numeric value specifying the contamination for the "elliptic_envelope" method. Default is 0.1.
#' @param lof_minPts an integer specifying the minimum number of points for the "lof" method. Default is 5.
#' @param lof_thresh a numeric value specifying the Local Outlier Factor threshold for the "lof" method. Default is 1.
#' @return a data frame with columns indicating the outlier status for each method
#' 
#' @return A list of data frames, each containing the outliers detected by a specific method
#' 
#' @examples
#' # Create example data
#' set.seed(123)
#' example_data <- data.frame(
#'   A = rnorm(100, 10, 2),
#'   B = rnorm(100, 20, 5),
#'   C = rnorm(100, 30, 10)
#' )
#'
#' # Add some artificial outliers
#' example_data[c(25, 50, 75), "B"] <- c(40, 10, 35)
#' example_data[c(10, 90), "C"] <- c(5, 50)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("z_score")
#'
#' # Use function for Z-score method
#' z_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("tukey")
#'
#' # Use function for Tukey's fences method
#' tukey_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("mahalanobis")
#'
#' # Use function for Mahalanobis distance method
#' mahalanobis_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("grubbs")
#'
#' # Use function for Grubbs method
#' grubbs_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("mad")
#'
#' # Use function for Median Absolute Deviation (MAD) method
#' mad_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("iglewicz_hoaglin")
#'
#' # Use function for Iglewicz and Hoaglin's Modified Z-score method
#' iglewicz_hoaglin_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("isolation_forest")
#'
#' # Use function for Isolation Forest method
#' isolation_forest_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("dbscan")
#'
#' # Use function for DBSCAN method
#' dbscan_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("one_class_svm")
#'
#' # Use function for One-Class SVM method
#' one_class_svm_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("elliptic_envelope")
#'
#' # Use function for Elliptic Envelope method
#' elliptic_envelope_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify outlier detection method to use
#' outlier_methods <- c("lof")
#'
#' # Use function for Local Outlier Factor (LOF) method
#' lof_results <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Specify methods to use for outlier detection
#' outlier_methods <- c("z", "tukey", "mahalanobis", "grubbs", "mad", 
#'                      "iglewicz_hoaglin", "isolation_forest", "dbscan", 
#'                      "one_class_svm", "elliptic_envelope", "lof")
#'
#' # Use function
#' all_methods <- detect_outliers(example_data, methods = outlier_methods)
#'
#' # Review results for each method
#' head(z_results)
#' head(tukey_results)
#' head(mahalanobis_results)
#' head(grubbs_results)
#' head(mad_results)
#' head(iglewicz_hoaglin_results)
#' head(isolation_forest_results)
#' head(dbscan_results)
#' head(one_class_svm_results)
#' head(elliptic_envelope_results)
#' head(lof_results)
#' head(all_methods)
#' 
#' @import e1071
#' @importFrom e1071 svm
#'
#' @import robustbase
#' @importFrom robustbase covMcd
#'
#' @import tidyverse
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot geom_point
#'
#' @import dbscan
#' @importFrom dbscan dbscan
#'
#' @import isotree
#' @importFrom isotree isolationForest
#'
#' @import MASS
#' @importFrom MASS mahalanobis
#' 
#' @export

###################################################### Run multiple outliers

# Main outliers detection function
detect_outliers <- function(data, methods = c("z", "tukey", "mahalanobis", "grubbs", "mad", "iglewicz_hoaglin", "isolation_forest", "dbscan", "one_class_svm", "elliptic_envelope", "lof"), z_thresh = 1.96, tukey_mult = 1.5, mahalanobis_thresh = qchisq(0.95, ncol(data)), grubbs_thresh = 0.05, mad_mult = 3, iglewicz_hoaglin_thresh = 3.5, isolation_forest_contamination = 0.1, dbscan_eps = 0.5, dbscan_minPts = 5, one_class_svm_nu = 0.05, elliptic_envelope_contamination = 0.1, lof_minPts = 5, lof_thresh = 1) {
  # Initialize output data frame
  results <- data.frame(row.names = row.names(data), data)
  
  # Loop through numeric columns
  for (column in names(data)[sapply(data, is.numeric)]) {
    # Remove missing values
    data_no_na <- data[complete.cases(data[, column]), ]
    
    # Calculate and store outliers for each method
    tryCatch({
      if ("z" %in% methods) {
        results[paste0(column, "_z_outlier")] <- ifelse(!complete.cases(data[, column]), NA, z_outliers(data_no_na, column, z_thresh))
      }
    }, error = function(e) {
      message("Error in Z-score method: ", e$message)
    })
    
    tryCatch({
      if ("tukey" %in% methods) {
        results[paste0(column, "_t_outlier")] <- ifelse(!complete.cases(data[, column]), NA, tukey_outliers(data_no_na, column, tukey_mult))
      }
    }, error = function(e) {
      message("Error in Tukey method: ", e$message)
    })
    
    tryCatch({
      if ("mahalanobis" %in% methods) {
        results[paste0(column, "_md_outlier")] <- ifelse(!complete.cases(data[, column]), NA, mahalanobis_outliers(data_no_na, column, mahalanobis_thresh))
      }
    }, error = function(e) {
      message("Error in Mahalanobis method: ", e$message)
    })
    
    tryCatch({
      if ("grubbs" %in% methods) {
        results[paste0(column, "_g_outlier")] <- ifelse(!complete.cases(data[, column]), NA, grubbs_outliers(data_no_na, column, grubbs_thresh))
      }
    }, error = function(e) {
      message("Error in Grubbs method: ", e$message)
    })
    
    tryCatch({
      if ("mad" %in% methods) {
        results[paste0(column, "_mad_outlier")] <- ifelse(!complete.cases(data[, column]), NA, mad_outliers(data_no_na, column, mad_mult))
      }
    }, error = function(e) {
      message("Error in MAD method: ", e$message)
    })
    
    tryCatch({
      if ("iglewicz_hoaglin" %in% methods) {
        data_no_na$iglewicz_hoaglin_outlier <- iglewicz_hoaglin_outliers(data_no_na, column, iglewicz_hoaglin_thresh)
        results[paste0(column, "_iglewicz_hoaglin_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$iglewicz_hoaglin_outlier)
      }
    }, error = function(e) {
      message("Error in Iglewicz-Hoaglin method: ", e$message)
    })
    
    if ("isolation_forest" %in% methods) {
      tryCatch(
        {
          data_no_na$isolation_forest_outlier <- isolation_forest_outliers(data_no_na, column, isolation_forest_contamination)
          results[paste0(column, "_isolation_forest_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$isolation_forest_outlier)
        }, error = function(e) {
          message("Error in Isolation Forest method: ", e$message)
        }
      )
    }
    
    tryCatch({
      if ("dbscan" %in% methods) {
        data_no_na$dbscan_outlier <- dbscan_outliers(data_no_na, column, dbscan_eps, dbscan_minPts)
        results[paste0(column, "_dbscan_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$dbscan_outlier)
      }
    }, error = function(e) {
      message("Error in DBSCAN method: ", e$message)
    })
    
    if ("one_class_svm" %in% methods) {
      tryCatch(
        {
          data_no_na$one_class_svm_outlier <- one_class_svm_outliers(data_no_na, column, one_class_svm_nu)
          results[paste0(column, "_one_class_svm_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$one_class_svm_outlier)
        }, error = function(e) {
          message("Error in One-Class SVM method: ", e$message)
        }
      )
    }
    
    if ("elliptic_envelope" %in% methods) {
      tryCatch(
        {
          # Create a separate variable for the Elliptic Envelope method
          data_no_na_ee <- data[complete.cases(data[, column]), ]
          
          # Run the function on the data without missing values
          data_no_na_ee$elliptic_envelope_outlier <- elliptic_envelope_outliers(data_no_na_ee, column, elliptic_envelope_contamination)
          
          # Assign the results to the original data, accounting for missing values
          results[paste0(column, "_elliptic_envelope_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na_ee$elliptic_envelope_outlier)
        }, error = function(e) {
          message("Error in Elliptic Envelope method: ", e$message)
        }
      )
    }
    
    tryCatch({
      if ("lof" %in% methods) {
        data_no_na$lof_outlier <- lof_outliers(data_no_na, column, lof_minPts, lof_thresh)
        results[paste0(column, "_lof_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$lof_outlier)
      }
    }, error = function(e) {
      message("Error in Local Outlier Factor (LOF) method: ", e$message)
    })
  }
  
  return(results)
}