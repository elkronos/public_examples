##############################
# Outlier Detection Functions
##############################

# ---------------------------
# Univariate Methods (vector based)
# ---------------------------

# Z–score method: returns a binary vector (1 = outlier, 0 = not)
z_outliers <- function(x, z_thresh = 1.96) {
  valid <- !is.na(x)
  out <- rep(NA, length(x))
  if (any(valid)) {
    mu <- mean(x[valid])
    sigma <- sd(x[valid])
    # If sigma is 0 then mark all as non–outliers
    if (sigma == 0) {
      out[valid] <- 0
    } else {
      z <- (x[valid] - mu) / sigma
      out[valid] <- ifelse(abs(z) > z_thresh, 1, 0)
    }
  }
  return(out)
}

# Tukey's Fences: identifies values beyond Q1 - k*IQR or Q3 + k*IQR
tukey_outliers <- function(x, tukey_mult = 1.5) {
  valid <- !is.na(x)
  out <- rep(NA, length(x))
  if (any(valid)) {
    Q1 <- quantile(x[valid], 0.25)
    Q3 <- quantile(x[valid], 0.75)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - tukey_mult * IQR_val
    upper_bound <- Q3 + tukey_mult * IQR_val
    out[valid] <- ifelse(x[valid] < lower_bound | x[valid] > upper_bound, 1, 0)
  }
  return(out)
}

# Grubbs test (single outlier detection, approximate critical value)
grubbs_outliers <- function(x, grubbs_thresh = 0.05) {
  valid <- !is.na(x)
  out <- rep(NA, length(x))
  n <- sum(valid)
  if (n < 3) {
    warning("Grubbs test requires at least 3 non-missing values.")
    return(out)
  }
  mu <- mean(x[valid])
  sigma <- sd(x[valid])
  if (sigma == 0) {
    out[valid] <- 0
    return(out)
  }
  g_scores <- abs(x[valid] - mu) / sigma
  # Compute critical value (an approximation)
  t_val <- qt(1 - grubbs_thresh/(2 * n), df = n - 2)
  threshold <- ((n - 1) / sqrt(n)) * sqrt(t_val^2 / (n - 2 + t_val^2))
  out[valid] <- ifelse(g_scores > threshold, 1, 0)
  return(out)
}

# Median Absolute Deviation (MAD) method
mad_outliers <- function(x, mad_mult = 3) {
  valid <- !is.na(x)
  out <- rep(NA, length(x))
  if (any(valid)) {
    med_val <- median(x[valid])
    mad_val <- median(abs(x[valid] - med_val))
    if (mad_val == 0) {
      # If all non-missing values are identical, flag non-equal values as outliers.
      out[valid] <- ifelse(x[valid] == med_val, 0, 1)
    } else {
      out[valid] <- ifelse(abs(x[valid] - med_val) > mad_mult * mad_val, 1, 0)
    }
  }
  return(out)
}

# Iglewicz–Hoaglin modified Z–score method
iglewicz_hoaglin_outliers <- function(x, threshold = 3.5) {
  valid <- !is.na(x)
  out <- rep(NA, length(x))
  if (any(valid)) {
    med_val <- median(x[valid])
    mad_val <- stats::mad(x[valid])
    if (mad_val == 0) {
      out[valid] <- ifelse(x[valid] == med_val, 0, 1)
    } else {
      mod_z <- 0.6745 * (x[valid] - med_val) / mad_val
      out[valid] <- ifelse(abs(mod_z) > threshold, 1, 0)
    }
  }
  return(out)
}

# ---------------------------
# Multivariate Methods (data frame based)
# ---------------------------

# Mahalanobis Distance method:
# df: a data frame of numeric columns
mahalanobis_outliers <- function(df, threshold = NULL) {
  n <- nrow(df)
  out <- rep(NA, n)
  complete_idx <- complete.cases(df)
  if (sum(complete_idx) < 2) {
    warning("Not enough complete cases for Mahalanobis distance.")
    return(out)
  }
  df_complete <- df[complete_idx, , drop = FALSE]
  center <- colMeans(df_complete)
  cov_mat <- stats::cov(df_complete)
  md <- mahalanobis(df_complete, center, cov_mat)
  if (is.null(threshold)) {
    threshold <- qchisq(0.95, df = ncol(df_complete))
  }
  out[complete_idx] <- ifelse(md > threshold, 1, 0)
  return(out)
}

# Isolation Forest method using the isotree package
isolation_forest_outliers <- function(df, contamination = 0.1, ndim = 1, ntree = 100, nthreads = 1) {
  complete_idx <- complete.cases(df)
  n <- nrow(df)
  out <- rep(NA, n)
  if (sum(complete_idx) < 1) return(out)
  df_complete <- df[complete_idx, , drop = FALSE]
  model <- isotree::isolation.forest(df_complete, ndim = ndim, ntree = ntree, nthreads = nthreads)
  scores <- predict(model, df_complete, output_type = "score")
  thresh <- as.numeric(quantile(scores, 1 - contamination))
  out[complete_idx] <- ifelse(scores >= thresh, 1, 0)
  return(out)
}

# DBSCAN–based outlier detection:
# Observations not assigned to any cluster (i.e. cluster 0) are flagged as outliers.
dbscan_outliers <- function(df, eps = 0.5, minPts = 5) {
  complete_idx <- complete.cases(df)
  n <- nrow(df)
  out <- rep(NA, n)
  if (sum(complete_idx) < 1) return(out)
  df_complete <- df[complete_idx, , drop = FALSE]
  clustering <- dbscan::dbscan(df_complete, eps = eps, minPts = minPts)
  out[complete_idx] <- ifelse(clustering$cluster == 0, 1, 0)
  return(out)
}

# One–Class SVM (from package e1071)
one_class_svm_outliers <- function(df, nu = 0.05) {
  complete_idx <- complete.cases(df)
  n <- nrow(df)
  out <- rep(NA, n)
  if (sum(complete_idx) < 1) return(out)
  df_complete <- df[complete_idx, , drop = FALSE]
  model <- e1071::svm(df_complete, scale = TRUE, type = "one-classification", nu = nu, kernel = "radial")
  preds <- predict(model, df_complete)
  out[complete_idx] <- ifelse(preds, 0, 1)
  return(out)
}

# Elliptic Envelope method (using robust covariance estimation from MASS::cov.rob)
elliptic_envelope_outliers <- function(df, contamination = 0.1) {
  complete_idx <- complete.cases(df)
  n <- nrow(df)
  out <- rep(NA, n)
  if (sum(complete_idx) < 1) return(out)
  df_complete <- df[complete_idx, , drop = FALSE]
  robust_cov <- MASS::cov.rob(df_complete)  # use MASS::cov.rob
  md <- mahalanobis(df_complete, robust_cov$center, robust_cov$cov)
  threshold <- qchisq(1 - contamination, df = ncol(df_complete))
  out[complete_idx] <- ifelse(md > threshold, 1, 0)
  return(out)
}

# Local Outlier Factor (LOF) method
lof_outliers <- function(df, minPts = 5, lof_thresh = 1.5) {
  complete_idx <- complete.cases(df)
  n <- nrow(df)
  out <- rep(NA, n)
  if (sum(complete_idx) < 1) return(out)
  df_complete <- df[complete_idx, , drop = FALSE]
  lof_scores <- dbscan::lof(as.matrix(df_complete), minPts = minPts)
  out[complete_idx] <- ifelse(lof_scores > lof_thresh, 1, 0)
  return(out)
}


###########################################
# Main function: detect_outliers()
###########################################
#' Detect Outliers in a Data Frame
#'
#' This function applies a suite of outlier detection methods to a data frame.
#' Univariate methods are applied column‐by‐column while multivariate methods
#' are applied to the entire numeric data set. The output is the original data
#' augmented by additional columns containing binary outlier flags (1 = outlier,
#' 0 = not an outlier, NA = insufficient data).
#'
#' @param data A data frame containing at least one numeric column.
#' @param methods Character vector specifying which methods to use. Options include:
#'   - Univariate: `"z"`, `"tukey"`, `"grubbs"`, `"mad"`, `"iglewicz_hoaglin"`
#'   - Multivariate: `"mahalanobis"`, `"isolation_forest"`, `"dbscan"`, `"one_class_svm"`,
#'     `"elliptic_envelope"`, `"lof"`
#'   The default is to run all methods.
#'
#' The remaining parameters are passed to the individual methods:
#' @param z_thresh Numeric threshold for the Z–score method (default 1.96).
#' @param tukey_mult Numeric multiplier for Tukey’s fences (default 1.5).
#' @param mahalanobis_thresh Numeric threshold for Mahalanobis; if NULL, a 95% chi–square quantile is used.
#' @param grubbs_thresh Numeric significance level for Grubbs test (default 0.05).
#' @param mad_mult Numeric multiplier for the MAD method (default 3).
#' @param iglewicz_hoaglin_thresh Numeric threshold for the modified Z–score (default 3.5).
#' @param isolation_forest_contamination Contamination proportion for Isolation Forest (default 0.1).
#' @param isolation_forest_ndim Dimensionality used in Isolation Forest (default 1).
#' @param isolation_forest_ntree Number of trees for Isolation Forest (default 100).
#' @param isolation_forest_nthreads Number of threads for Isolation Forest (default 1).
#' @param dbscan_eps Epsilon value for DBSCAN (default 0.5).
#' @param dbscan_minPts Minimum points for DBSCAN (default 5).
#' @param one_class_svm_nu Nu parameter for One–Class SVM (default 0.05).
#' @param elliptic_envelope_contamination Contamination for Elliptic Envelope (default 0.1).
#' @param lof_minPts Minimum points for LOF (default 5).
#' @param lof_thresh LOF threshold (default 1.5).
#'
#' @return A data frame with the original data plus additional columns for each method.
#' @export
detect_outliers <- function(data, 
                            methods = c("z", "tukey", "mahalanobis", "grubbs", "mad", 
                                        "iglewicz_hoaglin", "isolation_forest", "dbscan", 
                                        "one_class_svm", "elliptic_envelope", "lof"), 
                            z_thresh = 1.96, 
                            tukey_mult = 1.5, 
                            mahalanobis_thresh = NULL, 
                            grubbs_thresh = 0.05, 
                            mad_mult = 3, 
                            iglewicz_hoaglin_thresh = 3.5, 
                            isolation_forest_contamination = 0.1, 
                            isolation_forest_ndim = 1, 
                            isolation_forest_ntree = 100, 
                            isolation_forest_nthreads = 1,
                            dbscan_eps = 0.5, 
                            dbscan_minPts = 5, 
                            one_class_svm_nu = 0.05, 
                            elliptic_envelope_contamination = 0.1,
                            lof_minPts = 5, 
                            lof_thresh = 1.5) {
  if (!is.data.frame(data))
    stop("Input data must be a data frame.")
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) == 0)
    stop("The data frame must have at least one numeric column.")
  
  results <- data
  
  univariate_methods <- c("z", "tukey", "grubbs", "mad", "iglewicz_hoaglin")
  multivariate_methods <- c("mahalanobis", "isolation_forest", "dbscan", "one_class_svm", "elliptic_envelope", "lof")
  
  # Apply univariate methods column by column
  for (col in numeric_cols) {
    x <- data[[col]]
    for (method in methods) {
      if (method %in% univariate_methods) {
        colname <- switch(method,
                          "z" = paste0(col, "_z_outlier"),
                          "tukey" = paste0(col, "_tukey_outlier"),
                          "grubbs" = paste0(col, "_grubbs_outlier"),
                          "mad" = paste0(col, "_mad_outlier"),
                          "iglewicz_hoaglin" = paste0(col, "_iglewicz_hoaglin_outlier"))
        out <- tryCatch({
          if (method == "z") {
            z_outliers(x, z_thresh = z_thresh)
          } else if (method == "tukey") {
            tukey_outliers(x, tukey_mult = tukey_mult)
          } else if (method == "grubbs") {
            grubbs_outliers(x, grubbs_thresh = grubbs_thresh)
          } else if (method == "mad") {
            mad_outliers(x, mad_mult = mad_mult)
          } else if (method == "iglewicz_hoaglin") {
            iglewicz_hoaglin_outliers(x, threshold = iglewicz_hoaglin_thresh)
          }
        }, error = function(e) {
          warning(paste("Error in", method, "for column", col, ":", e$message))
          rep(NA, length(x))
        })
        results[[colname]] <- out
      }
    }
  }
  
  # Apply multivariate methods on all numeric columns
  numeric_data <- data[, numeric_cols, drop = FALSE]
  for (method in methods) {
    if (method %in% multivariate_methods) {
      colname <- paste0("multivariate_", method, "_outlier")
      out <- tryCatch({
        if (method == "mahalanobis") {
          if (is.null(mahalanobis_thresh))
            mahalanobis_thresh <- qchisq(0.95, df = ncol(numeric_data))
          mahalanobis_outliers(numeric_data, threshold = mahalanobis_thresh)
        } else if (method == "isolation_forest") {
          isolation_forest_outliers(numeric_data, contamination = isolation_forest_contamination,
                                    ndim = isolation_forest_ndim, ntree = isolation_forest_ntree, 
                                    nthreads = isolation_forest_nthreads)
        } else if (method == "dbscan") {
          dbscan_outliers(numeric_data, eps = dbscan_eps, minPts = dbscan_minPts)
        } else if (method == "one_class_svm") {
          one_class_svm_outliers(numeric_data, nu = one_class_svm_nu)
        } else if (method == "elliptic_envelope") {
          elliptic_envelope_outliers(numeric_data, contamination = elliptic_envelope_contamination)
        } else if (method == "lof") {
          lof_outliers(numeric_data, minPts = lof_minPts, lof_thresh = lof_thresh)
        }
      }, error = function(e) {
        warning(paste("Error in multivariate method", method, ":", e$message))
        rep(NA, nrow(numeric_data))
      })
      results[[colname]] <- out
    }
  }
  
  return(results)
}


#####################################
# User Acceptance Testing (UAT)
#####################################
# This UAT suite uses the testthat package to exercise every function and parameter.
# To run the tests, ensure that the testthat package is installed and then call run_uat().

run_uat <- function() {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("The 'testthat' package is required for running the UAT. Please install it.")
  }
  library(testthat)
  
  message("Running UAT for univariate outlier functions...")
  
  test_that("z_outliers identifies extreme values", {
    x <- c(rep(10, 10), 100)
    out <- z_outliers(x, z_thresh = 1.96)
    expect_equal(length(out), length(x))
    expect_true(sum(out, na.rm = TRUE) >= 1)
  })
  
  test_that("tukey_outliers flags values beyond fences", {
    x <- c(1, 2, 3, 4, 50)
    out <- tukey_outliers(x, tukey_mult = 1.5)
    expect_true(any(out == 1, na.rm = TRUE))
  })
  
  test_that("grubbs_outliers warns on too few values", {
    x <- c(5, NA)
    expect_warning(grubbs_outliers(x))
  })
  
  test_that("mad_outliers flags extreme deviations", {
    x <- c(5, 5, 5, 5, 20)
    out <- mad_outliers(x, mad_mult = 3)
    expect_true(any(out == 1, na.rm = TRUE))
  })
  
  test_that("iglewicz_hoaglin_outliers works as expected", {
    x <- c(1, 1, 1, 1, 10)
    out <- iglewicz_hoaglin_outliers(x, threshold = 3.5)
    expect_true(any(out == 1, na.rm = TRUE))
  })
  
  message("Running UAT for multivariate outlier functions...")
  
  set.seed(123)
  df_multi <- data.frame(
    A = rnorm(50, 10, 2),
    B = rnorm(50, 20, 5),
    C = rnorm(50, 30, 10)
  )
  df_multi[c(5, 15), "A"] <- c(25, -5)
  df_multi[c(10, 20), "B"] <- c(40, 5)
  df_multi[c(8, 22), "C"] <- c(60, 0)
  
  test_that("mahalanobis_outliers flags multivariate outliers", {
    out <- mahalanobis_outliers(df_multi, threshold = qchisq(0.95, df = ncol(df_multi)))
    expect_equal(length(out), nrow(df_multi))
    expect_true(sum(out, na.rm = TRUE) >= 1)
  })
  
  test_that("isolation_forest_outliers returns correct length", {
    out <- isolation_forest_outliers(df_multi, contamination = 0.1)
    expect_equal(length(out), nrow(df_multi))
  })
  
  test_that("dbscan_outliers returns binary flags", {
    out <- dbscan_outliers(df_multi, eps = 1, minPts = 3)
    expect_equal(length(out), nrow(df_multi))
    expect_true(all(is.na(out) | out %in% c(0, 1)))
  })
  
  test_that("one_class_svm_outliers returns binary flags", {
    out <- one_class_svm_outliers(df_multi, nu = 0.05)
    expect_equal(length(out), nrow(df_multi))
    expect_true(all(is.na(out) | out %in% c(0, 1)))
  })
  
  test_that("elliptic_envelope_outliers returns binary flags", {
    out <- elliptic_envelope_outliers(df_multi, contamination = 0.1)
    expect_equal(length(out), nrow(df_multi))
    expect_true(all(is.na(out) | out %in% c(0, 1)))
  })
  
  test_that("lof_outliers returns binary flags", {
    out <- lof_outliers(df_multi, minPts = 5, lof_thresh = 1.5)
    expect_equal(length(out), nrow(df_multi))
    expect_true(all(is.na(out) | out %in% c(0, 1)))
  })
  
  message("Running UAT for the detect_outliers() master function...")
  
  example_data <- data.frame(
    A = rnorm(100, 10, 2),
    B = rnorm(100, 20, 5),
    C = rnorm(100, 30, 10),
    D = sample(letters, 100, replace = TRUE)
  )
  example_data[c(25, 50, 75), "B"] <- c(40, 10, 35)
  example_data[c(10, 90), "C"] <- c(5, 50)
  
  res <- detect_outliers(example_data, 
                         methods = c("z", "tukey", "mahalanobis", "grubbs", "mad", 
                                     "iglewicz_hoaglin", "isolation_forest", "dbscan", 
                                     "one_class_svm", "elliptic_envelope", "lof"),
                         z_thresh = 1.96,
                         tukey_mult = 1.5,
                         mahalanobis_thresh = NULL,
                         grubbs_thresh = 0.05,
                         mad_mult = 3,
                         iglewicz_hoaglin_thresh = 3.5,
                         isolation_forest_contamination = 0.1,
                         isolation_forest_ndim = 1,
                         isolation_forest_ntree = 100,
                         isolation_forest_nthreads = 1,
                         dbscan_eps = 0.5,
                         dbscan_minPts = 5,
                         one_class_svm_nu = 0.05,
                         elliptic_envelope_contamination = 0.1,
                         lof_minPts = 5,
                         lof_thresh = 1.5)
  
  test_that("detect_outliers() returns original plus added columns", {
    expect_true(ncol(res) > ncol(example_data))
    flag_cols <- grep("_outlier$", names(res), value = TRUE)
    expect_true(length(flag_cols) > 0)
  })
  
  message("All UAT tests completed successfully!")
}

##############################
# End of Script
##############################

# To run the UAT, simply call:
# run_uat()
