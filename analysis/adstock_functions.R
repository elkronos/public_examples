#############################################
# Adstock Transformation Module
#############################################

.assert_integer_scalar <- function(x, name, min = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || (x %% 1) != 0) {
    stop(sprintf("`%s` must be an integer scalar.", name), call. = FALSE)
  }
  xi <- as.integer(x)
  if (xi < min) {
    stop(sprintf("`%s` must be >= %d.", name, min), call. = FALSE)
  }
  xi
}

.assert_probability_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    stop(sprintf("`%s` must be a numeric scalar in [0, 1].", name), call. = FALSE)
  }
  as.numeric(x)
}

.assert_numeric_vector <- function(x, name) {
  if (!is.numeric(x) || length(x) < 1L || is.matrix(x) || is.data.frame(x)) {
    stop(sprintf("`%s` must be a non-empty numeric vector.", name), call. = FALSE)
  }
  x
}

.align_metric_inputs <- function(actual, predicted, na_rm = TRUE) {
  if (length(actual) != length(predicted)) {
    stop("Lengths of `actual` and `predicted` must match.", call. = FALSE)
  }
  if (!is.numeric(actual) || !is.numeric(predicted)) {
    stop("`actual` and `predicted` must be numeric.", call. = FALSE)
  }
  if (!na_rm) {
    return(list(actual = actual, predicted = predicted))
  }
  ok <- is.finite(actual) & is.finite(predicted)
  list(actual = actual[ok], predicted = predicted[ok])
}

#' Compute Adstock Weights
#'
#' @param lag Positive integer representing the lag period.
#' @param decay Numeric value between 0 and 1 representing the decay rate.
#' @return A normalized numeric vector of weights.
compute_adstock_weights <- function(lag, decay) {
  lag <- .assert_integer_scalar(lag, "lag", min = 1L)
  decay <- .assert_probability_scalar(decay, "decay")
  
  if (lag == 1L) {
    return(1)
  }
  exponents <- 0:(lag - 1L)
  w <- decay^exponents
  w / sum(w)
}

#' Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lag Positive integer for the duration over which the adstock effect is spread.
#' @param decay Numeric decay rate between 0 and 1.
#' @param fill_na Logical; if TRUE returns NA for the first lag-1 positions,
#'   otherwise returns a full-length vector with leading history treated as zeros.
#' @param na_action Character; "propagate" leaves NA behavior as-is, "zero" treats NA values as zero.
#'
#' @return A numeric vector of adstock-transformed values.
adstock_transform <- function(x, lag, decay, fill_na = TRUE, na_action = c("propagate", "zero")) {
  x <- .assert_numeric_vector(x, "x")
  lag <- .assert_integer_scalar(lag, "lag", min = 1L)
  decay <- .assert_probability_scalar(decay, "decay")
  na_action <- match.arg(na_action)
  
  xx <- if (na_action == "zero" && anyNA(x)) {
    x2 <- x
    x2[is.na(x2)] <- 0
    x2
  } else {
    x
  }
  
  w <- compute_adstock_weights(lag, decay)
  n <- length(xx)
  
  if (fill_na) {
    y <- stats::filter(xx, filter = w, sides = 1L, method = "convolution")
    return(as.numeric(y))
  }
  
  x_pad <- c(rep(0, lag - 1L), xx)
  y_pad <- stats::filter(x_pad, filter = w, sides = 1L, method = "convolution")
  as.numeric(y_pad[lag:(lag + n - 1L)])
}

#' Calculate Sum of Squared Errors (SSE)
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values.
#' @param na_rm Logical; if TRUE removes non-finite pairs.
#' @return The SSE between actual and predicted.
calculate_sse <- function(actual, predicted, na_rm = TRUE) {
  v <- .align_metric_inputs(actual, predicted, na_rm = na_rm)
  sum((v$actual - v$predicted)^2)
}

#' Calculate Mean Absolute Error (MAE)
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values.
#' @param na_rm Logical; if TRUE removes non-finite pairs.
#' @return The MAE between actual and predicted.
calculate_mae <- function(actual, predicted, na_rm = TRUE) {
  v <- .align_metric_inputs(actual, predicted, na_rm = na_rm)
  mean(abs(v$actual - v$predicted))
}

.fit_trend_fast <- function(y) {
  n <- length(y)
  if (n < 2L) {
    return(list(intercept = if (n == 1L) as.numeric(y[1L]) else 0, slope = 0, n = n))
  }
  
  t_sum <- n * (n + 1) / 2
  tt_sum <- n * (n + 1) * (2 * n + 1) / 6
  
  y_sum <- sum(y)
  ty_sum <- sum(y * seq_len(n))
  
  denom <- (n * tt_sum - t_sum^2)
  if (!is.finite(denom) || denom == 0) {
    return(list(intercept = y_sum / n, slope = 0, n = n))
  }
  
  slope <- (n * ty_sum - t_sum * y_sum) / denom
  intercept <- (y_sum - slope * t_sum) / n
  list(intercept = intercept, slope = slope, n = n)
}

.predict_trend_forward <- function(model, n_ahead) {
  if (n_ahead < 1L) {
    return(numeric(0))
  }
  t_test <- (model$n + 1L):(model$n + n_ahead)
  model$intercept + model$slope * t_test
}

#' Train a Linear Trend Model and Predict
#'
#' @param x_train Numeric vector for training (original data).
#' @param x_test Numeric vector for testing (original data).
#' @param adstocked_train Numeric vector of adstock-transformed training data.
#'
#' @return A numeric vector of predictions for x_test.
train_and_predict <- function(x_train, x_test, adstocked_train) {
  x_train <- .assert_numeric_vector(x_train, "x_train")
  x_test <- .assert_numeric_vector(x_test, "x_test")
  adstocked_train <- .assert_numeric_vector(adstocked_train, "adstocked_train")
  
  if (length(x_train) != length(adstocked_train)) {
    stop("Length of `x_train` and `adstocked_train` must be equal.", call. = FALSE)
  }
  
  model <- .fit_trend_fast(adstocked_train)
  .predict_trend_forward(model, length(x_test))
}

.forward_folds <- function(n, k, min_train_size) {
  k <- .assert_integer_scalar(k, "k", min = 1L)
  min_train_size <- .assert_integer_scalar(min_train_size, "min_train_size", min = 1L)
  
  if (min_train_size >= n) {
    stop("`min_train_size` must be less than length of `x`.", call. = FALSE)
  }
  
  edges <- unique(as.integer(floor(seq(min_train_size, n, length.out = k + 1L))))
  edges <- edges[edges >= min_train_size & edges <= n]
  if (length(edges) < 2L) {
    stop("Insufficient fold boundaries for the given inputs.", call. = FALSE)
  }
  
  folds <- vector("list", length(edges) - 1L)
  for (i in seq_len(length(edges) - 1L)) {
    train_end <- edges[i]
    test_start <- train_end + 1L
    test_end <- edges[i + 1L]
    if (test_start <= test_end) {
      folds[[i]] <- c(train_end = train_end, test_start = test_start, test_end = test_end)
    } else {
      folds[[i]] <- NULL
    }
  }
  Filter(Negate(is.null), folds)
}

#' K-fold Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lag Positive integer for the adstock transformation lag.
#' @param decay Numeric decay rate between 0 and 1.
#' @param k Integer number of folds (default is 10).
#' @param min_train_size Minimum training size used for the first fold.
#' @param metric Character; "sse" or "mae".
#'
#' @return Mean error across folds.
k_fold_cv <- function(x, lag, decay, k = 10, min_train_size = NULL, metric = c("sse", "mae")) {
  x <- .assert_numeric_vector(x, "x")
  lag <- .assert_integer_scalar(lag, "lag", min = 1L)
  decay <- .assert_probability_scalar(decay, "decay")
  k <- .assert_integer_scalar(k, "k", min = 1L)
  metric <- match.arg(metric)
  
  n <- length(x)
  if (is.null(min_train_size)) {
    min_train_size <- max(2L, lag)
  }
  min_train_size <- .assert_integer_scalar(min_train_size, "min_train_size", min = 2L)
  
  folds <- .forward_folds(n, k, min_train_size)
  adstocked_full <- adstock_transform(x, lag, decay, fill_na = FALSE)
  
  err <- numeric(length(folds))
  for (i in seq_along(folds)) {
    f <- folds[[i]]
    train_end <- f[["train_end"]]
    test_idx <- f[["test_start"]]:f[["test_end"]]
    
    y_train <- adstocked_full[1L:train_end]
    x_test <- x[test_idx]
    
    pred <- train_and_predict(x_train = x[1L:train_end], x_test = x_test, adstocked_train = y_train)
    
    err[i] <- if (metric == "sse") {
      calculate_sse(x_test, pred)
    } else {
      calculate_mae(x_test, pred)
    }
  }
  
  mean(err)
}

#' Walk Forward Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lag Positive integer for the adstock transformation lag.
#' @param decay Numeric decay rate between 0 and 1.
#' @param min_train_size Minimum training size (default is 50% of x length).
#' @param step Integer step size for the moving window (default is 1).
#' @param metric Character; "mae" or "sse".
#'
#' @return Mean error over all iterations.
walk_forward_cv <- function(x, lag, decay, min_train_size = 0.5 * length(x), step = 1, metric = c("mae", "sse")) {
  x <- .assert_numeric_vector(x, "x")
  lag <- .assert_integer_scalar(lag, "lag", min = 1L)
  decay <- .assert_probability_scalar(decay, "decay")
  step <- .assert_integer_scalar(step, "step", min = 1L)
  metric <- match.arg(metric)
  
  n <- length(x)
  start_index <- as.integer(round(min_train_size))
  if (start_index < 2L || start_index >= n) {
    stop("`min_train_size` must be in [2, length(x)-1].", call. = FALSE)
  }
  
  adstocked_full <- adstock_transform(x, lag, decay, fill_na = FALSE)
  
  y <- adstocked_full
  cum_y <- cumsum(y)
  cum_ty <- cumsum(y * seq_len(n))
  
  err <- numeric(0)
  idx <- start_index
  
  while ((idx + step) <= n) {
    m <- idx
    
    t_sum <- m * (m + 1) / 2
    tt_sum <- m * (m + 1) * (2 * m + 1) / 6
    y_sum <- cum_y[m]
    ty_sum <- cum_ty[m]
    
    denom <- (m * tt_sum - t_sum^2)
    slope <- if (!is.finite(denom) || denom == 0) 0 else (m * ty_sum - t_sum * y_sum) / denom
    intercept <- (y_sum - slope * t_sum) / m
    
    test_idx <- (m + 1L):(m + step)
    t_test <- test_idx
    pred <- intercept + slope * t_test
    x_test <- x[test_idx]
    
    err <- c(err, if (metric == "sse") calculate_sse(x_test, pred) else calculate_mae(x_test, pred))
    idx <- idx + step
  }
  
  mean(err)
}

.grid_search_impl <- function(x, lags, decays, cores, eval_one) {
  lags <- as.integer(lags)
  if (any(is.na(lags)) || any(lags < 1L)) {
    stop("`lags` must be positive integers.", call. = FALSE)
  }
  if (!is.numeric(decays) || any(is.na(decays)) || any(decays < 0) || any(decays > 1)) {
    stop("`decays` must be numeric values in [0, 1].", call. = FALSE)
  }
  
  cores <- .assert_integer_scalar(cores, "cores", min = 1L)
  max_cores <- parallel::detectCores(logical = FALSE)
  if (is.finite(max_cores)) cores <- min(cores, max_cores)
  
  grid <- expand.grid(lag = lags, decay = decays, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  
  if (cores == 1L) {
    errors <- vapply(seq_len(nrow(grid)), function(i) eval_one(grid$lag[i], grid$decay[i]), numeric(1))
  } else {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    parallel::clusterExport(
      cl,
      varlist = c(
        "x",
        "eval_one",
        "compute_adstock_weights",
        "adstock_transform",
        "calculate_sse",
        "calculate_mae",
        "train_and_predict",
        "k_fold_cv",
        "walk_forward_cv",
        ".assert_integer_scalar",
        ".assert_probability_scalar",
        ".assert_numeric_vector",
        ".align_metric_inputs",
        ".fit_trend_fast",
        ".predict_trend_forward",
        ".forward_folds"
      ),
      envir = environment()
    )
    
    errors <- unlist(parallel::parLapply(cl, seq_len(nrow(grid)), function(i) {
      eval_one(grid$lag[i], grid$decay[i])
    }), use.names = FALSE)
  }
  
  err_mat <- matrix(errors, nrow = length(lags), ncol = length(decays))
  dimnames(err_mat) <- list(lag = as.character(lags), decay = as.character(decays))
  
  best_flat <- which.min(err_mat)
  best_idx <- arrayInd(best_flat, dim(err_mat))
  
  list(
    optimal_lag = lags[best_idx[1]],
    optimal_decay = decays[best_idx[2]],
    cv_errors = err_mat
  )
}

#' Grid Search with K-fold Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lags Vector of candidate lag values.
#' @param decays Vector of candidate decay values.
#' @param cores Number of CPU cores to use (default is 1).
#' @param k Integer number of folds (default is 10).
#' @param min_train_size Minimum training size used for the first fold.
#' @param metric Character; "sse" or "mae".
#'
#' @return A list with optimal lag, optimal decay, and the CV error matrix.
grid_search_cv <- function(x,
                           lags = seq(1, 20, by = 1),
                           decays = seq(0.1, 0.9, by = 0.05),
                           cores = 1,
                           k = 10,
                           min_train_size = NULL,
                           metric = c("sse", "mae")) {
  x <- .assert_numeric_vector(x, "x")
  k <- .assert_integer_scalar(k, "k", min = 1L)
  metric <- match.arg(metric)
  
  eval_one <- function(lag, decay) {
    k_fold_cv(x, lag = lag, decay = decay, k = k, min_train_size = min_train_size, metric = metric)
  }
  
  .grid_search_impl(x, lags, decays, cores, eval_one)
}

#' Grid Search with Walk Forward Cross-Validation for Adstock Transformation
#'
#' @param x Numeric vector of time-series data.
#' @param lags Vector of candidate lag values.
#' @param decays Vector of candidate decay values.
#' @param cores Number of CPU cores to use (default is 1).
#' @param min_train_size Minimum training size (default is 50% of x length).
#' @param step Step size for the rolling window (default is 1).
#' @param metric Character; "mae" or "sse".
#'
#' @return A list with optimal lag, optimal decay, and the CV error matrix.
grid_search_walk_forward_cv <- function(x,
                                        lags = seq(1, 20, by = 1),
                                        decays = seq(0.1, 0.9, by = 0.05),
                                        cores = 1,
                                        min_train_size = 0.5 * length(x),
                                        step = 1,
                                        metric = c("mae", "sse")) {
  x <- .assert_numeric_vector(x, "x")
  step <- .assert_integer_scalar(step, "step", min = 1L)
  metric <- match.arg(metric)
  
  eval_one <- function(lag, decay) {
    walk_forward_cv(x, lag = lag, decay = decay, min_train_size = min_train_size, step = step, metric = metric)
  }
  
  .grid_search_impl(x, lags, decays, cores, eval_one)
}

#############################################
# End of Adstock Transformation Module
#############################################


#############################################
# UAT Test Code using testthat
#############################################

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(ggplot2))

context("Adstock Transformation Functions")

test_that("compute_adstock_weights returns correct normalized weights", {
  lag <- 3
  decay <- 0.5
  weights <- compute_adstock_weights(lag, decay)
  expected <- 0.5^(0:(lag - 1))
  expected <- expected / sum(expected)
  expect_equal(weights, expected)
})

test_that("adstock_transform works correctly with fill_na = TRUE", {
  x <- 1:10
  lag <- 3
  decay <- 0.5
  transformed <- adstock_transform(x, lag, decay, fill_na = TRUE)
  
  expect_true(all(is.na(transformed[1:(lag - 1)])))
  expect_equal(length(transformed), length(x))
})

test_that("adstock_transform works correctly with fill_na = FALSE", {
  x <- 1:10
  lag <- 3
  decay <- 0.5
  transformed <- adstock_transform(x, lag, decay, fill_na = FALSE)
  
  expect_false(any(is.na(transformed)))
  expect_equal(length(transformed), length(x))
})

context("Error Metric Functions")

test_that("calculate_sse computes the correct value", {
  actual <- c(1, 2, 3)
  predicted <- c(1, 3, 2)
  sse_val <- calculate_sse(actual, predicted)
  expected <- sum((actual - predicted)^2)
  expect_equal(sse_val, expected)
})

test_that("calculate_mae computes the correct value", {
  actual <- c(1, 2, 3)
  predicted <- c(1, 3, 2)
  mae_val <- calculate_mae(actual, predicted)
  expected <- mean(abs(actual - predicted))
  expect_equal(mae_val, expected)
})

context("Model Training and Prediction")

test_that("train_and_predict returns predictions of correct length", {
  x_train <- 1:20
  x_test <- 21:30
  lag <- 3
  decay <- 0.5
  adstocked_train <- adstock_transform(x_train, lag, decay, fill_na = FALSE)
  predictions <- train_and_predict(x_train, x_test, adstocked_train)
  
  expect_equal(length(predictions), length(x_test))
})

context("Cross-Validation Functions")

test_that("k_fold_cv returns a numeric error value", {
  x <- 1:50
  lag <- 3
  decay <- 0.5
  error <- k_fold_cv(x, lag, decay, k = 5)
  expect_true(is.numeric(error))
  expect_length(error, 1)
})

test_that("walk_forward_cv returns a numeric error value", {
  x <- 1:50
  lag <- 3
  decay <- 0.5
  error <- walk_forward_cv(x, lag, decay, min_train_size = 20, step = 5)
  expect_true(is.numeric(error))
  expect_length(error, 1)
})

context("Grid Search Functions")

test_that("grid_search_cv returns correct list structure", {
  x <- 1:50
  lags <- 2:4
  decays <- seq(0.1, 0.5, by = 0.2)
  result <- grid_search_cv(x, lags = lags, decays = decays, cores = 1, k = 3)
  
  expect_true(is.list(result))
  expect_true("optimal_lag" %in% names(result))
  expect_true("optimal_decay" %in% names(result))
  expect_true("cv_errors" %in% names(result))
  expect_equal(dim(result$cv_errors), c(length(lags), length(decays)))
})

test_that("grid_search_walk_forward_cv returns correct list structure", {
  x <- 1:50
  lags <- 2:4
  decays <- seq(0.1, 0.5, by = 0.2)
  result <- grid_search_walk_forward_cv(
    x, lags = lags, decays = decays, cores = 1, min_train_size = 20, step = 5
  )
  
  expect_true(is.list(result))
  expect_true("optimal_lag" %in% names(result))
  expect_true("optimal_decay" %in% names(result))
  expect_true("cv_errors" %in% names(result))
  expect_equal(dim(result$cv_errors), c(length(lags), length(decays)))
})

context("Demonstration: Plotting and Predictions")

test_that("The plotting example produces a valid ggplot object", {
  set.seed(123)
  x <- 10 * sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, 0, 3)
  
  results_cv <- grid_search_cv(x, cores = 1)
  
  adstocked_x <- adstock_transform(x, results_cv$optimal_lag, results_cv$optimal_decay)
  adstocked_x[is.na(adstocked_x)] <- x[is.na(adstocked_x)]
  
  offset <- 1
  adstocked_x_offset <- adstocked_x + offset
  
  df <- data.frame(
    Time = 1:length(x),
    Original = x,
    Transformed = adstocked_x_offset
  )
  
  p <- ggplot(df, aes(Time)) +
    geom_line(aes(y = Original), linewidth = 1.2) +
    geom_line(aes(y = Transformed), linewidth = 1.2) +
    labs(
      x = "Time",
      y = "Value",
      title = "Original vs. Adstock Transformed Time Series"
    ) +
    theme_minimal()
  
  expect_true(inherits(p, "ggplot"))
})
