# Analysis utilities and unit acceptance tests
# -------------------------------------------

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing package(s): ", paste(missing, collapse = ", "),
      ". Install them to use these functions."
    )
  }
  invisible(TRUE)
}

.assert_data_frame <- function(x, arg = "data") {
  if (!is.data.frame(x)) stop(arg, " must be a data frame.")
  invisible(TRUE)
}

.assert_character_vec <- function(x, arg) {
  if (!is.character(x)) stop(arg, " must be a character vector.")
  if (length(x) == 0) stop(arg, " must have at least one element.")
  invisible(TRUE)
}

.assert_single_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(arg, " must be a single, non-empty character string.")
  }
  invisible(TRUE)
}

.assert_vars_exist <- function(data, vars) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      "The following variable(s) are not in the data: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  invisible(TRUE)
}

.safe_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(as.character(x)))
}

.safe_divide <- function(num, den) {
  out <- rep(NA_real_, length(num))
  ok <- den != 0 & !is.na(den)
  out[ok] <- num[ok] / den[ok]
  out
}

.extract_predictions <- function(model, newdata) {
  pred <- stats::predict(model, newdata = newdata)
  if (is.list(pred) && "class" %in% names(pred)) return(pred$class)
  pred
}

# ------------------------------------------------------------------------------
# Plot histograms
# ------------------------------------------------------------------------------

#' Plot Histograms of Specified Variables
#'
#' Creates a ggplot2 histogram for each requested variable.
#'
#' @param data A data frame.
#' @param variables A character vector of column names.
#' @param binwidth Numeric value used by geom_histogram().
#'
#' @return Invisibly returns a named list of ggplot objects.
#' @export
plot_histograms <- function(data, variables, binwidth = 0.5) {
  .require_pkgs(c("ggplot2", "rlang"))
  .assert_data_frame(data, "data")
  .assert_character_vec(variables, "variables")
  .assert_vars_exist(data, variables)
  
  plots <- vector("list", length(variables))
  names(plots) <- variables
  
  for (i in seq_along(variables)) {
    var <- variables[[i]]
    x <- data[[var]]
    
    if (!is.numeric(x)) {
      warning(
        sprintf("Variable '%s' is not numeric. Attempting to coerce to numeric.", var),
        call. = FALSE
      )
      x <- .safe_numeric(x)
      if (all(is.na(x))) {
        warning(
          sprintf("Variable '%s' could not be coerced to numeric. Skipping plot.", var),
          call. = FALSE
        )
        next
      }
    }
    
    df <- data.frame(.x = x)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .x)) +
      ggplot2::geom_histogram(binwidth = binwidth, fill = "lightblue", color = "black") +
      ggplot2::ggtitle(paste("Histogram of", var)) +
      ggplot2::theme_minimal()
    
    print(p)
    plots[[i]] <- p
  }
  
  plots <- plots[!vapply(plots, is.null, logical(1))]
  invisible(plots)
}

# ------------------------------------------------------------------------------
# Check normality within groups
# ------------------------------------------------------------------------------

#' Check Normality of Variables within Groups
#'
#' Runs Shapiro-Wilk normality tests per group and variable.
#'
#' @param data A data frame.
#' @param variables A character vector of column names.
#' @param grouping_var A single character string naming the grouping column.
#'
#' @return Invisibly returns a data frame with Group, Variable, W, p_value, and n.
#' @export
check_normality <- function(data, variables, grouping_var) {
  .assert_data_frame(data, "data")
  .assert_character_vec(variables, "variables")
  .assert_single_string(grouping_var, "grouping_var")
  .assert_vars_exist(data, c(variables, grouping_var))
  
  grp <- data[[grouping_var]]
  idx_by_group <- split(seq_len(nrow(data)), grp, drop = TRUE)
  
  rows <- vector("list", length(idx_by_group) * length(variables))
  k <- 0L
  
  for (g in names(idx_by_group)) {
    idx <- idx_by_group[[g]]
    for (var in variables) {
      x <- data[[var]][idx]
      
      if (!is.numeric(x)) {
        warning(
          sprintf("Variable '%s' in group '%s' is not numeric. Skipping normality test.", var, g),
          call. = FALSE
        )
        next
      }
      
      x <- x[!is.na(x)]
      n <- length(x)
      
      if (n < 3L) {
        warning(
          sprintf(
            "Not enough observations (n = %d) in group '%s' for variable '%s' to perform the Shapiro–Wilk test. Skipping.",
            n, g, var
          ),
          call. = FALSE
        )
        next
      }
      
      if (n > 5000L) {
        x <- sample(x, 5000L)
        n <- length(x)
      }
      
      test_result <- tryCatch(
        stats::shapiro.test(x),
        error = function(e) e
      )
      
      if (inherits(test_result, "error")) {
        warning(
          sprintf("Shapiro–Wilk test failed for group '%s', variable '%s'. Skipping.", g, var),
          call. = FALSE
        )
        next
      }
      
      k <- k + 1L
      rows[[k]] <- data.frame(
        Group = g,
        Variable = var,
        W = unname(test_result$statistic),
        p_value = unname(test_result$p.value),
        n = n,
        stringsAsFactors = FALSE
      )
      
      cat(sprintf(
        "Group: %s, Variable: %s, n: %d, W: %.4f, p-value: %.4g\n",
        g, var, n, unname(test_result$statistic), unname(test_result$p.value)
      ))
    }
  }
  
  rows <- rows[seq_len(k)]
  out <- if (k == 0L) {
    data.frame(Group = character(), Variable = character(), W = numeric(), p_value = numeric(), n = integer())
  } else {
    do.call(rbind, rows)
  }
  
  invisible(out)
}

# ------------------------------------------------------------------------------
# Check covariance homogeneity
# ------------------------------------------------------------------------------

#' Check for Homogeneity of Variance-Covariance
#'
#' Performs Box's M test across groups.
#'
#' @param data A data frame.
#' @param variables A character vector of numeric column names.
#' @param grouping_var A single character string naming the grouping column.
#'
#' @return Invisibly returns the heplots::boxM result.
#' @export
check_covariance_homogeneity <- function(data, variables, grouping_var) {
  .require_pkgs("heplots")
  .assert_data_frame(data, "data")
  .assert_character_vec(variables, "variables")
  .assert_single_string(grouping_var, "grouping_var")
  .assert_vars_exist(data, c(variables, grouping_var))
  
  non_numeric <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("Non-numeric variable(s): ", paste(non_numeric, collapse = ", "))
  }
  
  cc <- stats::complete.cases(data[, c(variables, grouping_var), drop = FALSE])
  d <- data[cc, , drop = FALSE]
  if (nrow(d) == 0L) stop("No complete cases available for the requested columns.")
  
  g <- d[[grouping_var]]
  if (length(unique(g)) < 2L) stop("At least two groups are required for Box's M test.")
  
  x <- as.matrix(d[, variables, drop = FALSE])
  result <- heplots::boxM(x, group = g)
  
  print(result)
  invisible(result)
}

# ------------------------------------------------------------------------------
# Pair plots
# ------------------------------------------------------------------------------

#' Plot Pair Plots
#'
#' Creates a GGally scatterplot matrix colored by a categorical variable.
#'
#' @param data A data frame.
#' @param variables A character vector of column names used for the matrix.
#' @param color_var A single character string naming the color column.
#'
#' @return Invisibly returns the ggpairs object.
#' @export
plot_pairplots <- function(data, variables, color_var) {
  .require_pkgs(c("GGally", "ggplot2", "rlang"))
  .assert_data_frame(data, "data")
  .assert_character_vec(variables, "variables")
  .assert_single_string(color_var, "color_var")
  .assert_vars_exist(data, c(variables, color_var))
  
  p <- suppressMessages(
    GGally::ggpairs(
      data,
      columns = variables,
      mapping = ggplot2::aes(color = !!rlang::sym(color_var))
    )
  ) + ggplot2::theme_minimal()
  
  print(p)
  invisible(p)
}

# ------------------------------------------------------------------------------
# Model evaluation
# ------------------------------------------------------------------------------

#' Evaluate Classification Model Performance
#'
#' Computes confusion matrix, accuracy, and per-class precision/recall/F1.
#'
#' @param model A fitted classification model compatible with predict().
#' @param test_data A data frame containing evaluation rows.
#' @param true_col A single character string naming the true label column.
#'
#' @return Invisibly returns a list with confusion_mat, accuracy, precision, recall, F1, and macro metrics.
#' @export
evaluate_model <- function(model, test_data, true_col) {
  .assert_data_frame(test_data, "test_data")
  .assert_single_string(true_col, "true_col")
  .assert_vars_exist(test_data, true_col)
  
  predictions <- .extract_predictions(model, test_data)
  actual <- test_data[[true_col]]
  
  if (length(predictions) != nrow(test_data)) {
    stop("Prediction length does not match the number of rows in test_data.")
  }
  
  drop_na <- is.na(predictions) | is.na(actual)
  if (any(drop_na)) {
    warning(
      sprintf("Dropping %d row(s) with missing predicted or actual labels.", sum(drop_na)),
      call. = FALSE
    )
    predictions <- predictions[!drop_na]
    actual <- actual[!drop_na]
  }
  
  predictions <- as.character(predictions)
  actual <- as.character(actual)
  
  lvls <- sort(unique(c(actual, predictions)))
  predictions_f <- factor(predictions, levels = lvls)
  actual_f <- factor(actual, levels = lvls)
  
  confusion_mat <- table(Predicted = predictions_f, Actual = actual_f)
  
  cat("\nConfusion Matrix:\n")
  print(confusion_mat)
  
  total <- sum(confusion_mat)
  accuracy <- if (total == 0) NA_real_ else sum(diag(confusion_mat)) / total
  
  tp <- diag(confusion_mat)
  fp <- rowSums(confusion_mat) - tp
  fn <- colSums(confusion_mat) - tp
  
  precision <- .safe_divide(tp, tp + fp)
  recall <- .safe_divide(tp, tp + fn)
  F1 <- .safe_divide(2 * precision * recall, precision + recall)
  
  names(precision) <- lvls
  names(recall) <- lvls
  names(F1) <- lvls
  
  macro_precision <- mean(precision, na.rm = TRUE)
  macro_recall <- mean(recall, na.rm = TRUE)
  macro_F1 <- mean(F1, na.rm = TRUE)
  
  cat(sprintf("Overall Accuracy: %.4f\n", accuracy))
  cat("Precision by class:\n"); print(precision)
  cat("Recall by class:\n"); print(recall)
  cat("F1 Score by class:\n"); print(F1)
  
  invisible(list(
    confusion_mat = confusion_mat,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    F1 = F1,
    macro_precision = macro_precision,
    macro_recall = macro_recall,
    macro_F1 = macro_F1
  ))
}

# ------------------------------------------------------------------------------
# LDA visualization
# ------------------------------------------------------------------------------

#' Visualize Linear Discriminant Analysis (LDA) Results
#'
#' Generates a density plot for a requested LD column.
#'
#' @param model A MASS::lda object or a caret train object with an LDA finalModel.
#' @param test_data A data frame containing rows for projection.
#' @param lda_col A single character string such as "LD1".
#' @param color_var A single character string naming the fill column created from predicted classes.
#'
#' @return Invisibly returns the ggplot object.
#' @export
plot_lda <- function(model, test_data, lda_col, color_var) {
  .require_pkgs(c("ggplot2", "rlang"))
  .assert_data_frame(test_data, "test_data")
  .assert_single_string(lda_col, "lda_col")
  .assert_single_string(color_var, "color_var")
  
  ld_scores <- NULL
  predicted_class <- NULL
  
  if (inherits(model, "train")) {
    if (is.null(model$finalModel)) stop("train object does not contain finalModel.")
    if (!inherits(model$finalModel, "lda")) stop("train finalModel is not an lda model.")
    
    required_cols <- colnames(model$finalModel$means)
    .assert_vars_exist(test_data, required_cols)
    
    pred_out <- stats::predict(model$finalModel, newdata = test_data[, required_cols, drop = FALSE])
    ld_scores <- as.data.frame(pred_out$x)
    predicted_class <- stats::predict(model, newdata = test_data)
  } else if (inherits(model, "lda")) {
    pred_out <- stats::predict(model, newdata = test_data)
    ld_scores <- as.data.frame(pred_out$x)
    predicted_class <- pred_out$class
  } else {
    stop("Model type not supported. Use an 'lda' model or a caret 'train' object with an LDA finalModel.")
  }
  
  colnames(ld_scores) <- paste0("LD", seq_len(ncol(ld_scores)))
  if (!(lda_col %in% names(ld_scores))) {
    stop(sprintf("Variable '%s' not found in the LDA results.", lda_col))
  }
  
  ld_scores[[color_var]] <- as.character(predicted_class)
  
  keep <- !is.na(ld_scores[[lda_col]]) & !is.na(ld_scores[[color_var]])
  ld_scores <- ld_scores[keep, , drop = FALSE]
  
  group_counts <- table(ld_scores[[color_var]])
  drop_groups <- names(group_counts[group_counts < 2L])
  if (length(drop_groups) > 0) {
    warning(
      sprintf(
        "%d group(s) have fewer than two data points and will be dropped from the plot. Groups: %s",
        length(drop_groups), paste(drop_groups, collapse = ", ")
      ),
      call. = FALSE
    )
    ld_scores <- ld_scores[!(ld_scores[[color_var]] %in% drop_groups), , drop = FALSE]
  }
  
  p <- ggplot2::ggplot(
    ld_scores,
    ggplot2::aes(
      x = !!rlang::sym(lda_col),
      fill = !!rlang::sym(color_var)
    )
  ) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(
      title = "Density plot of the Linear Discriminant",
      x = lda_col,
      fill = color_var
    ) +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = color_var))
  
  print(p)
  invisible(p)
}

# ------------------------------------------------------------------------------
# Unit acceptance tests
# ------------------------------------------------------------------------------

.require_pkgs("testthat")

testthat::test_that("plot_histograms works with numeric variables", {
  test_data <- data.frame(a = rnorm(100), b = rnorm(100))
  testthat::expect_silent(plot_histograms(test_data, c("a", "b")))
})

testthat::test_that("plot_histograms warns with non-numeric variables", {
  test_data <- data.frame(a = rnorm(100), b = as.character(rnorm(100)))
  testthat::expect_warning(plot_histograms(test_data, c("b")), regexp = "Attempting to coerce")
})

testthat::test_that("check_normality returns results and warns with insufficient data", {
  test_data <- data.frame(
    value = c(rnorm(10), rnorm(2)),
    group = rep(c("G1", "G2"), times = c(10, 2))
  )
  result <- check_normality(test_data, "value", "group")
  testthat::expect_true("G1" %in% result$Group)
  testthat::expect_false("G2" %in% result$Group)
})

testthat::test_that("check_normality errors if grouping_var not found", {
  test_data <- data.frame(a = rnorm(100))
  testthat::expect_error(check_normality(test_data, "a", "nonexistent"))
})

testthat::test_that("check_covariance_homogeneity works with valid input", {
  test_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    group = rep(c("A", "B"), 50)
  )
  result <- check_covariance_homogeneity(test_data, c("x", "y"), "group")
  testthat::expect_true("statistic" %in% names(result))
})

testthat::test_that("check_covariance_homogeneity errors for non-numeric variable", {
  test_data <- data.frame(
    x = rnorm(100),
    y = letters[1:100],
    group = rep(c("A", "B"), 50)
  )
  testthat::expect_error(check_covariance_homogeneity(test_data, c("x", "y"), "group"))
})

testthat::test_that("plot_pairplots works with valid input", {
  test_data <- data.frame(
    a = rnorm(50),
    b = rnorm(50),
    c = rnorm(50),
    group = sample(letters[1:3], 50, replace = TRUE)
  )
  testthat::expect_silent(plot_pairplots(test_data, c("a", "b", "c"), "group"))
})

testthat::test_that("plot_pairplots errors if color_var missing", {
  test_data <- data.frame(a = rnorm(50), b = rnorm(50))
  testthat::expect_error(plot_pairplots(test_data, c("a", "b"), "group"))
})

testthat::test_that("evaluate_model works with a simple LDA model", {
  .require_pkgs("MASS")
  data(iris)
  set.seed(123)
  train_idx <- sample(seq_len(nrow(iris)), size = floor(0.7 * nrow(iris)))
  train_data <- iris[train_idx, ]
  test_data <- iris[-train_idx, ]
  
  model <- MASS::lda(Species ~ ., data = train_data)
  result <- evaluate_model(model, test_data, "Species")
  testthat::expect_true(is.list(result))
  testthat::expect_true("confusion_mat" %in% names(result))
})

testthat::test_that("evaluate_model errors when true_col not in test_data", {
  .require_pkgs("MASS")
  data(iris)
  model <- MASS::lda(Species ~ ., data = iris)
  testthat::expect_error(evaluate_model(model, iris, "Nonexistent"))
})

testthat::test_that("plot_lda works with a MASS LDA model", {
  .require_pkgs("MASS")
  data(iris)
  set.seed(123)
  train_idx <- sample(seq_len(nrow(iris)), size = floor(0.7 * nrow(iris)))
  train_data <- iris[train_idx, ]
  test_data <- iris[-train_idx, ]
  
  model <- MASS::lda(Species ~ ., data = train_data)
  testthat::expect_silent(plot_lda(model, test_data, "LD1", "Species"))
})

testthat::test_that("plot_lda errors with unsupported model type", {
  data(iris)
  dummy_model <- stats::lm(Sepal.Length ~ Sepal.Width, data = iris)
  testthat::expect_error(plot_lda(dummy_model, iris, "LD1", "Species"))
})
