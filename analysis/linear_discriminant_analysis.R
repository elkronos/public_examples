# Enhanced Analysis Functions and Unit Acceptance Tests
# ------------------------------------------------------
# Load necessary packages
library(MASS)
library(ggplot2)
library(caret)
library(heplots)
library(GGally)
library(testthat)
# Note: We refer to rlang functions without attaching the package
# (e.g., rlang::sym) to avoid namespace conflicts.

# ------------------------------------------------------------------------------
# Helper Function: Check if required variables exist in a data frame
# ------------------------------------------------------------------------------
.check_variables_exist <- function(data, vars) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following variable(s) are not in the data: ",
         paste(missing_vars, collapse = ", "))
  }
}

# ------------------------------------------------------------------------------
# Function: Plot Histograms of Specified Variables
# ------------------------------------------------------------------------------
#' Plot Histograms of Specified Variables
#'
#' Iterates over a set of variables from a data frame and prints a ggplot2 histogram
#' for each. Non‐numeric variables are coerced to numeric (with a warning).
#'
#' @param data A data frame containing the variables to be plotted.
#' @param variables A character vector of variable names to be plotted.
#' @param binwidth Numeric value for the bin width (default is 0.5).
#'
#' @return Invisibly returns a list of ggplot objects (one per variable).
#' @export
plot_histograms <- function(data, variables, binwidth = 0.5) {
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (!is.character(variables)) stop("variables must be a character vector.")
  .check_variables_exist(data, variables)
  
  plots <- list()
  for (var in variables) {
    # Check if variable is numeric; if not, attempt coercion and warn the user.
    if (!is.numeric(data[[var]])) {
      warning(sprintf("Variable '%s' is not numeric. Attempting to coerce to numeric.", var))
      data[[var]] <- as.numeric(as.character(data[[var]]))
      if (all(is.na(data[[var]]))) {
        warning(sprintf("Variable '%s' could not be coerced to numeric. Skipping plot.", var))
        next
      }
    }
    # Use modern tidy evaluation instead of deprecated aes_string()
    p <- ggplot(data, aes(x = !!rlang::sym(var))) +
      geom_histogram(binwidth = binwidth, fill = "lightblue", color = "black") +
      ggtitle(paste("Histogram of", var)) +
      theme_minimal()
    print(p)
    plots[[var]] <- p
  }
  invisible(plots)
}

# ------------------------------------------------------------------------------
# Function: Check Normality of Variables within Groups
# ------------------------------------------------------------------------------
#' Check Normality of Variables within Groups
#'
#' Performs the Shapiro–Wilk test for normality on the specified variables within
#' groups defined by a grouping variable. Prints and returns a data frame with the
#' test statistics.
#'
#' @param data A data frame containing the variables and the grouping variable.
#' @param variables A character vector of variable names to test.
#' @param grouping_var A single character string with the name of the grouping variable.
#'
#' @return Invisibly returns a data frame with columns: Group, Variable, W, p_value, and n.
#' @export
check_normality <- function(data, variables, grouping_var) {
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (!is.character(variables)) stop("variables must be a character vector.")
  if (!is.character(grouping_var) || length(grouping_var) != 1)
    stop("grouping_var must be a single character string.")
  
  .check_variables_exist(data, variables)
  if (!(grouping_var %in% names(data))) stop("Grouping variable not found in data.")
  
  results <- data.frame(Group = character(),
                        Variable = character(),
                        W = numeric(),
                        p_value = numeric(),
                        n = integer(),
                        stringsAsFactors = FALSE)
  
  groups <- unique(data[[grouping_var]])
  for (grp in groups) {
    grp_data <- data[data[[grouping_var]] == grp, ]
    for (var in variables) {
      if (!is.numeric(grp_data[[var]])) {
        warning(sprintf("Variable '%s' in group '%s' is not numeric. Skipping normality test.", var, grp))
        next
      }
      n <- sum(!is.na(grp_data[[var]]))
      if (n < 3) {
        warning(sprintf("Not enough observations (n = %d) in group '%s' for variable '%s' to perform the Shapiro–Wilk test. Skipping.", n, grp, var))
        next
      }
      test_result <- shapiro.test(grp_data[[var]])
      results <- rbind(results,
                       data.frame(Group = grp,
                                  Variable = var,
                                  W = test_result$statistic,
                                  p_value = test_result$p.value,
                                  n = n,
                                  stringsAsFactors = FALSE))
      cat(sprintf("Group: %s, Variable: %s, n: %d, W: %.4f, p-value: %.4g\n",
                  grp, var, n, test_result$statistic, test_result$p.value))
    }
  }
  invisible(results)
}

# ------------------------------------------------------------------------------
# Function: Check for Homogeneity of Variance-Covariance
# ------------------------------------------------------------------------------
#' Check for Homogeneity of Variance-Covariance
#'
#' Uses Box's M test (via heplots::boxM) to assess whether the covariance matrices
#' of the specified variables are equal across groups.
#'
#' @param data A data frame containing the data.
#' @param variables A character vector with the names of the numeric variables.
#' @param grouping_var A single character string with the name of the grouping variable.
#'
#' @return Invisibly returns the result of the Box's M test.
#' @export
check_covariance_homogeneity <- function(data, variables, grouping_var) {
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (!is.character(variables)) stop("variables must be a character vector.")
  if (!is.character(grouping_var) || length(grouping_var) != 1)
    stop("grouping_var must be a single character string.")
  
  .check_variables_exist(data, variables)
  if (!(grouping_var %in% names(data))) stop("Grouping variable not found in data.")
  
  # Ensure that all specified variables are numeric.
  for (var in variables) {
    if (!is.numeric(data[[var]])) {
      stop(sprintf("Variable '%s' is not numeric.", var))
    }
  }
  
  result <- heplots::boxM(as.matrix(data[variables]), group = data[[grouping_var]])
  print(result)
  invisible(result)
}

# ------------------------------------------------------------------------------
# Function: Plot Pair Plots to Check for Linearity
# ------------------------------------------------------------------------------
#' Plot Pair Plots to Check for Linearity
#'
#' Creates pair plots (scatterplot matrices) for the specified variables, coloring
#' the points by a provided categorical variable.
#'
#' @param data A data frame containing the variables.
#' @param variables A character vector of variable names to include in the pair plot.
#' @param color_var A single character string with the name of the variable for color-coding.
#'
#' @return Invisibly returns the ggpair object.
#' @export
plot_pairplots <- function(data, variables, color_var) {
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (!is.character(variables)) stop("variables must be a character vector.")
  if (!is.character(color_var) || length(color_var) != 1)
    stop("color_var must be a single character string.")
  
  .check_variables_exist(data, variables)
  if (!(color_var %in% names(data))) stop("color_var not found in data.")
  
  p <- GGally::ggpairs(data,
                       columns = variables,
                       mapping = aes(color = !!rlang::sym(color_var))) +
    theme_minimal()
  print(p)
  invisible(p)
}

# ------------------------------------------------------------------------------
# Function: Evaluate Classification Model Performance
# ------------------------------------------------------------------------------
#' Evaluate Classification Model Performance
#'
#' Makes predictions with a classification model and computes various metrics such as
#' confusion matrix, accuracy, precision, recall, and F1 score. Results are printed and returned.
#'
#' @param model A model object compatible with the predict() method.
#' @param test_data A data frame containing the test data.
#' @param true_col A single character string with the name of the column containing the true class labels.
#'
#' @return Invisibly returns a list with components: confusion_mat, accuracy, precision, recall, and F1.
#' @export
evaluate_model <- function(model, test_data, true_col) {
  if (!is.data.frame(test_data)) stop("test_data must be a data frame.")
  if (!is.character(true_col) || length(true_col) != 1)
    stop("true_col must be a single character string.")
  if (!(true_col %in% names(test_data))) stop("true_col not found in test_data.")
  
  # Obtain predictions. For an LDA model from MASS, predict() returns a list;
  # use the 'class' element if available.
  pred <- predict(model, newdata = test_data)
  if (is.list(pred) && "class" %in% names(pred)) {
    predictions <- pred$class
  } else {
    predictions <- pred
  }
  
  if (is.factor(predictions)) predictions <- as.character(predictions)
  actual <- test_data[[true_col]]
  if (is.factor(actual)) actual <- as.character(actual)
  
  confusion_mat <- table(Predicted = predictions, Actual = actual)
  cat("\nConfusion Matrix:\n")
  print(confusion_mat)
  
  accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
  
  # Calculate per-class precision, recall, and F1 scores.
  precision <- diag(confusion_mat) / rowSums(confusion_mat)
  recall <- diag(confusion_mat) / colSums(confusion_mat)
  F1 <- 2 * (precision * recall) / (precision + recall)
  
  cat(sprintf("Overall Accuracy: %.4f\n", accuracy))
  cat("Precision by class:\n"); print(precision)
  cat("Recall by class:\n"); print(recall)
  cat("F1 Score by class:\n"); print(F1)
  
  invisible(list(confusion_mat = confusion_mat,
                 accuracy = accuracy,
                 precision = precision,
                 recall = recall,
                 F1 = F1))
}

# ------------------------------------------------------------------------------
# Function: Visualize Linear Discriminant Analysis (LDA) Results
# ------------------------------------------------------------------------------
#' Visualize Linear Discriminant Analysis (LDA) Results
#'
#' Generates a density plot of a specified linear discriminant (e.g., "LD1") from an LDA model.
#' Supports models of class "lda" (from MASS) or a caret train object that wraps an LDA model.
#'
#' @param model An LDA model (either a MASS::lda object or a caret train object with LDA).
#' @param test_data A data frame containing the data for prediction.
#' @param lda_col A single character string specifying the column (e.g., "LD1") to plot.
#' @param color_var A single character string with the name of the variable to use for color-coding.
#'
#' @return Invisibly returns the ggplot object.
#' @export
plot_lda <- function(model, test_data, lda_col, color_var) {
  if (!is.data.frame(test_data)) stop("test_data must be a data frame.")
  if (!is.character(lda_col) || length(lda_col) != 1)
    stop("lda_col must be a single character string.")
  if (!is.character(color_var) || length(color_var) != 1)
    stop("color_var must be a single character string.")
  
  ld_data <- NULL
  predicted_class <- NULL
  
  if ("train" %in% class(model)) {
    # For a caret train object with an LDA final model
    if (!("finalModel" %in% names(model)))
      stop("The provided train model does not contain a finalModel element.")
    required_cols <- colnames(model$finalModel$means)
    .check_variables_exist(test_data, required_cols)
    
    pred_out <- predict(model$finalModel, newdata = test_data[, required_cols, drop = FALSE])
    ld_data <- as.data.frame(pred_out$x)
    predicted_class <- predict(model, newdata = test_data)
  } else if ("lda" %in% class(model)) {
    pred_out <- predict(model, newdata = test_data)
    ld_data <- as.data.frame(pred_out$x)
    predicted_class <- pred_out$class
  } else {
    stop("Model type not supported. Must be of class 'lda' or a caret 'train' object with LDA.")
  }
  
  # Rename columns to standard names: "LD1", "LD2", etc.
  colnames(ld_data) <- paste0("LD", seq_len(ncol(ld_data)))
  
  # Append predicted classes for use in coloring.
  ld_data[[color_var]] <- predicted_class
  
  if (!(lda_col %in% names(ld_data))) {
    stop(sprintf("Variable '%s' not found in the LDA results.", lda_col))
  }
  
  # Drop groups with fewer than 2 observations to avoid plotting issues.
  group_counts <- table(ld_data[[color_var]])
  low_groups <- names(group_counts[group_counts < 2])
  if (length(low_groups) > 0) {
    warning(sprintf("%d group(s) have fewer than two data points and will be dropped from the plot. Groups: %s",
                    length(low_groups), paste(low_groups, collapse = ", ")))
    ld_data <- ld_data[!(ld_data[[color_var]] %in% low_groups), ]
  }
  
  p <- ggplot(ld_data, aes(x = !!rlang::sym(lda_col),
                           fill = !!rlang::sym(color_var))) +
    geom_density(alpha = 0.5) +
    labs(title = "Density plot of the Linear Discriminant", x = lda_col, fill = color_var) +
    theme_minimal() +
    guides(fill = guide_legend(title = color_var))
  
  print(p)
  invisible(p)
}

# ------------------------------------------------------------------------------
# Unit Acceptance Testing (UAT) using testthat
# ------------------------------------------------------------------------------
# The following tests check every parameter and function behavior.

test_that("plot_histograms works with numeric variables", {
  test_data <- data.frame(a = rnorm(100), b = rnorm(100))
  expect_silent(plot_histograms(test_data, c("a", "b")))
})

test_that("plot_histograms warns with non-numeric variables", {
  test_data <- data.frame(a = rnorm(100), b = as.character(rnorm(100)))
  expect_warning(plot_histograms(test_data, c("b")), regexp = "Attempting to coerce")
})

test_that("check_normality returns results and warns with insufficient data", {
  test_data <- data.frame(
    value = c(rnorm(10), rnorm(2)), 
    group = rep(c("G1", "G2"), times = c(10, 2))
  )
  result <- check_normality(test_data, "value", "group")
  # Should return results for group G1 but skip G2 (since n < 3)
  expect_true("G1" %in% result$Group)
  expect_false("G2" %in% result$Group)
})

test_that("check_normality errors if grouping_var not found", {
  test_data <- data.frame(a = rnorm(100))
  expect_error(check_normality(test_data, "a", "nonexistent"))
})

test_that("check_covariance_homogeneity works with valid input", {
  test_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    group = rep(c("A", "B"), 50)
  )
  result <- check_covariance_homogeneity(test_data, c("x", "y"), "group")
  expect_true("statistic" %in% names(result))
})

test_that("check_covariance_homogeneity errors for non-numeric variable", {
  test_data <- data.frame(
    x = rnorm(100),
    y = letters[1:100],
    group = rep(c("A", "B"), 50)
  )
  expect_error(check_covariance_homogeneity(test_data, c("x", "y"), "group"))
})

test_that("plot_pairplots works with valid input", {
  test_data <- data.frame(
    a = rnorm(50),
    b = rnorm(50),
    c = rnorm(50),
    group = sample(letters[1:3], 50, replace = TRUE)
  )
  expect_silent(plot_pairplots(test_data, c("a", "b", "c"), "group"))
})

test_that("plot_pairplots errors if color_var missing", {
  test_data <- data.frame(a = rnorm(50), b = rnorm(50))
  expect_error(plot_pairplots(test_data, c("a", "b"), "group"))
})

test_that("evaluate_model works with a simple LDA model", {
  data(iris)
  set.seed(123)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.7 * nrow(iris))
  train_data <- iris[train_idx, ]
  test_data <- iris[-train_idx, ]
  
  model <- MASS::lda(Species ~ ., data = train_data)
  result <- evaluate_model(model, test_data, "Species")
  expect_true(is.list(result))
  expect_true("confusion_mat" %in% names(result))
})

test_that("evaluate_model errors when true_col not in test_data", {
  data(iris)
  model <- MASS::lda(Species ~ ., data = iris)
  expect_error(evaluate_model(model, iris, "Nonexistent"))
})

test_that("plot_lda works with a MASS LDA model", {
  data(iris)
  set.seed(123)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.7 * nrow(iris))
  train_data <- iris[train_idx, ]
  test_data <- iris[-train_idx, ]
  
  model <- MASS::lda(Species ~ ., data = train_data)
  # Use the first linear discriminant ("LD1")
  expect_silent(plot_lda(model, test_data, "LD1", "Species"))
})

test_that("plot_lda errors with unsupported model type", {
  dummy_model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  expect_error(plot_lda(dummy_model, iris, "LD1", "Species"))
})

# ------------------------------------------------------------------------------
# Run tests if running interactively or under a testing environment.
if (interactive() || Sys.getenv("TESTTHAT") == "true") {
  test_dir(tempdir(), reporter = "Summary")
}

# ------------------------------------------------------------------------------
# Example Usage (Commented Out)
# ------------------------------------------------------------------------------
# \dontrun{
#   # Synthetic Data Creation:
#   set.seed(123)
#   n <- 150
#   species_A <- data.frame(
#     Sepal.Length = rnorm(n/3, mean = 5, sd = 0.5),
#     Sepal.Width  = rnorm(n/3, mean = 3.5, sd = 0.3),
#     Petal.Length = rnorm(n/3, mean = 1.5, sd = 0.3),
#     Petal.Width  = rnorm(n/3, mean = 0.3, sd = 0.1),
#     Species     = factor(rep("A", n/3))
#   )
#   species_B <- data.frame(
#     Sepal.Length = rnorm(n/3, mean = 6, sd = 0.5),
#     Sepal.Width  = rnorm(n/3, mean = 3, sd = 0.3),
#     Petal.Length = rnorm(n/3, mean = 4.5, sd = 0.4),
#     Petal.Width  = rnorm(n/3, mean = 1.4, sd = 0.2),
#     Species     = factor(rep("B", n/3))
#   )
#   species_C <- data.frame(
#     Sepal.Length = rnorm(n/3, mean = 6.5, sd = 0.6),
#     Sepal.Width  = rnorm(n/3, mean = 3.2, sd = 0.3),
#     Petal.Length = rnorm(n/3, mean = 5.5, sd = 0.5),
#     Petal.Width  = rnorm(n/3, mean = 2, sd = 0.2),
#     Species     = factor(rep("C", n/3))
#   )
#   synthetic_data <- rbind(species_A, species_B, species_C)
#   
#   # Check assumptions:
#   plot_histograms(synthetic_data, names(synthetic_data)[1:4])
#   check_normality(synthetic_data, names(synthetic_data)[1:4], "Species")
#   check_covariance_homogeneity(synthetic_data, names(synthetic_data)[1:4], "Species")
#   plot_pairplots(synthetic_data, names(synthetic_data)[1:4], "Species")
#   
#   # Split the Data:
#   set.seed(123)
#   splitIndex <- createDataPartition(synthetic_data$Species, p = .7, list = FALSE, times = 1)
#   train_data <- synthetic_data[splitIndex, ]
#   test_data  <- synthetic_data[-splitIndex, ]
#   
#   # Perform LDA with Cross-Validation:
#   train_control <- trainControl(method = "cv", number = 10)
#   lda_cv_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#                         data = train_data,
#                         method = "lda",
#                         trControl = train_control,
#                         metric = "Accuracy")
#   
#   # Evaluate Model:
#   evaluate_model(lda_cv_model, test_data, "Species")
#   
#   # Visualize LDA:
#   plot_lda(lda_cv_model, test_data, lda_col = "LD1", color_var = "Species")
# }
