#############################
# Begin: Function Definitions
#############################

#' Apply demographic weights to numeric columns in a data frame
#'
#' This function applies demographic weights to one or more numeric columns in a data frame.
#' Each specified numeric column is multiplied by a total weight calculated as the product
#' of the weights corresponding to each demographic variable. Optionally, intermediate
#' demographic-specific weights are added to the output.
#'
#' @param data A data frame.
#' @param demographic_vars A character vector of the names of demographic variables to use for weighting.
#' @param demographic_weights A named list. Each element should be a named numeric vector that gives
#'   the weights for the corresponding demographic variable (names must match the levels of the factor).
#' @param columns_to_weight Optional character vector specifying which numeric columns to weight.
#'   If `NULL` (the default), all numeric columns in `data` are used.
#' @param new_column_names Optional character vector of new names for the weighted columns.
#'   If provided, its length must equal the number of columns being weighted.
#'   If `NULL` (the default), new column names are created by appending `value_suffix` to the original names.
#' @param weight_suffix A character string appended to column names when adding weight information.
#'   Defaults to `"_weight"`.
#' @param value_suffix A character string appended to original column names to create the final weighted
#'   column names. Defaults to `"_weighted"`.
#' @param verbose Logical. If `TRUE`, columns with the individual demographic weights are added to the output.
#' @param drop_na Logical. If `TRUE` (the default), rows with missing values in any of the demographic or numeric
#'   columns are removed before processing.
#'
#' @return A data frame with the original columns plus new columns:
#'   \itemize{
#'     \item For each numeric column, a column with its total weight is added (named `original_total<weight_suffix>`).
#'     \item For each numeric column, a weighted version is added (named using `new_column_names` or by appending `value_suffix`).
#'     \item If `verbose = TRUE`, for each numeric column and demographic variable, a column with the individual
#'           weight is added (named `original_demographic<weight_suffix>`).
#'   }
#'
#' @examples
#' data <- data.frame(
#'   x = c(1, 2, 3, 4, 5),
#'   y = c(6, 7, 8, 9, 10),
#'   gender = c("male", "female", "male", "male", "female"),
#'   race = c("white", "black", "white", "black", "white"),
#'   stringsAsFactors = FALSE
#' )
#'
#' demographic_weights <- list(
#'   gender = c(male = 0.5, female = 1.5),
#'   race = c(white = 1.2, black = 0.8)
#' )
#'
#' # Basic usage without verbose
#' result <- apply_weights(data, demographic_vars = c("gender", "race"), 
#'                         demographic_weights = demographic_weights)
#'
#' # Verbose usage with custom weighted column names
#' result_verbose <- apply_weights(data, demographic_vars = c("gender", "race"), 
#'                                 demographic_weights = demographic_weights,
#'                                 columns_to_weight = c("x", "y"),
#'                                 new_column_names = c("new_x", "new_y"),
#'                                 verbose = TRUE)
#'
#' @export
apply_weights <- function(data, 
                          demographic_vars, 
                          demographic_weights, 
                          columns_to_weight = NULL, 
                          new_column_names = NULL, 
                          weight_suffix = "_weight", 
                          value_suffix = "_weighted", 
                          verbose = FALSE,
                          drop_na = TRUE) {
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  # Check that demographic_vars exist in data
  missing_demos <- setdiff(demographic_vars, names(data))
  if (length(missing_demos) > 0) {
    stop("The following demographic variables are missing in data: ", 
         paste(missing_demos, collapse = ", "))
  }
  
  # Convert demographic_vars to factors if necessary
  for (var in demographic_vars) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- as.factor(data[[var]])
    }
  }
  
  # Validate demographic_weights: each provided weight vector must be named and numeric.
  for (var in demographic_vars) {
    if (!is.null(demographic_weights[[var]])) {
      weights_vec <- demographic_weights[[var]]
      if (!is.numeric(weights_vec) || is.null(names(weights_vec))) {
        stop(sprintf("`demographic_weights` for '%s' must be a named numeric vector.", var))
      }
      # Ensure all levels of the demographic factor have a corresponding weight.
      missing_levels <- setdiff(levels(data[[var]]), names(weights_vec))
      if (length(missing_levels) > 0) {
        stop(sprintf("`demographic_weights` for '%s' is missing weights for levels: %s", 
                     var, paste(missing_levels, collapse = ", ")))
      }
    }
  }
  
  # Determine which columns to weight
  if (is.null(columns_to_weight)) {
    columns_to_weight <- names(data)[sapply(data, is.numeric)]
  } else {
    valid_cols <- intersect(columns_to_weight, names(data))
    if (length(valid_cols) == 0) {
      stop("None of the specified `columns_to_weight` exist in data.")
    }
    # Only keep numeric columns.
    valid_cols <- valid_cols[sapply(valid_cols, function(col) is.numeric(data[[col]]))]
    if (length(valid_cols) == 0) {
      stop("No numeric columns were found among the specified `columns_to_weight`.")
    }
    columns_to_weight <- valid_cols
  }
  
  # Validate new_column_names if provided
  if (!is.null(new_column_names)) {
    if (!is.character(new_column_names)) {
      stop("`new_column_names` must be a character vector.")
    }
    if (length(new_column_names) != length(columns_to_weight)) {
      stop("Length of `new_column_names` must equal the number of columns being weighted.")
    }
  }
  
  # Optionally drop rows with missing values in any demographic or numeric column
  if (drop_na) {
    relevant_vars <- unique(c(demographic_vars, columns_to_weight))
    data <- data[stats::complete.cases(data[, relevant_vars, drop = FALSE]), ]
  }
  
  # --- Apply Weights ---
  weighted_data <- data
  for (i in seq_along(columns_to_weight)) {
    col_name <- columns_to_weight[i]
    new_value_col <- if (!is.null(new_column_names)) new_column_names[i] else paste0(col_name, value_suffix)
    weighted_data <- apply_demographic_weights_to_column(
      data = weighted_data, 
      column_name = col_name, 
      demographic_vars = demographic_vars, 
      demographic_weights = demographic_weights, 
      weight_suffix = weight_suffix, 
      new_value_col = new_value_col, 
      verbose = verbose
    )
  }
  
  return(weighted_data)
}

#' Apply demographic weights to a single numeric column
#'
#' This helper function computes a total weight for a single numeric column by multiplying
#' the weights corresponding to each demographic variable. Optionally, it also adds columns
#' containing the individual weights.
#'
#' @param data A data frame.
#' @param column_name A character string specifying the name of the numeric column to weight.
#' @param demographic_vars A character vector of demographic variable names.
#' @param demographic_weights A list of named numeric vectors (weights) for the demographic variables.
#' @param weight_suffix A character string to append to new weight columns.
#' @param new_value_col A character string for the name of the final weighted numeric column.
#' @param verbose Logical. If `TRUE`, adds columns with individual demographic weights.
#'
#' @return A data frame with new columns added:
#'   \itemize{
#'     \item A total weight column named `column_name_total<weight_suffix>`.
#'     \item A weighted numeric column named as specified by `new_value_col`.
#'     \item If `verbose = TRUE`, for each demographic variable, a column named `column_name_demographic<weight_suffix>` is added.
#'   }
#'
#' @keywords internal
apply_demographic_weights_to_column <- function(data, 
                                                column_name, 
                                                demographic_vars, 
                                                demographic_weights, 
                                                weight_suffix, 
                                                new_value_col, 
                                                verbose) {
  n <- nrow(data)
  weight_list <- list()
  
  for (var in demographic_vars) {
    # Convert factor levels to character so they can be used to index the weight vector.
    level_vals <- as.character(data[[var]])
    
    # Retrieve the weights; if no weights are provided for this demographic, default to 1.
    if (!is.null(demographic_weights[[var]])) {
      w <- demographic_weights[[var]][level_vals]
    } else {
      w <- rep(1, n)
    }
    
    weight_list[[var]] <- w
    
    # If verbose, add the individual weight column for this demographic variable.
    if (verbose) {
      verbose_col_name <- paste0(column_name, "_", var, weight_suffix)
      data[[verbose_col_name]] <- w
    }
  }
  
  # Compute the total weight by multiplying across demographic variables.
  total_weight <- if (length(weight_list) > 0) {
    Reduce(`*`, weight_list)
  } else {
    rep(1, n)
  }
  
  # Add the total weight column.
  total_weight_col <- paste0(column_name, "_total", weight_suffix)
  data[[total_weight_col]] <- total_weight
  
  # Compute the weighted values and add the new numeric column.
  data[[new_value_col]] <- data[[column_name]] * total_weight
  
  return(data)
}

#############################
# End: Function Definitions
#############################

#############################
# Begin: User Acceptance Tests (UAT)
#############################

# Install and load testthat if not already available
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
library(testthat)

context("Testing apply_weights functionality")

# Create a sample data frame for testing
sample_data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(6, 7, 8, 9, 10),
  gender = c("male", "female", "male", "male", "female"),
  race = c("white", "black", "white", "black", "white"),
  stringsAsFactors = FALSE
)

# Define valid demographic weights
demographic_weights <- list(
  gender = c(male = 0.5, female = 1.5),
  race   = c(white = 1.2, black = 0.8)
)

test_that("apply_weights works in non-verbose mode", {
  result <- apply_weights(
    data = sample_data,
    demographic_vars = c("gender", "race"),
    demographic_weights = demographic_weights,
    verbose = FALSE
  )
  
  # Expected new columns: x_total_weight, x_weighted, y_total_weight, y_weighted
  expect_true("x_total_weight" %in% names(result))
  expect_true("x_weighted" %in% names(result))
  expect_true("y_total_weight" %in% names(result))
  expect_true("y_weighted" %in% names(result))
  
  # Verify that no verbose columns (like x_gender_weight) exist
  expect_false(any(grepl("x_gender_weight", names(result))))
  
  # Check calculated values for the first row.
  # Row 1: gender = "male" (weight 0.5), race = "white" (weight 1.2)
  # Total weight = 0.5 * 1.2 = 0.6, so x_weighted = 1 * 0.6 = 0.6, y_weighted = 6 * 0.6 = 3.6.
  expect_equal(result$x_total_weight[1], 0.6)
  expect_equal(result$x_weighted[1], 0.6)
  expect_equal(result$y_total_weight[1], 0.6)
  expect_equal(result$y_weighted[1], 3.6)
})

test_that("apply_weights works in verbose mode", {
  result <- apply_weights(
    data = sample_data,
    demographic_vars = c("gender", "race"),
    demographic_weights = demographic_weights,
    verbose = TRUE
  )
  
  # Check that verbose columns exist
  expect_true("x_gender_weight" %in% names(result))
  expect_true("x_race_weight" %in% names(result))
  
  # Verify that the verbose weight values match the expected values.
  # For row 2: gender = "female" (weight 1.5), race = "black" (weight 0.8)
  expect_equal(result[["x_gender_weight"]][2], 1.5)
  expect_equal(result[["x_race_weight"]][2], 0.8)
})

test_that("apply_weights works with custom numeric columns and new column names", {
  result <- apply_weights(
    data = sample_data,
    demographic_vars = c("gender", "race"),
    demographic_weights = demographic_weights,
    columns_to_weight = c("x", "y"),
    new_column_names = c("new_x", "new_y"),
    verbose = TRUE
  )
  
  # The new weighted columns should be named "new_x" and "new_y"
  expect_true("new_x" %in% names(result))
  expect_true("new_y" %in% names(result))
  
  # Also, verbose columns should exist for column "x" and "y"
  expect_true("x_gender_weight" %in% names(result))
  expect_true("y_race_weight" %in% names(result))
})

test_that("apply_weights errors on missing demographic variable", {
  # Expect an error if a demographic variable is not found in the data
  expect_error(
    apply_weights(
      data = sample_data,
      demographic_vars = c("nonexistent_var"),
      demographic_weights = demographic_weights
    ),
    "missing in data"
  )
})

test_that("apply_weights errors when demographic weights are missing a factor level", {
  # Create a bad weights list missing the weight for "female"
  bad_weights <- list(
    gender = c(male = 0.5),  # missing female
    race = c(white = 1.2, black = 0.8)
  )
  expect_error(
    apply_weights(
      data = sample_data,
      demographic_vars = c("gender", "race"),
      demographic_weights = bad_weights
    ),
    "missing weights for levels"
  )
})

test_that("apply_weights errors when specified numeric column does not exist", {
  expect_error(
    apply_weights(
      data = sample_data,
      demographic_vars = c("gender", "race"),
      demographic_weights = demographic_weights,
      columns_to_weight = c("nonexistent_column")
    ),
    "None of the specified `columns_to_weight` exist in data"
  )
})

test_that("apply_weights drops rows with NA when drop_na = TRUE", {
  data_with_na <- sample_data
  data_with_na$x[2] <- NA  # Introduce an NA in a numeric column
  result <- apply_weights(
    data = data_with_na,
    demographic_vars = c("gender", "race"),
    demographic_weights = demographic_weights,
    drop_na = TRUE
  )
  # Since one row had an NA, the resulting data should have one fewer row.
  expect_equal(nrow(result), nrow(data_with_na) - 1)
})

test_that("apply_weights does not drop rows when drop_na = FALSE", {
  data_with_na <- sample_data
  data_with_na$x[2] <- NA  # Introduce an NA in a numeric column
  result <- apply_weights(
    data = data_with_na,
    demographic_vars = c("gender", "race"),
    demographic_weights = demographic_weights,
    drop_na = FALSE
  )
  # The row with NA should remain in the data.
  expect_equal(nrow(result), nrow(data_with_na))
})

