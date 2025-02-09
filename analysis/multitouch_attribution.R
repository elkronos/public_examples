# Load required packages
library(dplyr)
library(tidyr)
library(tidyselect)
library(progress)
library(assertthat)

# ------------------------------------------------------------------------------
# Helper function: Validate input data
# ------------------------------------------------------------------------------
#' Validate Input Data for MTA Functions
#'
#' Checks that the data frame is valid (i.e. is a data.frame and contains the required columns).
#'
#' @param data A data frame to be validated.
#' @param required_columns A character vector of required column names.
#'   Default: c("customer_id", "touchpoint", "conversion", "timestamp").
#'
#' @return Invisibly returns TRUE if the data is valid; otherwise, stops with an error.
#' @export
mta_validate_data <- function(data, required_columns = c("customer_id", "touchpoint", "conversion", "timestamp")) {
  if (!is.data.frame(data)) {
    stop("The input data should be a data.frame.")
  }
  missing_cols <- setdiff(required_columns, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from the data: ", paste(missing_cols, collapse = ", "))
  }
  # Check that the timestamp column is of a proper date/time class
  if (!inherits(data$timestamp, "POSIXct") && !inherits(data$timestamp, "POSIXt")) {
    warning("The 'timestamp' column is not of POSIXct/POSIXt type. Attempting to convert using as.POSIXct.")
    data$timestamp <- as.POSIXct(data$timestamp)
    if (any(is.na(data$timestamp))) {
      stop("Conversion of 'timestamp' to POSIXct resulted in NA values.")
    }
  }
  # Ensure conversion is numeric or logical
  if (!is.numeric(data$conversion) && !is.logical(data$conversion)) {
    stop("The 'conversion' column should be numeric or logical.")
  }
  return(invisible(TRUE))
}

# ------------------------------------------------------------------------------
# Function: Rename columns
# ------------------------------------------------------------------------------
#' Rename Columns of a Data Frame for MTA Analysis
#'
#' Renames the specified columns of a data frame. The new column names must be chosen from
#' c("customer_id", "touchpoint", "conversion", "timestamp"). You may specify the columns to rename
#' by name or by position.
#'
#' @param data A data frame whose columns need to be renamed.
#' @param current_names_or_positions A character vector (column names) or numeric vector (column positions)
#'   indicating the columns to rename.
#' @param new_names A character vector indicating the new column names.
#'
#' @return A data frame with the specified columns renamed.
#' @examples
#' # Rename by name:
#' df <- data.frame(cust_id = 1:3, tp = c("A", "B", "C"), conv = c(0, 1, 0),
#'                  ts = as.POSIXct(Sys.Date() - 0:2))
#' df_new <- mta_rename(df, current_names_or_positions = c("cust_id", "tp", "conv", "ts"),
#'                      new_names = c("customer_id", "touchpoint", "conversion", "timestamp"))
#'
#' # Rename by column positions:
#' df_new2 <- mta_rename(df, current_names_or_positions = 1:4,
#'                       new_names = c("customer_id", "touchpoint", "conversion", "timestamp"))
#' @export
mta_rename <- function(data, current_names_or_positions, new_names) {
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data should be a data.frame.")
  }
  if (length(current_names_or_positions) != length(new_names)) {
    stop("The lengths of 'current_names_or_positions' and 'new_names' must be equal.")
  }
  
  allowed_new_names <- c("customer_id", "touchpoint", "conversion", "timestamp")
  if (any(!new_names %in% allowed_new_names)) {
    stop("New column names must be one or more of: ", paste(allowed_new_names, collapse = ", "))
  }
  
  # If current_names_or_positions is numeric, convert to column names
  if (is.numeric(current_names_or_positions)) {
    if (any(current_names_or_positions < 1 | current_names_or_positions > ncol(data))) {
      stop("One or more column positions in 'current_names_or_positions' are out of bounds.")
    }
    current_names_or_positions <- colnames(data)[current_names_or_positions]
  }
  
  missing_cols <- setdiff(current_names_or_positions, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following columns specified in 'current_names_or_positions' do not exist in the data: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # Create a renaming mapping: for each column (e.g., "cust_id") return the corresponding new name (e.g., "customer_id")
  renaming_map <- setNames(new_names, current_names_or_positions)
  
  # Use rename_with so that for each column in current_names_or_positions, its new name is taken from renaming_map
  data_renamed <- data %>%
    rename_with(~ renaming_map[.], all_of(current_names_or_positions))
  
  return(data_renamed)
}

# ------------------------------------------------------------------------------
# Function: Prepare data for attribution analysis
# ------------------------------------------------------------------------------
#' Prepare Data for Attribution Models
#'
#' Sorts the data by customer and timestamp, and adds a 'touch_rank' column indicating the order
#' of touchpoints in each customer journey.
#'
#' @param data A data frame containing at least 'customer_id', 'touchpoint', 'conversion', and 'timestamp' columns.
#'
#' @return A data frame with the added 'touch_rank' column.
#' @export
mta_prep <- function(data) {
  # Validate the input data
  mta_validate_data(data)
  
  if (nrow(data) == 0) {
    warning("The input data is empty. Returning an empty data frame.")
    return(data)
  }
  
  data_prepared <- data %>%
    arrange(customer_id, timestamp) %>%
    group_by(customer_id) %>%
    mutate(touch_rank = row_number()) %>%
    ungroup()
  
  return(data_prepared)
}

# ------------------------------------------------------------------------------
# Function: Linear attribution model
# ------------------------------------------------------------------------------
#' Linear Attribution Model
#'
#' Calculates linear attribution weights, assigning equal credit to all touchpoints.
#'
#' @param data A prepared data frame with 'customer_id', 'touchpoint', 'conversion', 'timestamp',
#'   and 'touch_rank' columns.
#'
#' @return A data frame with columns 'touchpoint', 'conversions', and 'attribution_weight'.
#' @export
mta_linear <- function(data) {
  if (nrow(data) == 0) {
    warning("The input data is empty. Returning an empty data frame for linear attribution.")
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric(), 
                      stringsAsFactors = FALSE))
  }
  
  result <- data %>%
    group_by(touchpoint) %>%
    summarise(conversions = sum(conversion, na.rm = TRUE), .groups = "drop")
  
  total_conv <- sum(result$conversions)
  if (total_conv == 0) {
    result <- result %>% mutate(attribution_weight = 0)
    warning("Total conversions are zero in linear attribution model.")
  } else {
    result <- result %>% mutate(attribution_weight = conversions / total_conv)
  }
  
  return(result)
}

# ------------------------------------------------------------------------------
# Function: First touch attribution model
# ------------------------------------------------------------------------------
#' First Touch Attribution Model
#'
#' Assigns full credit to the first touchpoint in each customer journey.
#'
#' @param data A prepared data frame with 'customer_id', 'touchpoint', 'conversion', 'timestamp',
#'   and 'touch_rank' columns.
#'
#' @return A data frame with columns 'touchpoint', 'conversions', and 'attribution_weight'.
#' @export
mta_first <- function(data) {
  first_touch <- data %>%
    filter(touch_rank == 1)
  
  if (nrow(first_touch) == 0) {
    warning("No first touch records found. Returning empty result for first touch attribution.")
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric(), 
                      stringsAsFactors = FALSE))
  }
  
  result <- first_touch %>%
    group_by(touchpoint) %>%
    summarise(conversions = sum(conversion, na.rm = TRUE), .groups = "drop")
  
  total_conv <- sum(result$conversions)
  if (total_conv == 0) {
    result <- result %>% mutate(attribution_weight = 0)
    warning("Total conversions are zero in first touch attribution model.")
  } else {
    result <- result %>% mutate(attribution_weight = conversions / total_conv)
  }
  
  return(result)
}

# ------------------------------------------------------------------------------
# Function: Last touch attribution model
# ------------------------------------------------------------------------------
#' Last Touch Attribution Model
#'
#' Assigns full credit to the last touchpoint that resulted in a conversion.
#'
#' @param data A prepared data frame with 'customer_id', 'touchpoint', 'conversion', 'timestamp',
#'   and 'touch_rank' columns.
#'
#' @return A data frame with columns 'touchpoint', 'conversions', and 'attribution_weight'.
#' @export
mta_last <- function(data) {
  last_touch <- data %>%
    filter(conversion == 1) %>%
    group_by(customer_id) %>%
    arrange(desc(timestamp)) %>%
    slice(1) %>%
    ungroup()
  
  if (nrow(last_touch) == 0) {
    warning("No conversion records found for last touch attribution. Returning empty result.")
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric(), 
                      stringsAsFactors = FALSE))
  }
  
  result <- last_touch %>%
    group_by(touchpoint) %>%
    summarise(conversions = n(), .groups = "drop")
  
  total_conv <- sum(result$conversions)
  if (total_conv == 0) {
    result <- result %>% mutate(attribution_weight = 0)
    warning("Total conversions are zero in last touch attribution model.")
  } else {
    result <- result %>% mutate(attribution_weight = conversions / total_conv)
  }
  
  return(result)
}

# ------------------------------------------------------------------------------
# Function: Position-based attribution model
# ------------------------------------------------------------------------------
#' Position-Based Attribution Model
#'
#' Calculates attribution weights based on the position of each touchpoint in the customer journey.
#' The weights for the first and last touchpoints are specified by the user (default 0.4 each), and
#' the remaining (middle) touchpoints share the remaining weight equally.
#'
#' @param data A prepared data frame with columns 'customer_id', 'touchpoint', 'conversion', 'timestamp', and 'touch_rank'.
#' @param first_touch_weight Numeric; the weight for the first touch (default: 0.4).
#' @param last_touch_weight Numeric; the weight for the last touch (default: 0.4).
#'
#' @return A data frame with columns 'touchpoint', 'conversion_weight', and 'attribution_weight'.
#' @export
mta_position <- function(data, first_touch_weight = 0.4, last_touch_weight = 0.4) {
  if (first_touch_weight + last_touch_weight > 1) {
    stop("The sum of first_touch_weight and last_touch_weight should not exceed 1.")
  }
  
  middle_touch_weight <- 1 - first_touch_weight - last_touch_weight
  
  data_position <- data %>%
    group_by(customer_id) %>%
    mutate(
      total_touchpoints = n(),
      position_weight = case_when(
        # If only one touchpoint, assign full credit if conversion occurred
        total_touchpoints == 1 ~ ifelse(conversion == 1, 1, 0),
        # First touch
        touch_rank == 1 ~ first_touch_weight,
        # Last touch (using explicit ranking)
        touch_rank == total_touchpoints ~ last_touch_weight,
        # Middle touchpoints
        TRUE ~ middle_touch_weight / (total_touchpoints - 2)
      )
    ) %>%
    ungroup()
  
  result <- data_position %>%
    group_by(touchpoint) %>%
    summarise(conversion_weight = sum(conversion * position_weight, na.rm = TRUE), .groups = "drop")
  
  total_weight <- sum(result$conversion_weight)
  if (total_weight == 0) {
    result <- result %>% mutate(attribution_weight = 0)
    warning("Total conversion weight is zero in position-based attribution model.")
  } else {
    result <- result %>% mutate(attribution_weight = conversion_weight / total_weight)
  }
  
  return(result)
}

# ------------------------------------------------------------------------------
# Function: Wrapper for Multitouch Attribution Analysis
# ------------------------------------------------------------------------------
#' Multitouch Attribution Analysis
#'
#' Calculates attribution weights for multiple models (linear, first touch, last touch, position-based)
#' and returns the results as a list.
#'
#' @param data A data frame with 'customer_id', 'touchpoint', 'conversion', and 'timestamp' columns.
#' @param models A character vector specifying the attribution models to calculate.
#'   Default: c("linear", "first_touch", "last_touch", "position_based").
#' @param progress_bar Logical; whether to display a progress bar during analysis (default: FALSE).
#'
#' @return A named list with the attribution results for each specified model.
#' @export
mta_analysis <- function(data, models = c("linear", "first_touch", "last_touch", "position_based"), progress_bar = FALSE) {
  # Validate input data and models
  mta_validate_data(data)
  valid_models <- c("linear", "first_touch", "last_touch", "position_based")
  
  if (!all(models %in% valid_models)) {
    stop("Invalid model(s) provided. Choose from: ", paste(valid_models, collapse = ", "))
  }
  
  if (!is.logical(progress_bar)) {
    stop("The 'progress_bar' parameter should be logical (TRUE or FALSE).")
  }
  
  data_prepared <- mta_prep(data)
  attribution_results <- list()
  
  if (progress_bar) {
    pb <- progress::progress_bar$new(total = length(models),
                                     format = "Calculating :current/:total [:bar] :percent ETA: :eta")
  }
  
  for (model in models) {
    if (model == "linear") {
      attribution_results[["linear"]] <- mta_linear(data_prepared)
    } else if (model == "first_touch") {
      attribution_results[["first_touch"]] <- mta_first(data_prepared)
    } else if (model == "last_touch") {
      attribution_results[["last_touch"]] <- mta_last(data_prepared)
    } else if (model == "position_based") {
      attribution_results[["position_based"]] <- mta_position(data_prepared)
    }
    
    if (progress_bar) {
      pb$tick()
    }
  }
  
  return(attribution_results)
}

# ------------------------------------------------------------------------------
# Function: Combine Touchpoint Weights
# ------------------------------------------------------------------------------
#' Combine Touchpoint Weights from Multiple Attribution Models
#'
#' Joins the attribution weights from multiple models to the original touchpoint data.
#'
#' @param data A data frame containing touchpoint-level data (must include a 'touchpoint' column).
#' @param attribution_results A list of attribution model results. Each element should be a data frame
#'   with at least 'touchpoint' and 'attribution_weight' columns.
#'
#' @return A data frame with the original data augmented by attribution weights from each model.
#' @export
mta_weights <- function(data, attribution_results) {
  if (!("touchpoint" %in% colnames(data))) {
    stop("The input data must have a 'touchpoint' column.")
  }
  
  data_with_weights <- data
  
  for (model_name in names(attribution_results)) {
    model_df <- attribution_results[[model_name]]
    if (!("touchpoint" %in% colnames(model_df)) || !("attribution_weight" %in% colnames(model_df))) {
      stop("Each attribution result must contain 'touchpoint' and 'attribution_weight' columns.")
    }
    
    # Add missing touchpoints (if any) with zero conversions and weight
    missing_touchpoints <- setdiff(unique(data$touchpoint), model_df$touchpoint)
    if (length(missing_touchpoints) > 0) {
      missing_df <- data.frame(
        touchpoint = missing_touchpoints,
        conversions = 0,
        attribution_weight = 0,
        stringsAsFactors = FALSE
      )
      model_df <- bind_rows(model_df, missing_df) %>%
        arrange(touchpoint)
    }
    
    suffix <- paste0("_", model_name)
    data_with_weights <- left_join(data_with_weights, model_df, by = "touchpoint", suffix = c("", suffix))
  }
  
  return(data_with_weights)
}


# ===============================
# UAT Script for Multitouch Attribution Functions
# ===============================

# --- Load required libraries ---
library(dplyr)
library(tidyr)
library(tidyselect)
library(progress)
library(assertthat)

# --- Include the attribution functions ---
# (Assuming you have already sourced or defined the functions below in your session.)
# If the functions are saved in a separate file (e.g., "mta_functions.R"), you can do:
# source("mta_functions.R")

# (Below is the full set of functions from our previous version.)
# [Paste the complete functions from the previous answer here if not already loaded]

# For clarity, the functions below should already be in your R environment:
#   mta_validate_data(), mta_rename(), mta_prep(), mta_linear(), 
#   mta_first(), mta_last(), mta_position(), mta_analysis(), mta_weights()


# ===============================
# UAT: Positive Tests
# ===============================

cat("\n===== UAT: Positive Tests =====\n\n")

### Test 1: Renaming Columns with mta_rename()
cat("Test 1: mta_rename()\n")
# Create sample data with nonstandard column names
sample_data <- data.frame(
  cust_id = 1:5,
  tp = sample(c("A", "B", "C"), 5, replace = TRUE),
  conv = sample(c(0, 1), 5, replace = TRUE),
  ts = as.POSIXct(Sys.Date() - sample(0:4, 5, replace = TRUE))
)
cat("  Original columns:", paste(colnames(sample_data), collapse = ", "), "\n")
# Rename columns
renamed_data <- mta_rename(
  sample_data,
  current_names_or_positions = c("cust_id", "tp", "conv", "ts"),
  new_names = c("customer_id", "touchpoint", "conversion", "timestamp")
)
cat("  Renamed columns: ", paste(colnames(renamed_data), collapse = ", "), "\n\n")


### Test 2: Data Preparation with mta_prep()
cat("Test 2: mta_prep()\n")
prepared_data <- mta_prep(renamed_data)
if ("touch_rank" %in% colnames(prepared_data)) {
  cat("  PASS: 'touch_rank' column created.\n")
} else {
  cat("  FAIL: 'touch_rank' column is missing.\n")
}
cat("  Preview of prepared data:\n")
print(prepared_data)
cat("\n")


### Test 3: Linear Attribution with mta_linear()
cat("Test 3: mta_linear()\n")
linear_result <- mta_linear(prepared_data)
cat("  Linear attribution result:\n")
print(linear_result)
cat("\n")


### Test 4: First Touch Attribution with mta_first()
cat("Test 4: mta_first()\n")
first_touch_result <- mta_first(prepared_data)
cat("  First touch attribution result:\n")
print(first_touch_result)
cat("\n")


### Test 5: Last Touch Attribution with mta_last()
cat("Test 5: mta_last()\n")
last_touch_result <- mta_last(prepared_data)
cat("  Last touch attribution result:\n")
print(last_touch_result)
cat("\n")


### Test 6: Position-Based Attribution with mta_position()
cat("Test 6: mta_position()\n")
position_result <- mta_position(prepared_data)
cat("  Position-based attribution result:\n")
print(position_result)
cat("\n")


### Test 7: Wrapper Analysis Function with mta_analysis()
cat("Test 7: mta_analysis() without progress bar\n")
analysis_result <- mta_analysis(prepared_data, progress_bar = FALSE)
cat("  Analysis result (models included):\n")
print(names(analysis_result))
cat("\n")

cat("Test 7b: mta_analysis() with progress bar\n")
analysis_result_pb <- mta_analysis(prepared_data, progress_bar = TRUE)
cat("  Analysis result (models included):\n")
print(names(analysis_result_pb))
cat("\n")


### Test 8: Combining Attribution Weights with mta_weights()
cat("Test 8: mta_weights()\n")
# Create a sample touchpoint-level summary data frame
touchpoint_summary <- data.frame(
  touchpoint = c("A", "B", "C"),
  some_stat = c(100, 200, 150),
  stringsAsFactors = FALSE
)
combined_weights <- mta_weights(touchpoint_summary, analysis_result)
cat("  Combined touchpoint weights:\n")
print(combined_weights)
cat("\n")


# ===============================
# UAT: Negative Tests (Edge Cases)
# ===============================

cat("\n===== UAT: Negative Tests =====\n\n")

### Negative Test 1: Empty Data Frame for mta_prep()
cat("Negative Test 1: mta_prep() with an empty data frame\n")
empty_data <- data.frame(
  customer_id = character(),
  touchpoint = character(),
  conversion = numeric(),
  timestamp = as.POSIXct(character())
)
empty_prep <- mta_prep(empty_data)
cat("  Processed empty data; number of rows:", nrow(empty_prep), "\n\n")


### Negative Test 2: Missing Required Column in mta_validate_data()
cat("Negative Test 2: mta_validate_data() with missing required column\n")
bad_data <- data.frame(
  customer_id = 1:5,
  touchpoint = sample(c("A", "B", "C"), 5, replace = TRUE),
  conversion = sample(c(0, 1), 5, replace = TRUE)
  # Missing 'timestamp'
)
tryCatch({
  mta_validate_data(bad_data)
}, error = function(e) {
  cat("  Expected error encountered: ", e$message, "\n")
})
cat("\n")


### Negative Test 3: Incorrect Column Names in mta_rename()
cat("Negative Test 3: mta_rename() with non-existent column name\n")
tryCatch({
  mta_rename(
    sample_data,
    current_names_or_positions = c("wrong_col", "tp", "conv", "ts"),
    new_names = c("customer_id", "touchpoint", "conversion", "timestamp")
  )
}, error = function(e) {
  cat("  Expected error encountered: ", e$message, "\n")
})
cat("\n")


cat("===== UAT Complete =====\n")
