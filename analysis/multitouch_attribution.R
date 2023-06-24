#' Rename columns of a data frame
#'
#' This function renames the columns of a given data frame based on the provided mapping between
#' current names or positions and new names. New column names should include 'customer_id', 'touchpoint',
#' 'conversion', and 'timestamp' only.
#' 
#' This function was designed to more readily rename variables to work with the multitouch_attribution() function.
#'
#' @param data A data frame whose columns need to be renamed.
#' @param current_names_or_positions A character vector or numeric vector indicating the current column names
#'   or positions that need to be renamed.
#' @param new_names A character vector indicating the new column names.
#'
#' @importFrom dplyr rename_with
#' @importFrom tidyselect all_of
#'
#' @return A data frame with the specified columns renamed.
#' @examples
#' \dontrun{
#' set.seed(123)
#' original_data <- data.frame(
#'   cust_id = 1:3,
#'   tp = c("A", "B", "C"),
#'   conv = c(0, 1, 0),
#'   ts = as.POSIXct(Sys.Date() - c(0, 1, 2))
#' )
#'
#' # Rename using variable names
#' renamed_data <- mta_rename(
#'   original_data,
#'   current_names_or_positions = c("cust_id", "tp", "conv", "ts"),
#'   new_names = c("customer_id", "touchpoint", "conversion", "timestamp")
#' )
#' 
#' # Rename columns by column positions
#' renamed_data_by_position <- mta_rename(
#'   original_data,
#'   current_names_or_positions = c(1, 2, 3, 4),
#'   new_names = c("customer_id", "touchpoint", "conversion", "timestamp")
#' )
#' }
#' @export

#  Load packages
library(dplyr)
library(tidyselect)
library(progress)
library(assertthat)

# Rename columns
mta_rename <- function(data, current_names_or_positions, new_names) {
  if (length(current_names_or_positions) != length(new_names)) {
    stop("The lengths of 'current_names_or_positions' and 'new_names' must be equal.")
  }
  
  if (any(!new_names %in% c("customer_id", "touchpoint", "conversion", "timestamp"))) {
    stop("New column names should only include 'customer_id', 'touchpoint', 'conversion', and 'timestamp'.")
  }
  
  renaming_map <- setNames(new_names, current_names_or_positions)
  
  data_renamed <- data %>%
    rename_with(~ renaming_map[.], all_of(current_names_or_positions))
  
  return(data_renamed)
}


#' Prepare data for attribution models
#'
#' This function prepares the input data for attribution models by ensuring it has the required columns
#' and adds a touch_rank column. The touch_rank column represents the touchpoint rank in the customer journey.
#'
#' @param data A data frame containing 'customer_id', 'touchpoint', 'conversion', and 'timestamp' columns.
#'
#' @importFrom dplyr arrange group_by mutate ungroup row_number
#'
#' @return A data frame with an additional 'touch_rank' column.
#' @examples
#' \dontrun{
#' set.seed(123)
#' original_data <- data.frame(
#'   customer_id = 1:3,
#'   touchpoint = c("A", "B", "C"),
#'   conversion = c(0, 1, 0),
#'   timestamp = as.POSIXct(Sys.Date() - c(0, 1, 2))
#' )
#'
#' prepared_data <- mta_prep(original_data)
#' }
#' @export

# Prepare data for attribution models
mta_prep <- function(data) {
  if (!all(c("customer_id", "touchpoint", "conversion", "timestamp") %in% colnames(data))) {
    stop("The input data frame should have 'customer_id', 'touchpoint', 'conversion', and 'timestamp' columns.")
  }
  
  if (!is.data.frame(data)) {
    stop("The input data should be a data.frame.")
  }
  
  data %>%
    arrange(customer_id, timestamp) %>%
    group_by(customer_id) %>%
    mutate(touch_rank = row_number()) %>%
    ungroup()
}


#' Linear attribution model
#'
#' This function calculates the linear attribution weights for each touchpoint in the input data.
#' Linear attribution assigns equal credit to all touchpoints in the customer journey.
#'
#' @param data A prepared data frame with 'customer_id', 'touchpoint', 'conversion', 'timestamp',
#'   and 'touch_rank' columns.
#'
#' @importFrom dplyr group_by summarise mutate
#'
#' @return A data frame with 'touchpoint', 'conversions', and 'attribution_weight' columns.
#' @examples
#' \dontrun{
#' set.seed(123)
#' 
#' original_data <- data.frame(
#'   customer_id = 1:3,
#'   touchpoint = c("A", "B", "C"),
#'   conversion = c(0, 1, 0),
#'   timestamp = as.POSIXct(Sys.Date() - c(0, 1, 2))
#' )
#'
#' prepared_data <- mta_prep(original_data)
#' linear_attrib_weights <- mta_linear(prepared_data)
#' }
#' @export

# Linear attribution model
mta_linear <- function(data) {
  attribution_weights <- data %>%
    group_by(touchpoint) %>%
    summarise(conversions = sum(conversion)) %>%
    mutate(attribution_weight = conversions / sum(conversions))
  return(attribution_weights)
}


#' First touch attribution model
#'
#' This function calculates the first touch attribution weights for each touchpoint in the input data.
#' First touch attribution assigns full credit to the first touchpoint in the customer journey.
#'
#' @param data A prepared data frame with 'customer_id', 'touchpoint', 'conversion', 'timestamp',
#'   and 'touch_rank' columns.
#'
#' @importFrom dplyr filter group_by summarise mutate
#'
#' @return A data frame with 'touchpoint', 'conversions', and 'attribution_weight' columns.
#' @examples
#' \dontrun{
#' set.seed(123)
#' original_data <- data.frame(
#'   customer_id = 1:3,
#'   touchpoint = c("A", "B", "C"),
#'   conversion = c(0, 1, 0),
#'   timestamp = as.POSIXct(Sys.Date() - c(0, 1, 2))
#' )
#'
#' prepared_data <- mta_prep(original_data)
#' first_touch_attrib_weights <- mta_first(prepared_data)
#' }
#' @export

# First touch attribution model
mta_first <- function(data) {
  first_touch <- data %>%
    filter(touch_rank == 1) %>%
    group_by(touchpoint) %>%
    summarise(conversions = sum(conversion))
  attribution_weights <- first_touch %>%
    mutate(attribution_weight = conversions / sum(conversions))
  return(attribution_weights)
}


#' Last touch attribution model
#'
#' This function calculates the last touch attribution weights for each touchpoint in the input data.
#' Last touch attribution assigns full credit to the last touchpoint in the customer journey that
#' resulted in a conversion.
#'
#' @param data A prepared data frame with 'customer_id', 'touchpoint', 'conversion', 'timestamp',
#'   and 'touch_rank' columns.
#'
#' @importFrom dplyr filter group_by arrange desc ungroup slice summarise mutate
#'
#' @return A data frame with 'touchpoint', 'conversions', and 'attribution_weight' columns.
#' @examples
#' \dontrun{
#' set.seed(123)
#' original_data <- data.frame(
#'   customer_id = 1:3,
#'   touchpoint = c("A", "B", "C"),
#'   conversion = c(0, 1, 0),
#'   timestamp = as.POSIXct(Sys.Date() - c(0, 1, 2))
#' )
#'
#' prepared_data <- mta_prep(original_data)
#' last_touch_attrib_weights <- mta_last(prepared_data)
#' }
#' @export

# Last touch attribution model
mta_last <- function(data) {
  last_touch <- data %>%
    filter(conversion == 1) %>%
    group_by(customer_id) %>%
    arrange(desc(timestamp)) %>%
    slice(1) %>%
    ungroup()
  attribution_weights <- last_touch %>%
    group_by(touchpoint) %>%
    summarise(conversions = n()) %>%
    mutate(attribution_weight = conversions / sum(conversions))
  return(attribution_weights)
}


#' Position-based attribution model
#'
#' This function calculates the attribution weights of each touchpoint in a position-based attribution model.
#' 
#' @param data A data.frame with columns "customer_id", "touchpoint", "conversion", and "touch_rank".
#' @param first_touch_weight The weight to assign to the first touchpoint in the attribution model. Default is 0.4.
#' @param last_touch_weight The weight to assign to the last touchpoint in the attribution model. Default is 0.4.
#' 
#' @return A data.frame with columns "touchpoint", "conversion_weight", and "attribution_weight".
#' 
#' @examples
#' data <- data.frame(customer_id = c(1, 1, 1), touchpoint = c("A", "B", "C"), conversion = c(0, 0, 1), touch_rank = c(1, 2, 3))
#' mta_position(data)
#' 
#' @export

# Position-based attribution model
mta_position <- function(data, first_touch_weight = 0.4, last_touch_weight = 0.4) {
  if (first_touch_weight + last_touch_weight > 1) {
    stop("The sum of first_touch_weight and last_touch_weight should not exceed 1.")
  }
  
  middle_touch_weight <- 1 - first_touch_weight - last_touch_weight
  data <- data %>%
    group_by(customer_id) %>%
    mutate(
      total_touchpoints = n(),
      position_weight = case_when(
        touch_rank == 1 ~ first_touch_weight,
        conversion == 1 ~ last_touch_weight,
        total_touchpoints == 2 ~ 0,
        TRUE ~ middle_touch_weight / (total_touchpoints - 2)
      )
    ) %>%
    ungroup()
  attribution_weights <- data %>%
    group_by(touchpoint) %>%
    summarise(conversion_weight = sum(conversion * position_weight)) %>%
    mutate(attribution_weight = conversion_weight / sum(conversion_weight))
  return(attribution_weights)
}


#' Wrapper function for multitouch attribution
#'
#' This function calculates the attribution weights for multiple attribution models
#' provided in the 'models' parameter. The results for each model are returned in a list.
#'
#' @param data A data frame with 'customer_id', 'touchpoint', 'conversion', and 'timestamp' columns.
#' @param models A character vector specifying the attribution models to calculate
#'   (default: c("linear", "first_touch", "last_touch", "position_based")).
#' @param progress_bar A logical value indicating whether to display a progress bar (default: FALSE).
#'
#' @importFrom dplyr mutate group_by summarise filter arrange
#' @importFrom progress progress_bar
#' @importFrom assertthat assert_that
#'
#' @return A list with the results for each attribution model specified in the 'models' parameter.
#' @examples
#' \dontrun{
#' original_data <- data.frame(
#'   customer_id = 1:3,
#'   touchpoint = c("A", "B", "C"),
#'   conversion = c(0, 1, 0),
#'   timestamp = as.POSIXct(Sys.Date() - c(0, 1, 2))
#' )
#'
#' prepared_data <- mta_prep(original_data)
#' attribution_results <- mta_analysis(prepared_data, progress_bar = TRUE)
#' }
#' @export

# Wrapper function for multitouch attribution
mta_analysis <- function(data, models = c("linear", "first_touch", "last_touch", "position_based"), progress_bar = FALSE) {
  # Validate the input data
  assert_that(
    is.data.frame(data),
    msg = "The input data should be a data.frame."
  )
  
  required_columns <- c("customer_id", "touchpoint", "conversion", "timestamp")
  assert_that(
    all(required_columns %in% colnames(data)),
    msg = "The input data frame should have 'customer_id', 'touchpoint', 'conversion', and 'timestamp' columns."
  )
  
  # Validate the models parameter
  valid_models <- c("linear", "first_touch", "last_touch", "position_based")
  assert_that(
    is.character(models),
    all(models %in% valid_models),
    msg = "Invalid model(s) provided. Choose from 'linear', 'first_touch', 'last_touch', and 'position_based'."
  )
  
  # Validate the progress_bar parameter
  assert_that(
    is.logical(progress_bar),
    msg = "The 'progress_bar' parameter should be logical (TRUE or FALSE)."
  )
  
  if (any(!models %in% c("linear", "first_touch", "last_touch", "position_based"))) {
    stop("Invalid model(s) provided. Choose from 'linear', 'first_touch', 'last_touch', and 'position_based'.")
  }
  
  data <- mta_prep(data)
  
  attribution_results <- list()
  
  if (progress_bar) {
    pb <- progress::progress_bar$new(total = length(models), format = "Calculating :current/:total [:bar] :percent ETA: :eta")
  }
  
  for (model in models) {
    if (model == "linear") {
      attribution_results[["linear"]] <- mta_linear(data)
    } else if (model == "first_touch") {
      attribution_results[["first_touch"]] <- mta_first(data)
    } else if (model == "last_touch") {
      attribution_results[["last_touch"]] <- mta_last(data)
    } else if (model == "position_based") {
      attribution_results[["position_based"]] <- mta_position(data)
    }
    
    if (progress_bar) {
      pb$tick()
    }
  }
  
  return(attribution_results)
}


#' Calculate touchpoint weights based on attribution model results
#'
#' @param data A data frame containing the touchpoint data
#' @param attribution_results A list of attribution model results
#' 
#' @return A data frame with touchpoint weights calculated from attribution model results
#' 
#' @examples
#' data <- tibble(touchpoint = c("A", "B", "C"), conversions = c(10, 20, 30))
#' attribution_results <- list(model1 = tibble(touchpoint = c("A", "B"), attribution_weight = c(0.3, 0.7)),
#'                              model2 = tibble(touchpoint = c("B", "C"), attribution_weight = c(0.4, 0.6)))
#' mta_weights(data, attribution_results)
#'
#' @import tibble dplyr
#' @export

# Save function to grab weights
mta_weights <- function(data, attribution_results) {
  data_with_weights <- data
  
  for (model_name in names(attribution_results)) {
    # Ensure each attribution model has the same number of touchpoints
    missing_touchpoints <- setdiff(unique(data$touchpoint), attribution_results[[model_name]]$touchpoint)
    for (missing_touchpoint in missing_touchpoints) {
      row_to_add <- tibble(touchpoint = missing_touchpoint, conversions = 0, attribution_weight = 0)
      attribution_results[[model_name]] <- attribution_results[[model_name]] %>%
        bind_rows(row_to_add) %>%
        arrange(touchpoint)
    }
    
    # Join data with weights from the available attribution models
    suffix <- paste0("_", model_name)
    data_with_weights <- data_with_weights %>%
      left_join(attribution_results[[model_name]], by = "touchpoint", suffix = c("", suffix))
  }
  
  return(data_with_weights)
}