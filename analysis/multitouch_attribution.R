# ------------------------------------------------------------------------------
# Multitouch attribution utilities
# ------------------------------------------------------------------------------

#' Validate input data for multitouch attribution
#'
#' @param data A data.frame.
#' @param required_columns Character vector of required columns.
#' @return Invisibly TRUE when checks pass.
#' @export
mta_validate_data <- function(
    data,
    required_columns = c("customer_id", "touchpoint", "conversion", "timestamp")
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (anyNA(data$customer_id)) stop("`customer_id` contains NA.")
  if (anyNA(data$touchpoint)) stop("`touchpoint` contains NA.")
  if (anyNA(data$timestamp)) stop("`timestamp` contains NA.")
  
  if (!is.logical(data$conversion) && !is.numeric(data$conversion) && !is.integer(data$conversion)) {
    stop("`conversion` must be logical or numeric.")
  }
  
  ts_ok <- inherits(data$timestamp, "POSIXct") || inherits(data$timestamp, "POSIXt")
  if (!ts_ok) {
    ts_try <- suppressWarnings(as.POSIXct(data$timestamp, tz = "UTC"))
    if (anyNA(ts_try)) stop("`timestamp` must be convertible to POSIXct without NA.")
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.mta_to_posixct <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  out <- suppressWarnings(as.POSIXct(x, tz = tz))
  if (anyNA(out)) stop("`timestamp` conversion produced NA.")
  out
}

.mta_to_01_int <- function(x) {
  if (is.logical(x)) return(as.integer(!is.na(x) & x))
  if (is.numeric(x) || is.integer(x)) return(as.integer(!is.na(x) & x != 0))
  stop("`conversion` must be logical or numeric.")
}

.mta_is_prepared <- function(data) {
  isTRUE(attr(data, "mta_prepared")) &&
    all(c("touch_rank", "touch_n") %in% names(data))
}

.mta_normalize <- function(df, credit_col = "conversions") {
  total <- sum(df[[credit_col]], na.rm = TRUE)
  if (!is.finite(total) || total <= 0) {
    df$attribution_weight <- 0
    return(df)
  }
  df$attribution_weight <- df[[credit_col]] / total
  df
}

.mta_dt_available <- function() {
  requireNamespace("data.table", quietly = TRUE)
}

# ------------------------------------------------------------------------------
# Column rename utility
# ------------------------------------------------------------------------------

#' Rename columns for multitouch attribution
#'
#' @param data A data.frame.
#' @param current_names_or_positions Character (names) or numeric (positions).
#' @param new_names Character vector of new names.
#' @return A data.frame with renamed columns.
#' @export
mta_rename <- function(data, current_names_or_positions, new_names) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (length(current_names_or_positions) != length(new_names)) {
    stop("`current_names_or_positions` and `new_names` must have the same length.")
  }
  
  allowed_new_names <- c("customer_id", "touchpoint", "conversion", "timestamp")
  if (any(!new_names %in% allowed_new_names)) {
    stop("`new_names` must be drawn from: ", paste(allowed_new_names, collapse = ", "))
  }
  if (anyDuplicated(new_names)) stop("`new_names` must be unique.")
  
  old <- current_names_or_positions
  if (is.numeric(old)) {
    if (any(old < 1 | old > ncol(data))) stop("Column positions out of bounds.")
    old <- names(data)[old]
  }
  
  missing_old <- setdiff(old, names(data))
  if (length(missing_old) > 0) {
    stop("Columns not present in `data`: ", paste(missing_old, collapse = ", "))
  }
  
  out_names <- names(data)
  out_names[match(old, out_names)] <- new_names
  
  if (anyDuplicated(out_names)) stop("Renaming results in duplicate column names.")
  
  names(data) <- out_names
  data
}

# ------------------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------------------

#' Prepare data for attribution models
#'
#' Produces one conversion path per customer by trimming touchpoints to a conversion cutoff.
#'
#' @param data A data.frame with customer_id, touchpoint, conversion, timestamp.
#' @param conversion_event Conversion cutoff per customer: "first" or "last".
#' @param tz Timezone used when coercing timestamps.
#' @return A data.frame containing converting paths with touch_rank and touch_n.
#' @export
mta_prep <- function(data, conversion_event = c("first", "last"), tz = "UTC") {
  conversion_event <- match.arg(conversion_event)
  
  mta_validate_data(data)
  
  if (nrow(data) == 0) {
    out <- data
    attr(out, "mta_prepared") <- TRUE
    attr(out, "mta_total_conversions") <- 0L
    return(out)
  }
  
  data$customer_id <- as.character(data$customer_id)
  data$touchpoint  <- as.character(data$touchpoint)
  data$conversion  <- .mta_to_01_int(data$conversion)
  data$timestamp   <- .mta_to_posixct(data$timestamp, tz = tz)
  
  if (.mta_dt_available()) {
    dt <- data.table::as.data.table(data.table::copy(data))
    dt[, .row_id := .I]
    data.table::setorderv(dt, c("customer_id", "timestamp", ".row_id"))
    
    dt[, .conv_time := {
      if (!any(conversion == 1L)) as.POSIXct(NA, tz = tz)
      else if (conversion_event == "first") min(timestamp[conversion == 1L])
      else max(timestamp[conversion == 1L])
    }, by = "customer_id"]
    
    dt <- dt[!is.na(.conv_time) & timestamp <= .conv_time]
    dt[, touch_rank := seq_len(.N), by = "customer_id"]
    dt[, touch_n := .N, by = "customer_id"]
    
    out <- as.data.frame(dt[, c("customer_id", "touchpoint", "conversion", "timestamp", "touch_rank", "touch_n")])
    attr(out, "mta_prepared") <- TRUE
    attr(out, "mta_total_conversions") <- length(unique(out$customer_id))
    return(out)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package `dplyr` is required when `data.table` is not available.")
  }
  
  d <- dplyr::as_tibble(data)
  d$.row_id <- seq_len(nrow(d))
  d <- dplyr::arrange(d, customer_id, timestamp, .row_id)
  
  d <- dplyr::group_by(d, customer_id)
  d$.conv_time <- {
    has_conv <- any(d$conversion == 1L)
    if (!has_conv) as.POSIXct(NA, tz = tz)
    else if (conversion_event == "first") min(d$timestamp[d$conversion == 1L])
    else max(d$timestamp[d$conversion == 1L])
  }
  d <- dplyr::ungroup(d)
  
  d <- dplyr::filter(d, !is.na(.conv_time) & timestamp <= .conv_time)
  
  d <- dplyr::group_by(d, customer_id)
  d <- dplyr::mutate(d, touch_rank = dplyr::row_number(), touch_n = dplyr::n())
  d <- dplyr::ungroup(d)
  
  out <- dplyr::select(d, customer_id, touchpoint, conversion, timestamp, touch_rank, touch_n)
  out <- as.data.frame(out)
  attr(out, "mta_prepared") <- TRUE
  attr(out, "mta_total_conversions") <- length(unique(out$customer_id))
  out
}

# ------------------------------------------------------------------------------
# Attribution models
# ------------------------------------------------------------------------------

#' Linear attribution
#'
#' @param data Prepared data from mta_prep(), or raw data (auto-prep).
#' @return data.frame with touchpoint, conversions, attribution_weight.
#' @export
mta_linear <- function(data) {
  if (!.mta_is_prepared(data)) data <- mta_prep(data)
  
  if (nrow(data) == 0) {
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric()))
  }
  
  if (.mta_dt_available()) {
    dt <- data.table::as.data.table(data.table::copy(data))
    dt[, credit := 1 / touch_n]
    res <- dt[, .(conversions = sum(credit)), by = "touchpoint"]
    res <- as.data.frame(res)
    res <- .mta_normalize(res, "conversions")
    return(res)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package `dplyr` is required.")
  d <- dplyr::as_tibble(data)
  res <- dplyr::summarise(
    dplyr::group_by(d, touchpoint),
    conversions = sum(1 / touch_n),
    .groups = "drop"
  )
  res <- as.data.frame(res)
  .mta_normalize(res, "conversions")
}

#' First-touch attribution
#'
#' @param data Prepared data from mta_prep(), or raw data (auto-prep).
#' @return data.frame with touchpoint, conversions, attribution_weight.
#' @export
mta_first <- function(data) {
  if (!.mta_is_prepared(data)) data <- mta_prep(data)
  
  if (nrow(data) == 0) {
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric()))
  }
  
  if (.mta_dt_available()) {
    dt <- data.table::as.data.table(data.table::copy(data))
    res <- dt[touch_rank == 1L, .(conversions = .N), by = "touchpoint"]
    res <- as.data.frame(res)
    res <- .mta_normalize(res, "conversions")
    return(res)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package `dplyr` is required.")
  d <- dplyr::as_tibble(data)
  d <- dplyr::filter(d, touch_rank == 1L)
  res <- dplyr::summarise(dplyr::group_by(d, touchpoint), conversions = dplyr::n(), .groups = "drop")
  res <- as.data.frame(res)
  .mta_normalize(res, "conversions")
}

#' Last-touch attribution
#'
#' @param data Prepared data from mta_prep(), or raw data (auto-prep).
#' @return data.frame with touchpoint, conversions, attribution_weight.
#' @export
mta_last <- function(data) {
  if (!.mta_is_prepared(data)) data <- mta_prep(data)
  
  if (nrow(data) == 0) {
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric()))
  }
  
  if (.mta_dt_available()) {
    dt <- data.table::as.data.table(data.table::copy(data))
    res <- dt[touch_rank == touch_n, .(conversions = .N), by = "touchpoint"]
    res <- as.data.frame(res)
    res <- .mta_normalize(res, "conversions")
    return(res)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package `dplyr` is required.")
  d <- dplyr::as_tibble(data)
  d <- dplyr::filter(d, touch_rank == touch_n)
  res <- dplyr::summarise(dplyr::group_by(d, touchpoint), conversions = dplyr::n(), .groups = "drop")
  res <- as.data.frame(res)
  .mta_normalize(res, "conversions")
}

#' Position-based attribution
#'
#' @param data Prepared data from mta_prep(), or raw data (auto-prep).
#' @param first_touch_weight Weight on the first touchpoint.
#' @param last_touch_weight Weight on the last touchpoint.
#' @return data.frame with touchpoint, conversions, attribution_weight.
#' @export
mta_position <- function(data, first_touch_weight = 0.4, last_touch_weight = 0.4) {
  if (!is.numeric(first_touch_weight) || !is.numeric(last_touch_weight)) {
    stop("Position weights must be numeric.")
  }
  if (first_touch_weight < 0 || last_touch_weight < 0) stop("Position weights must be non-negative.")
  if (first_touch_weight + last_touch_weight > 1) stop("Position weights must sum to <= 1.")
  
  if (!.mta_is_prepared(data)) data <- mta_prep(data)
  
  if (nrow(data) == 0) {
    return(data.frame(touchpoint = character(), conversions = numeric(), attribution_weight = numeric()))
  }
  
  middle_weight <- 1 - first_touch_weight - last_touch_weight
  
  if (.mta_dt_available()) {
    dt <- data.table::as.data.table(data.table::copy(data))
    
    dt[, credit := {
      n0 <- touch_n[1L]
      
      if (n0 == 1L) {
        rep(1, .N)
      } else if (n0 == 2L) {
        data.table::fifelse(
          touch_rank == 1L,
          first_touch_weight + middle_weight / 2,
          last_touch_weight + middle_weight / 2
        )
      } else {
        data.table::fcase(
          touch_rank == 1L, first_touch_weight,
          touch_rank == n0, last_touch_weight,
          default = middle_weight / (n0 - 2L)
        )
      }
    }, by = "customer_id"]
    
    res <- dt[, .(conversions = sum(credit)), by = "touchpoint"]
    res <- as.data.frame(res)
    res <- .mta_normalize(res, "conversions")
    return(res)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package `dplyr` is required.")
  d <- dplyr::as_tibble(data)
  
  d <- dplyr::group_by(d, customer_id)
  d <- dplyr::mutate(
    d,
    conversions = dplyr::case_when(
      touch_n == 1L ~ 1,
      touch_n == 2L & touch_rank == 1L ~ first_touch_weight + middle_weight / 2,
      touch_n == 2L & touch_rank == 2L ~ last_touch_weight + middle_weight / 2,
      touch_rank == 1L ~ first_touch_weight,
      touch_rank == touch_n ~ last_touch_weight,
      TRUE ~ middle_weight / (touch_n - 2L)
    )
  )
  d <- dplyr::ungroup(d)
  
  res <- dplyr::summarise(dplyr::group_by(d, touchpoint), conversions = sum(conversions), .groups = "drop")
  res <- as.data.frame(res)
  .mta_normalize(res, "conversions")
}

# ------------------------------------------------------------------------------
# Analysis wrapper
# ------------------------------------------------------------------------------

#' Multitouch attribution analysis
#'
#' @param data Raw or prepared data.
#' @param models Character vector of model names.
#' @param progress_bar Logical for progress output.
#' @param conversion_event Conversion cutoff per customer: "first" or "last".
#' @param position_first_touch_weight First-touch weight for position-based.
#' @param position_last_touch_weight Last-touch weight for position-based.
#' @param tz Timezone used when coercing timestamps.
#' @return Named list of model result data.frames.
#' @export
mta_analysis <- function(
    data,
    models = c("linear", "first_touch", "last_touch", "position_based"),
    progress_bar = FALSE,
    conversion_event = c("first", "last"),
    position_first_touch_weight = 0.4,
    position_last_touch_weight = 0.4,
    tz = "UTC"
) {
  valid_models <- c("linear", "first_touch", "last_touch", "position_based")
  if (!is.character(models) || length(models) == 0) stop("`models` must be a non-empty character vector.")
  if (any(!models %in% valid_models)) stop("Invalid model(s): ", paste(setdiff(models, valid_models), collapse = ", "))
  if (!is.logical(progress_bar) || length(progress_bar) != 1) stop("`progress_bar` must be TRUE/FALSE.")
  
  conversion_event <- match.arg(conversion_event)
  
  prepared <- if (.mta_is_prepared(data)) data else mta_prep(data, conversion_event = conversion_event, tz = tz)
  
  if (progress_bar) {
    if (!requireNamespace("progress", quietly = TRUE)) stop("Package `progress` is required for progress output.")
    pb <- progress::progress_bar$new(
      total = length(models),
      format = "Calculating :current/:total [:bar] :percent ETA: :eta"
    )
  }
  
  out <- vector("list", length(models))
  names(out) <- models
  
  for (i in seq_along(models)) {
    m <- models[[i]]
    out[[i]] <- switch(
      m,
      linear = mta_linear(prepared),
      first_touch = mta_first(prepared),
      last_touch = mta_last(prepared),
      position_based = mta_position(
        prepared,
        first_touch_weight = position_first_touch_weight,
        last_touch_weight  = position_last_touch_weight
      )
    )
    if (progress_bar) pb$tick()
  }
  
  out
}

# ------------------------------------------------------------------------------
# Weight join utility
# ------------------------------------------------------------------------------

#' Join attribution outputs back to a touchpoint table
#'
#' @param data A data.frame containing a `touchpoint` column.
#' @param attribution_results Named list of model outputs from mta_analysis().
#' @param weight_prefix Column prefix for weight columns.
#' @param credit_prefix Column prefix for credit columns.
#' @return A data.frame augmented with per-model credit and weight columns.
#' @export
mta_weights <- function(
    data,
    attribution_results,
    weight_prefix = "weight_",
    credit_prefix = "credit_"
) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!("touchpoint" %in% names(data))) stop("`data` must contain `touchpoint`.")
  if (!is.list(attribution_results) || length(attribution_results) == 0) {
    stop("`attribution_results` must be a non-empty list.")
  }
  
  out <- data
  out$touchpoint <- as.character(out$touchpoint)
  
  for (model_name in names(attribution_results)) {
    model_df <- attribution_results[[model_name]]
    if (!is.data.frame(model_df)) stop("Each model result must be a data.frame.")
    if (!all(c("touchpoint", "attribution_weight") %in% names(model_df))) {
      stop("Each model result must contain `touchpoint` and `attribution_weight`.")
    }
    
    credit_col <- if ("conversions" %in% names(model_df)) "conversions" else NULL
    
    add <- model_df[, c("touchpoint", credit_col, "attribution_weight"), drop = FALSE]
    add$touchpoint <- as.character(add$touchpoint)
    
    w_name <- paste0(weight_prefix, model_name)
    names(add)[names(add) == "attribution_weight"] <- w_name
    
    if (!is.null(credit_col)) {
      c_name <- paste0(credit_prefix, model_name)
      names(add)[names(add) == credit_col] <- c_name
    }
    
    out <- merge(out, add, by = "touchpoint", all.x = TRUE, sort = FALSE)
    
    out[[w_name]] <- ifelse(is.na(out[[w_name]]), 0, out[[w_name]])
    if (!is.null(credit_col)) {
      c_name <- paste0(credit_prefix, model_name)
      out[[c_name]] <- ifelse(is.na(out[[c_name]]), 0, out[[c_name]])
    }
  }
  
  out
}

# ------------------------------------------------------------------------------
# UAT script
# ------------------------------------------------------------------------------

# Sample data generator
mta_uat_data <- function(seed = 1L) {
  set.seed(seed)
  
  customers <- sprintf("C%03d", 1:200)
  touchpoints <- c("Search", "Social", "Email", "Display", "Affiliate")
  
  n_touches <- sample(1:10, length(customers), replace = TRUE)
  
  df <- do.call(
    rbind,
    lapply(seq_along(customers), function(i) {
      n <- n_touches[[i]]
      ts0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + sample(0:(60 * 60 * 24 * 20), 1)
      ts <- ts0 + sort(sample(0:(60 * 60 * 24 * 10), n, replace = FALSE))
      tp <- sample(touchpoints, n, replace = TRUE)
      
      conv_flag <- rbinom(1, 1, 0.25)
      conversion <- integer(n)
      if (conv_flag == 1L) conversion[n] <- 1L
      
      data.frame(
        customer_id = customers[[i]],
        touchpoint = tp,
        conversion = conversion,
        timestamp = ts,
        stringsAsFactors = FALSE
      )
    })
  )
  
  df
}

# UAT runner
mta_uat_run <- function() {
  cat("\n===== UAT: Multitouch Attribution =====\n\n")
  
  df <- mta_uat_data()
  
  cat("Rows:", nrow(df), "\n")
  cat("Customers:", length(unique(df$customer_id)), "\n\n")
  
  prep <- mta_prep(df, conversion_event = "first")
  cat("Prepared rows:", nrow(prep), "\n")
  cat("Prepared customers:", length(unique(prep$customer_id)), "\n\n")
  
  res <- mta_analysis(
    prep,
    progress_bar = FALSE,
    position_first_touch_weight = 0.4,
    position_last_touch_weight = 0.4
  )
  
  cat("Models:\n")
  print(names(res))
  cat("\n")
  
  cat("Linear:\n")
  print(res$linear[order(-res$linear$attribution_weight), ])
  cat("\n")
  
  cat("First-touch:\n")
  print(res$first_touch[order(-res$first_touch$attribution_weight), ])
  cat("\n")
  
  cat("Last-touch:\n")
  print(res$last_touch[order(-res$last_touch$attribution_weight), ])
  cat("\n")
  
  cat("Position-based:\n")
  print(res$position_based[order(-res$position_based$attribution_weight), ])
  cat("\n")
  
  tp_summary <- data.frame(
    touchpoint = sort(unique(df$touchpoint)),
    some_stat = sample(100:500, length(unique(df$touchpoint))),
    stringsAsFactors = FALSE
  )
  
  joined <- mta_weights(tp_summary, res)
  
  cat("Joined weights:\n")
  print(joined)
  cat("\n")
  
  invisible(list(prepared = prep, results = res, joined = joined))
}

# Example invocation:
# mta_uat_run()
