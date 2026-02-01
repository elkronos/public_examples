# Daily interval counts and plotting utilities with testthat UAT
# =============================================================

# ------------------------------------------------------------------------------
# Function: daily_counts
# ------------------------------------------------------------------------------
#' Calculate daily counts from inclusive date intervals
#'
#' @param data A data.table or data.frame.
#' @param start_col Column name for interval start dates.
#' @param end_col Column name for interval end dates.
#' @param group_col Optional column name used to segment counts.
#' @param .debug Logical flag that prints intermediate diagnostics.
#'
#' @return A data.table with a daily date column named as `start_col` and count
#'   column `N`. If `group_col` is provided, a column of the same name is included.
daily_counts <- function(data, start_col, end_col, group_col = NULL, .debug = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
  
  dt <- data.table::as.data.table(data)
  
  # Column presence validation
  if (!(start_col %in% names(dt))) {
    stop("The start_col '", start_col, "' was not found in the data.")
  }
  if (!(end_col %in% names(dt))) {
    stop("The end_col '", end_col, "' was not found in the data.")
  }
  if (!is.null(group_col) && !(group_col %in% names(dt))) {
    stop("The group_col '", group_col, "' was not found in the data.")
  }
  
  # Input size validation
  if (nrow(dt) == 0L) {
    stop("Input data is empty.")
  }
  
  raw_start <- dt[[start_col]]
  raw_end   <- dt[[end_col]]
  
  # Fast Date representation for arithmetic and sequencing
  start_id <- data.table::as.IDate(raw_start)
  end_id   <- data.table::as.IDate(raw_end)
  
  # Conversion integrity for non-missing inputs
  if (any(!is.na(raw_start) & is.na(start_id))) {
    stop("Start dates contain values that cannot be converted to Date.")
  }
  if (any(!is.na(raw_end) & is.na(end_id))) {
    stop("End dates contain values that cannot be converted to Date.")
  }
  
  # Complete interval filtering
  keep <- !(is.na(start_id) | is.na(end_id))
  if (!any(keep)) {
    stop("No complete start/end pairs found.")
  }
  
  start_id <- start_id[keep]
  end_id   <- end_id[keep]
  
  # Interval ordering validation
  if (any(start_id > end_id, na.rm = TRUE)) {
    stop("Start dates must be less than or equal to end dates.")
  }
  
  cast_counts <- function(x) {
    mx <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.finite(mx) && mx <= .Machine$integer.max) as.integer(x) else as.numeric(x)
  }
  
  if (isTRUE(.debug)) {
    message("daily_counts diagnostics")
    message("  rows_in: ", nrow(dt))
    message("  rows_used: ", length(start_id))
    message("  start_class: ", paste(class(raw_start), collapse = "/"))
    message("  end_class: ", paste(class(raw_end), collapse = "/"))
  }
  
  build_counts_ungrouped <- function(s, e) {
    range_start <- min(s)
    range_end   <- max(e)
    
    days <- data.table::data.table(date = seq(range_start, range_end, by = 1L))
    
    endp1 <- e + 1L
    if (any(is.na(endp1) & !is.na(e))) {
      stop("End dates are out of supported range.")
    }
    
    # Event deltas for inclusive intervals: +1 at start, -1 at end + 1
    events <- data.table::rbindlist(list(
      data.table::data.table(date = s,     delta = 1L),
      data.table::data.table(date = endp1, delta = -1L)
    ))
    events <- events[, .(delta = sum(delta)), by = date]
    
    # Date join between the full day grid and event deltas
    days[events, delta := i.delta, on = "date"]
    days[is.na(delta), delta := 0L]
    
    counts_num <- cumsum(days$delta)
    days[, N := cast_counts(counts_num)]
    days[, delta := NULL]
    
    days[, date := as.Date(date)]
    data.table::setnames(days, "date", start_col)
    
    if (isTRUE(.debug)) {
      message("  ungrouped_range: ", as.character(min(days[[start_col]])), " .. ", as.character(max(days[[start_col]])))
      message("  N_type: ", paste(class(days$N), collapse = "/"))
    }
    
    days[]
  }
  
  build_counts_grouped <- function(s, e, g) {
    dti <- data.table::data.table(grp = g, start = s, end = e)
    
    ranges <- dti[, .(
      range_start = min(start),
      range_end   = max(end)
    ), by = grp]
    
    grid <- ranges[, .(date = seq(range_start, range_end, by = 1L)), by = grp]
    data.table::setorder(grid, grp, date)
    
    endp1 <- dti$end + 1L
    if (any(is.na(endp1) & !is.na(dti$end))) {
      stop("End dates are out of supported range.")
    }
    
    # Event deltas per group for inclusive intervals
    events <- data.table::rbindlist(list(
      dti[, .(grp, date = start, delta = 1L)],
      data.table::data.table(grp = dti$grp, date = endp1, delta = -1L)
    ))
    events <- events[, .(delta = sum(delta)), by = .(grp, date)]
    data.table::setorder(events, grp, date)
    
    # Group/date join between the per-group day grid and event deltas
    grid[events, delta := i.delta, on = c("grp", "date")]
    grid[is.na(delta), delta := 0L]
    
    grid[, N := cast_counts(cumsum(delta)), by = grp]
    grid[, delta := NULL]
    
    grid[, date := as.Date(date)]
    data.table::setnames(grid, c("grp", "date"), c(group_col, start_col))
    
    if (isTRUE(.debug)) {
      message("  groups_out: ", length(unique(grid[[group_col]])))
      message("  N_type: ", paste(class(grid$N), collapse = "/"))
    }
    
    grid[]
  }
  
  if (is.null(group_col)) {
    build_counts_ungrouped(start_id, end_id)
  } else {
    grp <- dt[[group_col]][keep]
    build_counts_grouped(start_id, end_id, grp)
  }
}

# ------------------------------------------------------------------------------
# Function: plot_daily_counts
# ------------------------------------------------------------------------------
#' Plot daily counts
#'
#' @param result A data.frame or data.table with a date column and `N`.
#' @param date_col Date column name in `result`.
#' @param group_col Optional grouping column name in `result`.
#' @param color Line/point color for ungrouped plots.
#' @param size Line width.
#' @param theme_style ggplot2 theme object.
#' @param .debug Logical flag that prints intermediate diagnostics.
#'
#' @return A ggplot object.
plot_daily_counts <- function(result, date_col, group_col = NULL,
                              color = "#0072B2FF",
                              size = 1.5,
                              theme_style = ggplot2::theme_minimal(),
                              .debug = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required.")
  }
  
  # Required column validation
  if (!(date_col %in% names(result))) {
    stop("The date_col '", date_col, "' does not exist in the result.")
  }
  if (!("N" %in% names(result))) {
    stop("The result must contain a column named 'N' representing counts.")
  }
  if (!is.null(group_col) && !(group_col %in% names(result))) {
    stop("The group_col '", group_col, "' does not exist in the result.")
  }
  
  # Date column normalization for plotting
  if (!inherits(result[[date_col]], "Date")) {
    result[[date_col]] <- as.Date(result[[date_col]])
  }
  
  if (isTRUE(.debug)) {
    message("plot_daily_counts diagnostics")
    message("  ggplot2_version: ", as.character(utils::packageVersion("ggplot2")))
    message("  date_type: ", paste(class(result[[date_col]]), collapse = "/"))
    message("  N_type: ", paste(class(result[["N"]]), collapse = "/"))
  }
  
  # Line width parameter selection based on ggplot2 version
  use_linewidth <- utils::packageVersion("ggplot2") >= "3.4.0"
  line_geom_args <- if (use_linewidth) list(linewidth = size) else list(size = size)
  
  x_sym <- rlang::sym(date_col)
  
  if (!is.null(group_col)) {
    g_sym <- rlang::sym(group_col)
    
    p <- ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = !!x_sym,
        y = rlang::.data[["N"]],
        group = !!g_sym,
        color = !!g_sym
      )
    ) +
      do.call(ggplot2::geom_line, line_geom_args) +
      ggplot2::geom_point(size = 3, shape = 21, fill = "white") +
      ggplot2::labs(
        title = "Daily Counts by Group",
        x = date_col,
        y = "Count",
        color = group_col
      ) +
      theme_style
  } else {
    p <- ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = !!x_sym,
        y = rlang::.data[["N"]]
      )
    ) +
      do.call(ggplot2::geom_line, c(list(color = color), line_geom_args)) +
      ggplot2::geom_point(color = color, size = 3, shape = 21, fill = "white") +
      ggplot2::labs(
        title = "Overall Daily Counts",
        x = date_col,
        y = "Count"
      ) +
      theme_style
  }
  
  p
}

# ------------------------------------------------------------------------------
# User Acceptance Testing (UAT) using testthat
# ------------------------------------------------------------------------------
if (requireNamespace("testthat", quietly = TRUE) && requireNamespace("data.table", quietly = TRUE)) {
  library(testthat)
  library(data.table)
  
  dt1 <- data.table(
    start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05")),
    end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08"))
  )
  
  dt2 <- data.table(
    start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05", "2023-01-03")),
    end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08", "2023-01-04")),
    group = c("A", "A", "B", "C")
  )
  
  dt3 <- data.table(
    start = as.Date(c("2023-01-01", "2023-01-02")),
    end   = as.Date(c("2023-01-03", "2023-01-04")),
    group = c("A", NA)
  )
  
  dt_bad <- data.table(
    start = as.Date("2023-01-10"),
    end   = as.Date("2023-01-05")
  )
  
  test_that("daily_counts without grouping works", {
    res <- daily_counts(dt1, "start", "end")
    expect_true(is.data.table(res))
    expect_true(all(c("start", "N") %in% names(res)))
    
    expected_dates <- seq(min(dt1$start), max(dt1$end), by = "day")
    expect_equal(res[["start"]], expected_dates)
    
    expected_counts <- c(1L, 2L, 2L, 3L, 3L, 2L, 1L)
    expect_equal(as.integer(res$N), expected_counts)
    
    expect_true(all(res$N >= 0))
  })
  
  test_that("daily_counts with grouping works", {
    res <- daily_counts(dt2, "start", "end", group_col = "group")
    expect_true(is.data.table(res))
    expect_true(all(c("start", "group", "N") %in% names(res)))
    
    expect_true(all(unique(dt2$group) %in% res$group))
    
    a <- res[group == "A"][order(start)]
    b <- res[group == "B"][order(start)]
    c <- res[group == "C"][order(start)]
    
    expect_equal(as.integer(a$N), c(1L, 2L, 2L, 2L, 2L, 1L))
    expect_equal(as.integer(b$N), c(1L, 1L, 1L, 1L))
    expect_equal(as.integer(c$N), c(1L, 1L))
  })
  
  test_that("daily_counts handles NA in grouping", {
    res <- daily_counts(dt3, "start", "end", group_col = "group")
    expect_true(is.data.table(res))
    expect_true("group" %in% names(res))
    expect_true(any(is.na(res$group)))
  })
  
  test_that("daily_counts errors when start > end", {
    expect_error(
      daily_counts(dt_bad, "start", "end"),
      "Start dates must be less than or equal to end dates"
    )
  })
  
  test_that("daily_counts errors when required column is missing", {
    expect_error(
      daily_counts(dt1, "nonexistent", "end"),
      "The start_col 'nonexistent' was not found"
    )
    expect_error(
      daily_counts(dt1, "start", "nonexistent"),
      "The end_col 'nonexistent' was not found"
    )
    expect_error(
      daily_counts(dt1, "start", "end", group_col = "nonexistent"),
      "The group_col 'nonexistent' was not found"
    )
  })
  
  test_that("daily_counts errors when data is empty", {
    dt_empty <- data.table(start = as.Date(character()), end = as.Date(character()))
    expect_error(
      daily_counts(dt_empty, "start", "end"),
      "Input data is empty"
    )
  })
  
  test_that("plot_daily_counts without grouping returns a ggplot object", {
    res <- daily_counts(dt1, "start", "end")
    p <- plot_daily_counts(res, date_col = "start")
    expect_true(inherits(p, "ggplot"))
  })
  
  test_that("plot_daily_counts with grouping returns a ggplot object", {
    res <- daily_counts(dt2, "start", "end", group_col = "group")
    p <- plot_daily_counts(res, date_col = "start", group_col = "group")
    expect_true(inherits(p, "ggplot"))
  })
  
  test_that("plot_daily_counts errors when date_col is missing", {
    dt_wrong <- copy(dt1)
    setnames(dt_wrong, "start", "wrong_date")
    expect_error(
      plot_daily_counts(dt_wrong, date_col = "start"),
      "The date_col 'start' does not exist"
    )
  })
  
  test_that("plot_daily_counts errors when group_col is missing", {
    res <- daily_counts(dt1, "start", "end")
    expect_error(
      plot_daily_counts(res, date_col = "start", group_col = "group"),
      "The group_col 'group' does not exist"
    )
  })
  
  print("All tests passed!")
}
