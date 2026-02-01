# Daily Summary Module
suppressPackageStartupMessages({
  library(data.table)
  library(testthat)
  library(ggplot2)
})

#' Column Assertions
#'
#' Validates that required columns exist on a data.table.
assert_has_cols <- function(dt, cols) {
  missing <- setdiff(cols, names(dt))
  if (length(missing) > 0L) {
    stop(sprintf("Missing column(s): %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Date Range Assertions
#'
#' Validates start/end date vectors for missing values and ordering.
assert_valid_ranges <- function(start, end) {
  if (length(start) != length(end)) stop("Start/end length mismatch.", call. = FALSE)
  if (anyNA(start) || anyNA(end)) stop("NA present in start or end dates.", call. = FALSE)
  if (any(start > end)) stop("Start date later than end date detected.", call. = FALSE)
  invisible(TRUE)
}

#' Summary Function Resolver
#'
#' Normalizes summary specification to a scalar-returning function.
resolve_summary_fun <- function(summary_func) {
  if (is.character(summary_func) && length(summary_func) == 1L) {
    allowed <- c("sum", "mean", "median")
    if (!summary_func %chin% allowed) {
      stop("summary_func must be 'sum', 'mean', 'median', or a function.", call. = FALSE)
    }
    if (summary_func == "sum")    return(function(x) sum(x, na.rm = TRUE))
    if (summary_func == "mean")   return(function(x) mean(x, na.rm = TRUE))
    if (summary_func == "median") return(function(x) median(x, na.rm = TRUE))
  }
  
  if (is.function(summary_func)) {
    f <- summary_func
    return(function(x) {
      y <- f(x)
      if (!is.numeric(y) || length(y) != 1L) {
        stop("Custom summary function must return a single numeric value.", call. = FALSE)
      }
      y
    })
  }
  
  stop("summary_func must be 'sum', 'mean', 'median', or a function.", call. = FALSE)
}

#' Weekday Code Parser
#'
#' Converts weekday labels to POSIXlt wday codes (0=Sunday ... 6=Saturday).
as_wday_codes <- function(x) {
  if (is.null(x)) return(NULL)
  
  if (is.numeric(x)) {
    x <- as.integer(x)
    if (anyNA(x) || any(x < 0L | x > 6L)) stop("weekdays must be in 0:6.", call. = FALSE)
    return(unique(x))
  }
  
  if (!is.character(x)) stop("weekdays must be numeric (0:6) or character labels.", call. = FALSE)
  
  map <- c(
    sun = 0L, sunday = 0L,
    mon = 1L, monday = 1L,
    tue = 2L, tues = 2L, tuesday = 2L,
    wed = 3L, weds = 3L, wednesday = 3L,
    thu = 4L, thur = 4L, thurs = 4L, thursday = 4L,
    fri = 5L, friday = 5L,
    sat = 6L, saturday = 6L
  )
  
  key <- tolower(trimws(x))
  if (!all(key %chin% names(map))) {
    bad <- key[!key %chin% names(map)]
    stop(sprintf("Unrecognized weekday label(s): %s", paste(unique(bad), collapse = ", ")), call. = FALSE)
  }
  
  unique(unname(map[key]))
}

#' Accrual Calendar Builder
#'
#' Builds the set of dates that receive accrual.
filter_dates <- function(dates,
                         skip_weekends = FALSE,
                         weekdays = NULL,
                         exclude_dates = NULL,
                         exclude_ranges = NULL) {
  d <- as.IDate(dates)
  if (length(d) == 0L) return(d)
  
  wday <- as.POSIXlt(as.Date(d))$wday
  
  if (!is.null(weekdays)) {
    keep_wd <- as_wday_codes(weekdays)
    d <- d[wday %in% keep_wd]
  } else if (isTRUE(skip_weekends)) {
    d <- d[!(wday %in% c(0L, 6L))]
  }
  
  if (!is.null(exclude_dates)) {
    ex <- as.IDate(exclude_dates)
    d <- d[!(d %in% ex)]
  }
  
  if (!is.null(exclude_ranges)) {
    r <- as.data.table(exclude_ranges)
    assert_has_cols(r, c("start", "end"))
    r[, `:=`(start = as.IDate(start), end = as.IDate(end))]
    assert_valid_ranges(r$start, r$end)
    
    dr <- data.table(rid = seq_along(d), start = d, end = d)
    setkey(dr, start, end)
    setkey(r, start, end)
    
    hit <- foverlaps(dr, r, nomatch = 0L)
    if (nrow(hit) > 0L) d <- d[!hit$rid]
  }
  
  sort(unique(d))
}

#' Position Grid Builder
#'
#' Builds the output grid over positions in the accrual calendar.
build_pos_grid <- function(dcal, groups = NULL, group_col = NULL) {
  m <- length(dcal)
  if (m == 0L) {
    if (is.null(group_col)) return(data.table(Date = as.Date(character()), pos = integer(), N = numeric()))
    return(data.table(Date = as.Date(character()), pos = integer(), N = numeric(), tmp = groups)[0][, (group_col) := tmp][, tmp := NULL])
  }
  
  if (is.null(group_col)) {
    return(data.table(pos = seq_len(m), Date = as.Date(dcal)))
  }
  
  grid <- CJ(tmp = groups, pos = seq_len(m), sorted = TRUE)
  grid[, Date := as.Date(dcal[pos])]
  setnames(grid, "tmp", group_col)
  setcolorder(grid, c(group_col, "Date", "pos"))
  grid
}

#' Calendar Index Mapping
#'
#' Maps start/end ranges to index ranges on the accrual calendar.
calendar_indices <- function(start, end, dcal) {
  m <- length(dcal)
  if (m == 0L) return(list(start_idx = integer(length(start)), end_idx = integer(length(end))))
  
  sidx <- findInterval(start - 1L, dcal) + 1L
  eidx <- findInterval(end, dcal)
  
  list(start_idx = sidx, end_idx = eidx)
}

#' Calendar Range Expansion
#'
#' Expands calendar index ranges into one row per calendar date.
expand_calendar_ranges <- function(start_idx, end_idx, dcal, value, group = NULL, group_col = NULL, max_expansion_rows = 5e7) {
  keep <- (start_idx <= end_idx) & (end_idx >= 1L) & (start_idx <= length(dcal))
  if (!any(keep)) {
    out <- data.table(Date = as.Date(character()), Value = value[0])
    if (!is.null(group_col)) out[, (group_col) := group[0]]
    return(out)
  }
  
  s <- pmax.int(start_idx[keep], 1L)
  e <- pmin.int(end_idx[keep], length(dcal))
  len <- as.integer(e - s + 1L)
  
  total <- sum(len)
  if (!is.finite(total) || total < 0L) stop("Invalid calendar range lengths.", call. = FALSE)
  if (total > max_expansion_rows) {
    stop(sprintf("Expansion would create %s rows; raise max_expansion_rows to proceed.",
                 format(total, big.mark = ",")),
         call. = FALSE)
  }
  
  idx <- rep.int(which(keep), len)
  off <- sequence(len) - 1L
  pos <- s[match(idx, which(keep))] + off
  
  out <- data.table(Date = as.Date(dcal[pos]), Value = value[idx])
  
  if (!is.null(group_col)) {
    out[, (group_col) := group[idx]]
    setcolorder(out, c(group_col, "Date", "Value"))
  }
  
  out
}

#' Daily Summary
#'
#' Computes daily summaries with accrual constrained to a user-defined calendar.
daily_summary <- function(data,
                          start_col,
                          end_col,
                          value_col,
                          group_col = NULL,
                          summary_func = "sum",
                          fill_value = NA_real_,
                          max_expansion_rows = 5e7,
                          date_start = NULL,
                          date_end = NULL,
                          skip_weekends = FALSE,
                          weekdays = NULL,
                          exclude_dates = NULL,
                          exclude_ranges = NULL) {
  if (!is.data.table(data)) stop("Input data must be a data.table.", call. = FALSE)
  
  req <- c(start_col, end_col, value_col)
  assert_has_cols(data, req)
  if (!is.null(group_col)) assert_has_cols(data, group_col)
  
  start <- as.IDate(data[[start_col]])
  end   <- as.IDate(data[[end_col]])
  assert_valid_ranges(start, end)
  
  if (nrow(data) == 0L) {
    out <- data.table(Date = as.Date(character()), N = numeric())
    if (!is.null(group_col)) out[, (group_col) := data[[group_col]][0]]
    return(out)
  }
  
  data_min <- min(start)
  data_max <- max(end)
  
  run_start <- if (is.null(date_start)) data_min else as.IDate(date_start)
  run_end   <- if (is.null(date_end))   data_max else as.IDate(date_end)
  if (anyNA(c(run_start, run_end)) || run_start > run_end) stop("Invalid date_start/date_end window.", call. = FALSE)
  
  full_window <- seq(run_start, run_end, by = 1L)
  dcal <- filter_dates(full_window,
                       skip_weekends = skip_weekends,
                       weekdays = weekdays,
                       exclude_dates = exclude_dates,
                       exclude_ranges = exclude_ranges)
  
  groups <- NULL
  group_vec <- NULL
  if (!is.null(group_col)) {
    group_vec <- data[[group_col]]
    groups <- unique(group_vec)
  }
  
  if (length(dcal) == 0L) {
    out <- build_pos_grid(dcal, groups, group_col)
    out[, `:=`(N = numeric())]
    out[, pos := NULL]
    return(out[])
  }
  
  summary_fun <- resolve_summary_fun(summary_func)
  idxs <- calendar_indices(start, end, dcal)
  start_idx <- idxs$start_idx
  end_idx <- idxs$end_idx
  
  if (is.character(summary_func) && summary_func %chin% c("sum", "mean")) {
    value <- data[[value_col]]
    if (!is.numeric(value)) stop("Value column must be numeric for 'sum'/'mean'.", call. = FALSE)
    
    keep <- (start_idx <= end_idx) & (end_idx >= 1L) & (start_idx <= length(dcal)) & !is.na(value)
    if (!any(keep)) {
      out <- build_pos_grid(dcal, groups, group_col)
      out[, N := NA_real_]
      if (!is.na(fill_value)) out[is.na(N), N := as.numeric(fill_value)]
      out[, pos := NULL]
      return(out[])
    }
    
    s <- pmax.int(start_idx[keep], 1L)
    e <- pmin.int(end_idx[keep], length(dcal))
    pos_end <- e + 1L
    
    if (is.null(group_col)) {
      ev_sum <- data.table(pos = c(s, pos_end), delta_sum = c(value[keep], -value[keep]))
      ev_cnt <- data.table(pos = c(s, pos_end), delta_cnt = c(rep.int(1.0, sum(keep)), rep.int(-1.0, sum(keep))))
    } else {
      gk <- group_vec[keep]
      ev_sum <- data.table(tmp = c(gk, gk), pos = c(s, pos_end), delta_sum = c(value[keep], -value[keep]))
      ev_cnt <- data.table(tmp = c(gk, gk), pos = c(s, pos_end), delta_cnt = c(rep.int(1.0, sum(keep)), rep.int(-1.0, sum(keep))))
      setnames(ev_sum, "tmp", group_col)
      setnames(ev_cnt, "tmp", group_col)
    }
    
    ev_sum <- ev_sum[pos >= 1L & pos <= length(dcal)]
    ev_cnt <- ev_cnt[pos >= 1L & pos <= length(dcal)]
    
    keys <- if (is.null(group_col)) "pos" else c(group_col, "pos")
    
    ev_sum <- ev_sum[, .(delta_sum = sum(delta_sum, na.rm = TRUE)), by = keys]
    ev_cnt <- ev_cnt[, .(delta_cnt = sum(delta_cnt, na.rm = TRUE)), by = keys]
    
    grid <- build_pos_grid(dcal, groups, group_col)
    
    out <- ev_sum[grid, on = keys]
    out[is.na(delta_sum), delta_sum := 0]
    
    out <- ev_cnt[out, on = keys]
    out[is.na(delta_cnt), delta_cnt := 0]
    
    if (is.null(group_col)) {
      out[, `:=`(sum_run = cumsum(delta_sum), cnt_run = cumsum(delta_cnt))]
    } else {
      out[, `:=`(sum_run = cumsum(delta_sum), cnt_run = cumsum(delta_cnt)), by = group_col]
    }
    
    if (summary_func == "sum") {
      out[, N := fifelse(cnt_run == 0, NA_real_, sum_run)]
    } else {
      out[, N := fifelse(cnt_run == 0, NA_real_, sum_run / cnt_run)]
    }
    
    if (!is.na(fill_value)) out[is.na(N) | is.nan(N), N := as.numeric(fill_value)]
    
    out[, c("pos", "delta_sum", "delta_cnt", "sum_run", "cnt_run") := NULL]
    if (is.null(group_col)) setorderv(out, "Date") else setorderv(out, c(group_col, "Date"))
    return(out[])
  }
  
  expanded <- expand_calendar_ranges(
    start_idx = start_idx,
    end_idx = end_idx,
    dcal = dcal,
    value = data[[value_col]],
    group = group_vec,
    group_col = group_col,
    max_expansion_rows = max_expansion_rows
  )
  
  keys <- if (is.null(group_col)) "Date" else c(group_col, "Date")
  agg <- expanded[, .(N = {
    x <- Value
    if (length(x) == 0L || all(is.na(x))) NA_real_ else summary_fun(x)
  }), by = keys]
  
  grid_out <- build_pos_grid(dcal, groups, group_col)
  grid_out[, pos := NULL]
  
  out <- agg[grid_out, on = keys]
  if (!is.na(fill_value)) out[is.na(N) | is.nan(N), N := as.numeric(fill_value)]
  
  if (is.null(group_col)) setorderv(out, "Date") else setorderv(out, c(group_col, "Date"))
  out[]
}

# -------------------------------
# UAT
# -------------------------------

sample_data <- data.table(
  group = c("A", "A", "A", "A", "B", "B", "B", "B"),
  start = as.Date(c("1999-01-02", "1999-01-02", "1999-01-03", "1999-01-03",
                    "1999-01-06", "1999-01-06", "1999-01-07", "1999-01-07")),
  end   = as.Date(c("1999-01-05", "1999-01-05", "1999-01-06", "1999-01-06",
                    "1999-01-09", "1999-01-09", "1999-01-10", "1999-01-10")),
  value = c(5, 4, 8, 2, 4, 1, 5, 3)
)

test_that("daily_summary sum with group returns expected totals and NA gaps", {
  res <- daily_summary(sample_data, "start", "end", "value", "group", "sum")
  expect_true(all(c("group", "Date", "N") %in% names(res)))
  expect_equal(nrow(res), 18L)
  expect_equal(res[group == "A" & Date == as.Date("1999-01-02"), N], 9)
  expect_equal(res[group == "A" & Date == as.Date("1999-01-06"), N], 10)
  expect_true(is.na(res[group == "B" & Date == as.Date("1999-01-02"), N]))
  expect_equal(res[group == "B" & Date == as.Date("1999-01-10"), N], 8)
})

test_that("daily_summary mean with group returns expected values and NA gaps", {
  res <- daily_summary(sample_data, "start", "end", "value", "group", "mean")
  expect_true(is.numeric(res$N))
  expect_equal(res[group == "A" & Date == as.Date("1999-01-02"), N], 4.5)
  expect_equal(res[group == "A" & Date == as.Date("1999-01-03"), N], 19 / 4)
  expect_true(is.na(res[group == "B" & Date == as.Date("1999-01-02"), N]))
})

test_that("daily_summary median with group returns expected values", {
  res <- daily_summary(sample_data, "start", "end", "value", "group", "median")
  expect_true(is.numeric(res$N))
  expect_equal(res[group == "A" & Date == as.Date("1999-01-03"), N], 4.5)
  expect_equal(res[group == "B" & Date == as.Date("1999-01-07"), N], 3.5)
})

test_that("daily_summary sum without group returns expected totals", {
  res <- daily_summary(sample_data, "start", "end", "value", group_col = NULL, summary_func = "sum")
  expect_true(all(c("Date", "N") %in% names(res)))
  expect_equal(nrow(res), 9L)
  expect_equal(res[Date == as.Date("1999-01-06"), N], 15)
})

test_that("daily_summary supports a custom summary function", {
  custom_max <- function(x) if (length(x) == 0L || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  res <- daily_summary(sample_data, "start", "end", "value", "group", custom_max)
  expect_true(all(c("group", "Date", "N") %in% names(res)))
  expect_equal(res[group == "A" & Date == as.Date("1999-01-02"), N], 5)
  expect_equal(res[group == "B" & Date == as.Date("1999-01-06"), N], 4)
})

test_that("accrual calendar skips weekends and changes totals accordingly", {
  res <- daily_summary(sample_data, "start", "end", "value", "group", "sum", skip_weekends = TRUE)
  wday <- as.POSIXlt(res$Date)$wday
  expect_true(all(!(wday %in% c(0L, 6L))))
  expect_equal(res[group == "A" & Date == as.Date("1999-01-04"), N], 19)
  expect_equal(res[group == "A" & Date == as.Date("1999-01-06"), N], 10)
  expect_equal(res[group == "B" & Date == as.Date("1999-01-06"), N], 5)
  expect_equal(res[group == "B" & Date == as.Date("1999-01-08"), N], 13)
})

test_that("excluded dates remove accrual and terminate at next included date", {
  res <- daily_summary(
    sample_data, "start", "end", "value", "group", "sum",
    exclude_dates = as.Date("1999-01-06")
  )
  expect_true(!any(res$Date == as.Date("1999-01-06")))
  expect_true(is.na(res[group == "A" & Date == as.Date("1999-01-07"), N]))
})

# -------------------------------
# Example Plot
# -------------------------------

result_mean <- daily_summary(sample_data, "start", "end", "value", "group", "mean")
result_median <- daily_summary(sample_data, "start", "end", "value", "group", "median")

result_plot <- merge(result_mean, result_median, by = c("group", "Date"), suffixes = c("_mean", "_median"))

p <- ggplot(result_plot, aes(x = Date)) +
  geom_line(aes(y = N_mean, color = group, linetype = "Mean"), linewidth = 1) +
  geom_line(aes(y = N_median, color = group, linetype = "Median"), linewidth = 1) +
  labs(x = "Date", y = "Summary", color = "Group", linetype = "Statistic") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", size = 14, face = "bold")
  )

print(p)
