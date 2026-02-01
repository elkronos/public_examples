#' Date conversion utilities for data.frame / tibble / data.table
#'
#' This file defines:
#' - convert_date_cols(): converts date-like columns and attaches a `date_info` attribute
#' - make_date_converter(): returns a reusable converter for repeatable workflows
#' - get_date_info(): extracts the attached diagnostics
#' - clean_date_text(), as_date_orders(), guess_date_orders(): helpers for date parsing
#'
#' Notes on formats/orders:
#' - If a format contains "%", it is treated like a strptime-style format and is translated
#'   into a lubridate order string.
#' - Otherwise, it is treated as a lubridate order string directly (e.g., "Y-m-d", "d/m/Y").
#'
#' Dependencies:
#' - lubridate is required
#' - tibble and data.table are optional and used only when the input object uses those classes


`%||%` <- function(x, y) if (is.null(x)) y else x


#' Clean date-like text
#'
#' Removes common ordinal suffixes and trims non-date characters while keeping
#' digits, Unicode letters, and common separators.
#'
#' @param txt Character vector
#' @return Character vector
clean_date_text <- function(txt) {
  if (is.null(txt)) return(txt)
  txt <- as.character(txt)
  
  # Remove ordinal suffixes after a digit, case-insensitive
  txt <- gsub("(?<=\\d)(st|nd|rd|th|er)", "", txt, perl = TRUE, ignore.case = TRUE)
  
  # Keep digits, Unicode letters, spaces, and common separators
  txt <- gsub("[^0-9\\p{L} ,./:-]", "", txt, perl = TRUE)
  
  trimws(txt)
}


.is_blank_or_na <- function(x) {
  x <- as.character(x)
  is.na(x) | nchar(trimws(x)) == 0
}

.normalize_text_missing <- function(x) {
  x <- as.character(x)
  x[.is_blank_or_na(x)] <- NA_character_
  x
}

.require_lubridate <- function() {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required.")
  }
  invisible(TRUE)
}


#' Translate strptime formats into lubridate orders
#'
#' Supported directives cover common date/time patterns:
#' %Y %y %m %d %e %H %I %M %S %p %b %B
#'
#' @param formats Character vector
#' @return Character vector of lubridate orders
as_date_orders <- function(formats) {
  if (is.null(formats)) return(NULL)
  formats <- as.character(formats)
  
  translate_one <- function(fmt) {
    if (!grepl("%", fmt, fixed = TRUE)) return(fmt)
    
    # Replace directives with lubridate tokens, leaving separators intact
    x <- fmt
    x <- gsub("%Y", "Y", x, fixed = TRUE)
    x <- gsub("%y", "y", x, fixed = TRUE)
    
    x <- gsub("%m", "m", x, fixed = TRUE)
    
    # Day of month
    x <- gsub("%d", "d", x, fixed = TRUE)
    x <- gsub("%e", "d", x, fixed = TRUE)
    
    # Time
    x <- gsub("%H", "H", x, fixed = TRUE)
    x <- gsub("%I", "I", x, fixed = TRUE)
    x <- gsub("%M", "M", x, fixed = TRUE)
    x <- gsub("%S", "S", x, fixed = TRUE)
    x <- gsub("%p", "p", x, fixed = TRUE)
    
    # Month names
    x <- gsub("%b", "b", x, fixed = TRUE)
    x <- gsub("%B", "B", x, fixed = TRUE)
    
    x
  }
  
  unique(vapply(formats, translate_one, character(1)))
}


#' Guess candidate orders from a sample of values
#'
#' @param vals Character vector (ideally already normalized/cleaned)
#' @param sample_size Integer
#' @param base_orders Character vector of general orders used as a search space
#' @return Character vector of lubridate orders
guess_date_orders <- function(
    vals,
    sample_size = 1000,
    base_orders = c("Ymd", "Ymd HMS", "Y-m-d", "Y/m/d", "Y.m.d",
                    "mdY", "mdY HMS", "m/d/Y", "m-d-Y",
                    "dmY", "dmY HMS", "d/m/Y", "d-m-Y", "d.m.Y",
                    "YmdT", "Y-m-d HMS", "Y/m/d HMS", "Y.m.d HMS")
) {
  .require_lubridate()
  
  vals <- .normalize_text_missing(vals)
  vals <- vals[!is.na(vals)]
  
  if (!length(vals)) return(character(0))
  
  if (length(vals) > sample_size) {
    vals <- sample(vals, sample_size)
  }
  
  unique(lubridate::guess_formats(vals, orders = base_orders))
}


.parse_with_orders <- function(txt, orders, locale, tz, exact) {
  .require_lubridate()
  
  suppressWarnings(
    lubridate::parse_date_time(
      txt,
      orders = orders,
      locale = locale,
      tz = tz,
      exact = exact
    )
  )
}


.compute_parse_stats <- function(parsed, non_missing_idx) {
  if (!any(non_missing_idx)) {
    return(list(
      parsed_pct = NA_real_,
      n_total = 0L,
      n_parsed = 0L,
      n_failed = 0L
    ))
  }
  
  ok <- non_missing_idx & !is.na(parsed)
  
  n_total  <- sum(non_missing_idx)
  n_parsed <- sum(ok)
  n_failed <- n_total - n_parsed
  parsed_pct <- if (n_total > 0) (n_parsed / n_total) else NA_real_
  
  list(
    parsed_pct = parsed_pct,
    n_total = as.integer(n_total),
    n_parsed = as.integer(n_parsed),
    n_failed = as.integer(n_failed)
  )
}


.should_convert <- function(parsed_pct, threshold, allow_na) {
  !is.na(parsed_pct) &&
    parsed_pct >= threshold &&
    (allow_na || parsed_pct == 1)
}


.is_candidate_name <- function(name, name_patterns) {
  if (is.null(name_patterns)) return(TRUE)
  any(vapply(name_patterns, function(p) grepl(p, name, ignore.case = TRUE), logical(1)))
}


# Excel serial handling --------------------------------------------------------

.as_date_origin <- function(excel_origin) {
  if (inherits(excel_origin, "Date")) return(excel_origin)
  as.Date(excel_origin)
}

# A lightweight plausibility check for Excel serials
.is_probable_excel_serial <- function(x, origin_date, min_date, max_date) {
  x <- x[!is.na(x)]
  if (!length(x)) return(FALSE)
  
  # Excel serials are typically non-negative and near-integer
  near_integer <- mean(abs(x - round(x)) < 1e-6)
  if (near_integer < 0.8) return(FALSE)
  
  # Map the allowed date window into serial range
  origin_date <- .as_date_origin(origin_date)
  min_serial <- as.numeric(as.Date(min_date) - origin_date)
  max_serial <- as.numeric(as.Date(max_date) - origin_date)
  
  in_range <- mean(x >= min_serial & x <= max_serial)
  in_range >= 0.8
}

.parse_excel_serial <- function(x, excel_origin, datetime, tz, min_date, max_date) {
  origin_date <- .as_date_origin(excel_origin)
  
  out <- rep(NA, length(x))
  nz <- !is.na(x)
  
  # Convert to Date/POSIXct; respect the plausibility window by marking out-of-window values NA
  serial <- x[nz]
  date_candidate <- origin_date + serial
  
  date_ok <- date_candidate >= as.Date(min_date) & date_candidate <= as.Date(max_date)
  parsed_idx <- which(nz)[date_ok]
  
  if (!length(parsed_idx)) {
    return(list(parsed = out, method = "excel", non_missing = nz))
  }
  
  if (datetime) {
    # Allow fractional days as time-of-day
    origin_posix <- as.POSIXct(origin_date, tz = tz)
    out[parsed_idx] <- origin_posix + (x[parsed_idx] * 86400)
  } else {
    out[parsed_idx] <- as.Date(x[parsed_idx], origin = origin_date)
  }
  
  list(parsed = out, method = "excel", non_missing = nz)
}


# Input class handling --------------------------------------------------------

.prepare_input <- function(data) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame, tibble, or data.table.")
  }
  
  is_dt  <- inherits(data, "data.table")
  is_tbl <- inherits(data, "tbl_df") || inherits(data, "tbl")
  
  if (is_dt && requireNamespace("data.table", quietly = TRUE)) {
    d <- data.table::copy(data)
    restore <- function(x) data.table::as.data.table(x)
  } else if (is_tbl && requireNamespace("tibble", quietly = TRUE)) {
    d <- tibble::as_tibble(data)
    restore <- function(x) tibble::as_tibble(x)
  } else {
    d <- as.data.frame(data)
    restore <- function(x) as.data.frame(x)
  }
  
  list(data = d, restore = restore, is_dt = is_dt, is_tbl = is_tbl)
}


# Diagnostics/logging ---------------------------------------------------------

.new_diag_row <- function(column, parsed_pct, method, type_in, type_out, n_total, n_parsed, n_failed) {
  data.frame(
    column = column,
    parsed_pct = parsed_pct,
    method = method,
    type_in = type_in,
    type_out = type_out,
    n_total = n_total,
    n_parsed = n_parsed,
    n_failed = n_failed,
    stringsAsFactors = FALSE
  )
}

.failure_rows <- function(column, idx, value) {
  data.frame(
    column = column,
    row = idx,
    value = value,
    stringsAsFactors = FALSE
  )
}

.write_failures_csv <- function(failures, log_path) {
  if (!length(failures)) return(invisible(FALSE))
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(do.call(rbind, failures), log_path, row.names = FALSE)
  invisible(TRUE)
}


#' Extract attached diagnostics
#'
#' @param x An object returned by convert_date_cols()
#' @return data.frame (may be empty)
get_date_info <- function(x) {
  info <- attr(x, "date_info")
  if (is.null(info)) return(data.frame())
  info
}


#' Create a reusable converter for repeatable workflows
#'
#' @param ... Arguments forwarded to convert_date_cols() (except `data`)
#' @return An object with a `$run(data, ...)` method
make_date_converter <- function(...) {
  config <- list(...)
  structure(
    list(
      config = config,
      run = function(data, ...) {
        extra <- list(...)
        do.call(convert_date_cols, c(list(data = data), config, extra))
      }
    ),
    class = "date_col_converter"
  )
}

print.date_col_converter <- function(x, ...) {
  cat("<date_col_converter>\n")
  if (length(x$config)) {
    cat("config:\n")
    for (nm in names(x$config)) {
      cat(" - ", nm, ": ", deparse(x$config[[nm]]), "\n", sep = "")
    }
  } else {
    cat("config: <none>\n")
  }
  invisible(x)
}


#' Convert date-like columns in a data.frame/tibble/data.table
#'
#' Scans columns for date-like values (character, factor, optional Excel serials)
#' and converts them to Date or POSIXct. Attaches diagnostics and optionally logs
#' parse failures.
#'
#' @param data A data.frame, tibble or data.table.
#' @param formats Character vector. If elements contain "%", they are interpreted as
#'   strptime-style formats and translated into lubridate orders. Otherwise, they are
#'   treated as lubridate orders directly (e.g., "Y-m-d", "d/m/Y", "Ymd HMS").
#'   Use NULL to trigger auto-guessing.
#' @param name_patterns Optional regex patterns to restrict which columns are scanned.
#' @param locale Locale for parsing month/day names (e.g., "fr_FR").
#' @param fuzzy If TRUE, apply clean_date_text() before parsing.
#' @param threshold Fraction [0,1] of non-missing values that must parse.
#' @param allow_na If FALSE, conversion occurs only when all non-missing values parse.
#' @param sample_size Number of rows sampled for guessing if formats is NULL.
#' @param datetime If TRUE, return POSIXct; if FALSE, return Date.
#' @param tz Time zone for parsed datetimes.
#' @param exact Passed to lubridate::parse_date_time().
#' @param excel If TRUE, numeric columns may be interpreted as Excel serials.
#' @param excel_mode "auto" applies plausibility checks; "force" attempts conversion
#'   for any numeric column.
#' @param excel_origin Origin date for Excel serials.
#' @param excel_min_date Minimum allowed date when interpreting Excel serials.
#' @param excel_max_date Maximum allowed date when interpreting Excel serials.
#' @param log If TRUE, write parse failures to CSV at log_path.
#' @param log_path File path for the failure CSV.
#' @param verbose One of "none", "summary", or "all".
#' @return Converted data with attribute `date_info`.
convert_date_cols <- function(
    data,
    formats       = c(
      "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y",
      "%d.%m.%Y", "%d/%m/%Y", "%Y%m%d",
      "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S"
    ),
    name_patterns = NULL,
    locale        = Sys.getlocale("LC_TIME"),
    fuzzy         = FALSE,
    threshold     = 1,
    allow_na      = TRUE,
    sample_size   = 1000,
    datetime      = FALSE,
    tz            = "UTC",
    exact         = TRUE,
    excel         = FALSE,
    excel_mode    = c("auto", "force"),
    excel_origin  = "1899-12-30",
    excel_min_date = "1900-01-01",
    excel_max_date = as.character(Sys.Date() + 3650),
    log           = FALSE,
    log_path      = "date_parse_failures.csv",
    verbose       = c("none", "summary", "all")
) {
  verbose <- match.arg(verbose)
  excel_mode <- match.arg(excel_mode)
  
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    stop("`threshold` must be a single number in [0,1].")
  }
  if (!is.logical(allow_na) || length(allow_na) != 1) stop("`allow_na` must be TRUE/FALSE.")
  if (!is.logical(fuzzy) || length(fuzzy) != 1) stop("`fuzzy` must be TRUE/FALSE.")
  if (!is.logical(datetime) || length(datetime) != 1) stop("`datetime` must be TRUE/FALSE.")
  if (!is.logical(excel) || length(excel) != 1) stop("`excel` must be TRUE/FALSE.")
  if (!is.logical(log) || length(log) != 1) stop("`log` must be TRUE/FALSE.")
  if (!is.numeric(sample_size) || length(sample_size) != 1 || sample_size <= 0) {
    stop("`sample_size` must be a positive number.")
  }
  
  prep <- .prepare_input(data)
  d <- prep$data
  
  diagnostics <- list()
  failures <- list()
  
  user_provided_formats <- !missing(formats)
  
  for (nm in names(d)) {
    if (!.is_candidate_name(nm, name_patterns)) next
    
    col <- d[[nm]]
    type_in <- paste(class(col), collapse = "/")
    
    # Excel serial conversion (numeric columns)
    if (excel && is.numeric(col)) {
      excel_ok <- TRUE
      if (excel_mode == "auto") {
        excel_ok <- .is_probable_excel_serial(
          col,
          origin_date = excel_origin,
          min_date = excel_min_date,
          max_date = excel_max_date
        )
      }
      
      if (excel_ok) {
        res <- .parse_excel_serial(
          x = col,
          excel_origin = excel_origin,
          datetime = datetime,
          tz = tz,
          min_date = excel_min_date,
          max_date = excel_max_date
        )
        
        stats <- .compute_parse_stats(res$parsed, res$non_missing)
        pct <- stats$parsed_pct
        
        diagnostics[[nm]] <- .new_diag_row(
          column = nm,
          parsed_pct = pct,
          method = res$method,
          type_in = type_in,
          type_out = if (datetime) "POSIXct" else "Date",
          n_total = stats$n_total,
          n_parsed = stats$n_parsed,
          n_failed = stats$n_failed
        )
        
        if (.should_convert(pct, threshold, allow_na)) {
          d[[nm]] <- res$parsed
          if (verbose == "all") message(sprintf("Converted '%s' via excel (%.1f%%)", nm, 100 * pct))
        } else if (verbose == "all") {
          message(sprintf("Skipped '%s' via excel (%.1f%%)", nm, 100 * pct))
        }
        
        if (log && any(res$non_missing & is.na(res$parsed))) {
          idx <- which(res$non_missing & is.na(res$parsed))
          failures[[nm]] <- .failure_rows(nm, idx, col[idx])
        }
      } else {
        if (verbose == "all") message(sprintf("Skipped '%s' numeric column (excel plausibility not met)", nm))
      }
      
      next
    }
    
    # Text-like columns
    if (!is.character(col) && !is.factor(col)) next
    
    raw_txt <- as.character(col)
    raw_txt <- .normalize_text_missing(raw_txt)
    
    work_txt <- if (fuzzy) clean_date_text(raw_txt) else raw_txt
    work_txt <- .normalize_text_missing(work_txt)
    
    nz <- !is.na(work_txt)
    if (!any(nz)) next
    
    orders <- if (is.null(formats)) {
      guess_date_orders(work_txt[nz], sample_size = sample_size)
    } else {
      as_date_orders(formats)
    }
    
    if (!length(orders)) {
      if (verbose != "none") message(sprintf("No candidate orders for column '%s'", nm))
      next
    }
    
    parsed <- .parse_with_orders(work_txt, orders, locale = locale, tz = tz, exact = exact)
    
    stats <- .compute_parse_stats(parsed, nz)
    pct <- stats$parsed_pct
    
    # One additional attempt using cleaned text when fuzzy is FALSE and parsing yields no matches
    if (!fuzzy && !is.na(pct) && pct == 0) {
      alt_txt <- clean_date_text(raw_txt)
      alt_txt <- .normalize_text_missing(alt_txt)
      nz2 <- !is.na(alt_txt)
      
      parsed2 <- .parse_with_orders(alt_txt, orders, locale = locale, tz = tz, exact = exact)
      stats2 <- .compute_parse_stats(parsed2, nz2)
      pct2 <- stats2$parsed_pct
      
      if (!is.na(pct2) && pct2 > pct) {
        parsed <- parsed2
        stats <- stats2
        pct <- pct2
        nz <- nz2
      }
    }
    
    if (pct > 0 || user_provided_formats) {
      method <- paste(orders, collapse = "|")
      diagnostics[[nm]] <- .new_diag_row(
        column = nm,
        parsed_pct = pct,
        method = method,
        type_in = type_in,
        type_out = if (datetime) "POSIXct" else "Date",
        n_total = stats$n_total,
        n_parsed = stats$n_parsed,
        n_failed = stats$n_failed
      )
    }
    
    if (.should_convert(pct, threshold, allow_na)) {
      if (datetime) {
        d[[nm]] <- as.POSIXct(parsed, tz = tz)
      } else {
        d[[nm]] <- as.Date(parsed, tz = tz)
      }
      if (verbose == "all") message(sprintf("Converted '%s' (%.1f%%)", nm, 100 * pct))
    } else if (verbose == "all" && !is.na(pct) && pct > 0) {
      message(sprintf("Skipped '%s' (%.1f%%)", nm, 100 * pct))
    }
    
    if (log && any(nz & is.na(parsed))) {
      idx <- which(nz & is.na(parsed))
      failures[[nm]] <- .failure_rows(nm, idx, raw_txt[idx])
    }
  }
  
  if (log && length(failures)) {
    wrote <- .write_failures_csv(failures, log_path)
    if (wrote && verbose != "none") message("Failures written to: ", log_path)
  }
  
  diag_df <- if (length(diagnostics)) do.call(rbind, diagnostics) else data.frame()
  attr(d, "date_info") <- diag_df
  
  if (verbose == "summary" && nrow(diag_df) > 0) print(diag_df)
  
  prep$restore(d)
}
