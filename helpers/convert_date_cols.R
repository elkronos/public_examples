#' Convert date-like columns in a data.frame/tibble/data.table
#'
#' Scans columns for date-like values (character, factor, Excel serials)
#' and converts them to Date or POSIXct. Provides diagnostics and optional logging.
#'
#' Enhanced fallback: if an initial parse fails (0% success) and `fuzzy = FALSE`,
#' the function makes one additional attempt with fuzzy cleaning, so
#' strings like "1er avril 2025" or "1ER avril 2025" are recognized even without `fuzzy = TRUE`.
#'
#' @param data A data.frame, tibble or data.table.
#' @param formats Character vector of strptime formats; NULL triggers auto-guess.
#' @param name_patterns Optional regex patterns to restrict which columns are scanned.
#' @param locale Locale (e.g., "fr_FR") for parsing month names.
#' @param fuzzy If TRUE, strip ordinal suffixes (st/nd/rd/th/er) and non-date chars.
#' @param threshold Fraction [0,1] of non-NA values that must successfully parse.
#' @param allow_na If FALSE, only convert when **all** non-NA values parse.
#' @param sample_size Number of rows sampled for format guessing if `formats = NULL`.
#' @param datetime If TRUE, return POSIXct; if FALSE (default), return Date.
#' @param excel If TRUE, also convert numeric columns from Excel serials.
#' @param excel_origin Origin date for Excel serials (default "1899-12-30").
#' @param log If TRUE, write parse failures to a CSV at `log_path`.
#' @param log_path File path for the failure CSV (default "date_parse_failures.csv").
#' @param verbose One of "none", "summary", or "all".
#' @return The input data (converted columns in place) with an attribute
#'   `date_info` (data.frame with columns `column`, `parsed_pct`, and `method`).
#' @importFrom utils write.csv
#' @importFrom lubridate parse_date_time guess_formats
#' @importFrom tibble as_tibble
#' @importFrom data.table copy as.data.table
#' @export
convert_date_cols <- function(
    data,
    formats       = c(
      "%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%m-%d-%Y",
      "%d.%m.%Y","%d/%m/%Y","%Y%m%d"
    ),
    name_patterns = NULL,
    locale        = Sys.getlocale("LC_TIME"),
    fuzzy         = FALSE,
    threshold     = 1,
    allow_na      = TRUE,
    sample_size   = 1000,
    datetime      = FALSE,
    excel         = FALSE,
    excel_origin  = "1899-12-30",
    log           = FALSE,
    log_path      = "date_parse_failures.csv",
    verbose       = c("none","summary","all")
) {
  verbose <- match.arg(verbose)
  user_formats <- !missing(formats)
  
  # Validate input type
  if (!inherits(data, "data.frame") &&
      !inherits(data, "tbl_df") &&
      !inherits(data, "tbl") &&
      !inherits(data, "data.table")) {
    stop("`data` must be a data.frame, tibble, or data.table.")
  }
  
  # Copy or coerce
  is_dt  <- inherits(data, "data.table")
  is_tbl <- inherits(data, "tbl_df") || inherits(data, "tbl")
  if (is_dt) {
    data <- data.table::copy(data)
  } else if (is_tbl) {
    data <- tibble::as_tibble(data)
  } else {
    data <- as.data.frame(data)
  }
  
  if (!requireNamespace("lubridate", quietly=TRUE))
    stop("Please install the 'lubridate' package to parse dates.")
  
  diagnostics <- list()
  failures    <- list()
  
  # Helpers ---------------------------------------------------------------
  is_candidate <- function(name) {
    if (is.null(name_patterns)) return(TRUE)
    any(vapply(name_patterns,
               function(p) grepl(p, name, ignore.case=TRUE),
               logical(1)))
  }
  clean_text <- function(txt) {
    # Remove ordinal suffixes, case-insensitive
    no_ord <- gsub("(?<=\\d)(st|nd|rd|th|er)", "", txt, perl=TRUE, ignore.case=TRUE)
    # Keep digits, Unicode letters, and common separators
    gsub("[^0-9\\p{L} ,./:-]", "", no_ord, perl=TRUE)
  }
  guess_fmts <- function(vals) {
    sample_vals <- if (length(vals) > sample_size) sample(vals, sample_size) else vals
    unique(lubridate::guess_formats(sample_vals,
                                    orders = c("Ymd","Ymd HMS","mdy","mdy HMS","dmy","dmy HMS")))
  }
  try_parse <- function(txt, fmts) {
    suppressWarnings(
      lubridate::parse_date_time(txt, orders=fmts, locale=locale, exact=TRUE)
    )
  }
  should_convert <- function(pct) {
    !is.na(pct) &&
      pct >= threshold &&
      (allow_na || pct == 1)
  }
  
  # Main loop -------------------------------------------------------------
  for (nm in names(data)) {
    col <- data[[nm]]
    if (!is_candidate(nm)) next
    
    # Excel serials
    if (excel && is.numeric(col)) {
      nz     <- !is.na(col)
      parsed <- as.Date(col, origin=excel_origin)
      pct    <- if (any(nz)) mean(!is.na(parsed)[nz]) else NA_real_
      if (pct > 0) {
        diagnostics[[nm]] <- data.frame(
          column     = nm,
          parsed_pct = pct,
          method     = "excel",
          stringsAsFactors = FALSE
        )
      }
      if (should_convert(pct)) {
        data[[nm]] <- if (datetime) as.POSIXct(parsed) else parsed
        if (verbose == "all") message(sprintf("Converted '%s' via excel (%.1f%%)", nm, 100*pct))
      } else if (verbose == "all" && pct > 0) {
        message(sprintf("Skipped excel '%s' (%.1f%% < %.1f%% threshold)", nm, 100*pct, 100*threshold))
      }
      if (log && any(nz & is.na(parsed))) {
        idx <- which(nz & is.na(parsed))
        failures[[nm]] <- data.frame(
          column = nm,
          row    = idx,
          value  = col[idx],
          stringsAsFactors = FALSE
        )
      }
      next
    }
    
    # Character or factor columns
    if (!is.character(col) && !is.factor(col)) next
    raw_txt  <- as.character(col)
    work_txt <- if (fuzzy) clean_text(raw_txt) else raw_txt
    nz       <- !is.na(work_txt)
    fmts     <- if (is.null(formats)) guess_fmts(work_txt[nz]) else formats
    if (length(fmts) == 0) {
      if (verbose != "none") message(sprintf("No candidate formats for column '%s'", nm))
      next
    }
    
    parsed <- try_parse(work_txt, fmts)
    ok     <- nz & !is.na(parsed)
    pct    <- if (any(nz)) mean(ok[nz]) else NA_real_
    
    # Fallback once if nothing parsed
    if (pct == 0 && !fuzzy) {
      parsed2 <- try_parse(clean_text(raw_txt), fmts)
      ok2      <- nz & !is.na(parsed2)
      pct2     <- if (any(nz)) mean(ok2[nz]) else NA_real_
      if (pct2 > pct) {
        parsed <- parsed2
        pct    <- pct2
      }
    }
    
    # Record diagnostics only if parsed or user provided formats
    if (pct > 0 || user_formats) {
      diagnostics[[nm]] <- data.frame(
        column     = nm,
        parsed_pct = pct,
        method     = paste(fmts, collapse = "|"),
        stringsAsFactors = FALSE
      )
    }
    
    if (should_convert(pct)) {
      data[[nm]] <- if (datetime) parsed else as.Date(parsed)
      if (verbose == "all") message(sprintf("Converted '%s' (%.1f%%)", nm, 100*pct))
    } else if (verbose == "all" && pct > 0) {
      message(sprintf("Skipped '%s' (%.1f%% < %.1f%% threshold)", nm, 100*pct, 100*threshold))
    }
    if (log && any(nz & is.na(parsed))) {
      idx <- which(nz & is.na(parsed))
      failures[[nm]] <- data.frame(
        column = nm,
        row    = idx,
        value  = raw_txt[idx],
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Write failures log if requested
  if (log && length(failures) > 0) {
    dir <- dirname(log_path)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    utils::write.csv(do.call(rbind, failures), log_path, row.names = FALSE)
    if (verbose != "none") message("Failures written to: ", log_path)
  }
  
  # Attach diagnostics
  diag_df <- if (length(diagnostics)) do.call(rbind, diagnostics) else data.frame()
  attr(data, "date_info") <- diag_df
  if (verbose == "summary" && nrow(diag_df) > 0) print(diag_df)
  
  # Restore original class
  if (is_tbl)        data <- tibble::as_tibble(data)
  else if (is_dt)    data <- data.table::as.data.table(data)
  
  data
}
