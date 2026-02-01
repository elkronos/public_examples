# -----------------------
# Internal utilities
# -----------------------

.stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) .stopf("Package '%s' is required.", pkg)
  invisible(TRUE)
}

.is_empty <- function(x) length(x) == 0L

.na_scalar <- function(x) {
  if (is.factor(x)) return(factor(NA, levels = levels(x)))
  if (inherits(x, "Date")) return(as.Date(NA))
  if (inherits(x, "POSIXt")) return(as.POSIXct(NA))
  if (is.character(x)) return(NA_character_)
  if (is.integer(x)) return(NA_integer_)
  if (is.numeric(x)) return(NA_real_)
  NA
}

.safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (.is_empty(x)) return(NA_real_)
  mean(x)
}

.safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (.is_empty(x)) return(NA)
  min(x)
}

.safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (.is_empty(x)) return(NA)
  max(x)
}

.safe_div <- function(num, den) ifelse(den == 0, NA_real_, num / den)

#' Convert values to Date
#'
#' Converts a vector to Date while handling Date, POSIXt, and character inputs.
#' Character inputs are parsed using POSIXct with a configurable set of formats.
#'
#' @param x Vector to convert.
#' @param tz Timezone used when parsing character values via POSIXct.
#' @param try_formats Character vector of formats tried for character parsing.
#'
#' @return A Date vector.
#' @export
as_date_safe <- function(
    x,
    tz = "UTC",
    try_formats = c(
      "%Y-%m-%d", "%Y/%m/%d",
      "%m/%d/%Y", "%d/%m/%Y",
      "%m-%d-%Y", "%d-%m-%Y",
      "%Y%m%d"
    )
) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  if (is.character(x)) {
    px <- suppressWarnings(as.POSIXct(x, tz = tz, tryFormats = try_formats))
    return(as.Date(px))
  }
  
  suppressWarnings(as.Date(x))
}

# -----------------------
# Mode
# -----------------------

#' Mode of a vector
#'
#' Computes the most frequent value(s) in a vector, with configurable output formats
#' and tie-handling behavior.
#'
#' @param x Atomic vector or factor.
#' @param return_type One of: "string", "vector", "first", "count".
#' @param na_rm Remove NAs before computing.
#' @param ties One of: "all", "first".
#' @param tie_break One of: "first", "smallest", "largest" (used when ties="first").
#' @param sep Separator used when return_type="string".
#' @param keep_class Keep original class for "vector"/"first" where possible.
#'
#' @return Depending on return_type:
#'   - "string": a delimited character string of modes
#'   - "vector": a vector of modes
#'   - "first": a single mode value
#'   - "count": the number of modes
#' @export
mode_value <- function(
    x,
    return_type = c("string", "vector", "first", "count"),
    na_rm = TRUE,
    ties = c("all", "first"),
    tie_break = c("first", "smallest", "largest"),
    sep = ", ",
    keep_class = TRUE
) {
  return_type <- match.arg(return_type)
  ties <- match.arg(ties)
  tie_break <- match.arg(tie_break)
  
  if (!(is.atomic(x) || is.factor(x))) .stopf("x must be an atomic vector or factor.")
  
  x2 <- if (na_rm) x[!is.na(x)] else x
  
  if (.is_empty(x2)) {
    if (return_type == "count") return(0L)
    if (return_type == "string") return(NA_character_)
    return(.na_scalar(x))
  }
  
  u <- unique(x2)
  idx <- match(x2, u)
  counts <- tabulate(idx, nbins = length(u))
  maxc <- max(counts)
  modes <- u[counts == maxc]
  
  if (ties == "first" && length(modes) > 1L) {
    if (tie_break == "first") {
      modes <- modes[1L]
    } else {
      ok_order <- is.numeric(modes) || is.integer(modes) || inherits(modes, "Date")
      if (ok_order) {
        modes <- if (tie_break == "smallest") modes[which.min(modes)] else modes[which.max(modes)]
      } else {
        modes <- modes[1L]
      }
    }
  }
  
  if (return_type == "count") return(as.integer(length(modes)))
  if (return_type == "string") return(paste(as.character(modes), collapse = sep))
  
  if (!keep_class && is.factor(modes)) modes <- as.character(modes)
  if (return_type == "vector") return(modes)
  
  modes[[1L]]
}

# -----------------------
# Numeric conversion
# -----------------------

#' Convert to numeric
#'
#' Converts a vector to numeric (double). Factors are converted through character.
#' Character values can be trimmed, specific strings can be treated as NA, and commas
#' can be removed before parsing.
#'
#' @param x Vector to convert.
#' @param na_values Character values treated as NA (after trimming).
#' @param trim Trim whitespace for character inputs.
#' @param remove_commas Remove "," before parsing.
#' @param strict If TRUE and conversion yields all NA while input had non-NA, return all-NA.
#'
#' @return Numeric (double) vector.
#' @export
to_numeric <- function(
    x,
    na_values = c("", "NA", "N/A", "NULL", "null"),
    trim = TRUE,
    remove_commas = TRUE,
    strict = TRUE
) {
  if (is.null(x)) return(numeric(0))
  if (is.numeric(x)) return(as.numeric(x))
  
  n <- length(x)
  nm <- names(x)
  
  if (is.factor(x)) x <- as.character(x)
  
  if (is.logical(x)) {
    out <- as.numeric(x)
    names(out) <- nm
    return(out)
  }
  
  if (is.character(x)) {
    y <- x
    if (trim) y <- trimws(y)
    y[y %in% na_values] <- NA_character_
    if (remove_commas) y <- gsub(",", "", y, fixed = TRUE)
    
    out <- suppressWarnings(as.numeric(y))
    
    if (strict) {
      had_non_na <- any(!is.na(y))
      if (had_non_na && all(is.na(out))) out <- rep(NA_real_, n)
    }
    
    names(out) <- nm
    return(out)
  }
  
  out <- suppressWarnings(as.numeric(as.character(x)))
  if (strict) {
    had_non_na <- any(!is.na(x))
    if (had_non_na && all(is.na(out))) out <- rep(NA_real_, n)
  }
  names(out) <- nm
  out
}

# -----------------------
# Date stats
# -----------------------

#' Date statistics
#'
#' Computes span between min/max dates, counts invalid entries, computes gap summaries,
#' and identifies streaks of consecutive dates.
#'
#' @param dates Vector of Date/POSIXt/character.
#' @param tz Timezone used for parsing character dates.
#' @param try_formats Formats to try for parsing.
#' @param unique_dates Use unique valid dates for gaps/streaks.
#' @param middle_as "Date" or "character".
#'
#' @return A list with:
#'   - span: numeric span in days
#'   - invalid: count of NA after conversion
#'   - invalid_input: count of NA in the original input
#'   - n_valid: count of non-NA after conversion
#'   - n_unique: count of unique valid dates used for gaps
#'   - middle_date: midpoint date between min and max
#'   - avg_gap, median_gap: gap summaries in days
#'   - longest_streak, shortest_streak: streak lengths for consecutive dates
#'   - max_gap, min_gap: largest/smallest gap in days
#' @export
date_stats <- function(
    dates,
    tz = "UTC",
    try_formats = c(
      "%Y-%m-%d", "%Y/%m/%d",
      "%m/%d/%Y", "%d/%m/%Y",
      "%m-%d-%Y", "%d-%m-%Y",
      "%Y%m%d"
    ),
    unique_dates = TRUE,
    middle_as = c("Date", "character")
) {
  middle_as <- match.arg(middle_as)
  
  if (is.null(dates)) .stopf("dates must not be NULL.")
  if (!is.vector(dates)) .stopf("dates must be a vector.")
  
  invalid_input <- sum(is.na(dates))
  d <- as_date_safe(dates, tz = tz, try_formats = try_formats)
  invalid <- sum(is.na(d))
  
  valid <- d[!is.na(d)]
  if (.is_empty(valid)) {
    return(list(
      span = NA_real_,
      invalid = invalid,
      invalid_input = invalid_input,
      n_valid = 0L,
      n_unique = 0L,
      middle_date = if (middle_as == "Date") as.Date(NA) else NA_character_,
      avg_gap = NA_real_,
      median_gap = NA_real_,
      longest_streak = NA_integer_,
      shortest_streak = NA_integer_,
      max_gap = NA_real_,
      min_gap = NA_real_
    ))
  }
  
  if (unique_dates) valid <- unique(valid)
  valid <- sort(valid)
  
  if (length(valid) == 1L) {
    md <- valid[1L]
    return(list(
      span = 0,
      invalid = invalid,
      invalid_input = invalid_input,
      n_valid = sum(!is.na(d)),
      n_unique = length(valid),
      middle_date = if (middle_as == "Date") md else as.character(md),
      avg_gap = NA_real_,
      median_gap = NA_real_,
      longest_streak = 1L,
      shortest_streak = 1L,
      max_gap = NA_real_,
      min_gap = NA_real_
    ))
  }
  
  span <- as.numeric(max(valid) - min(valid))
  middle_date <- min(valid) + as.integer(round(span / 2))
  
  gaps <- as.numeric(diff(valid))
  avg_gap <- mean(gaps)
  median_gap <- stats::median(gaps)
  max_gap <- max(gaps)
  min_gap <- min(gaps)
  
  r <- rle(gaps == 1)
  if (any(r$values)) {
    streaks <- r$lengths[r$values] + 1L
    longest_streak <- max(streaks)
    shortest_streak <- min(streaks)
  } else {
    longest_streak <- 1L
    shortest_streak <- 1L
  }
  
  list(
    span = span,
    invalid = invalid,
    invalid_input = invalid_input,
    n_valid = sum(!is.na(d)),
    n_unique = length(valid),
    middle_date = if (middle_as == "Date") middle_date else as.character(middle_date),
    avg_gap = avg_gap,
    median_gap = median_gap,
    longest_streak = as.integer(longest_streak),
    shortest_streak = as.integer(shortest_streak),
    max_gap = max_gap,
    min_gap = min_gap
  )
}

# -----------------------
# Factor stats
# -----------------------

#' Factor statistics
#'
#' Computes level count, observation counts and percentages, and the mode.
#'
#' @param x Factor (or coercible to factor when strict=FALSE).
#' @param strict If TRUE, require factor input.
#' @param include_na Include NA counts.
#'
#' @return A list with:
#'   - levels_count
#'   - obs_count
#'   - obs_perc
#'   - mode
#' @export
factor_stats <- function(x, strict = TRUE, include_na = TRUE) {
  if (is.null(x)) .stopf("Input must not be NULL.")
  
  if (!is.factor(x)) {
    if (strict) .stopf("Input must be a factor when strict=TRUE.")
    x <- as.factor(x)
  }
  
  tab <- table(x, useNA = if (include_na) "ifany" else "no")
  perc <- round(100 * prop.table(tab), 2)
  
  list(
    levels_count = length(levels(x)),
    obs_count = paste(names(tab), as.integer(tab), sep = ": ", collapse = "; "),
    obs_perc = paste(as.numeric(perc), collapse = ", "),
    mode = mode_value(x, return_type = "string")
  )
}

# -----------------------
# GIS stats
# -----------------------

#' GIS statistics from lon/lat
#'
#' Creates an sf point object from lon/lat columns, computes centroid and convex hull,
#' and optionally computes a pairwise distance matrix and nearest-neighbor indices.
#'
#' @param coords data.frame with lon/lat columns.
#' @param lon Name of longitude column (case-insensitive).
#' @param lat Name of latitude column (case-insensitive).
#' @param input_crs EPSG of input CRS.
#' @param output_crs EPSG of output CRS used when projected=TRUE.
#' @param projected If TRUE, transforms points to output_crs.
#' @param distance_matrix If TRUE, computes a full pairwise distance matrix.
#' @param nearest If TRUE, computes nearest-neighbor index per point.
#' @param max_n_matrix Maximum number of points for distance matrix computations.
#' @param drop_units If TRUE, drops units from distance outputs when the units package is available.
#'
#' @return A list with:
#'   - centroid: sf geometry for centroid
#'   - convex_hull: sf geometry for convex hull
#'   - distance_matrix: matrix of distances (optional)
#'   - nearest_index: integer vector of nearest indices (optional)
#'   - greatest_distance: maximum distance observed
#'   - n_points: number of points used
#' @export
gis_stats <- function(
    coords,
    lon = "longitude",
    lat = "latitude",
    input_crs = 4326,
    output_crs = 3857,
    projected = TRUE,
    distance_matrix = FALSE,
    nearest = FALSE,
    max_n_matrix = 5000L,
    drop_units = TRUE
) {
  .require_pkg("sf")
  
  if (!is.data.frame(coords)) .stopf("coords must be a data.frame.")
  
  cols_lower <- tolower(names(coords))
  if (!(tolower(lon) %in% cols_lower && tolower(lat) %in% cols_lower)) {
    .stopf("coords must contain lon/lat columns (case-insensitive).")
  }
  
  lon_name <- names(coords)[match(tolower(lon), cols_lower)]
  lat_name <- names(coords)[match(tolower(lat), cols_lower)]
  
  ok <- !(is.na(coords[[lon_name]]) | is.na(coords[[lat_name]]))
  df_ok <- coords[ok, , drop = FALSE]
  
  if (nrow(df_ok) == 0L) {
    return(list(
      centroid = NA,
      convex_hull = NA,
      distance_matrix = NULL,
      nearest_index = NULL,
      greatest_distance = NA,
      n_points = 0L
    ))
  }
  
  pts <- sf::st_as_sf(df_ok, coords = c(lon_name, lat_name), crs = input_crs, remove = FALSE)
  if (projected) pts <- sf::st_transform(pts, crs = output_crs)
  
  n <- nrow(pts)
  
  centroid <- sf::st_centroid(sf::st_union(pts))
  hull <- sf::st_convex_hull(sf::st_union(pts))
  
  hull_xy <- sf::st_coordinates(hull)
  hull_xy <- unique(hull_xy[, c("X", "Y"), drop = FALSE])
  hull_pts <- sf::st_as_sf(
    data.frame(X = hull_xy[, 1], Y = hull_xy[, 2]),
    coords = c("X", "Y"),
    crs = sf::st_crs(pts)
  )
  greatest_distance <- if (nrow(hull_pts) >= 2L) max(sf::st_distance(hull_pts)) else NA
  
  dm <- NULL
  nearest_index <- NULL
  
  if (nearest) distance_matrix <- TRUE
  
  if (distance_matrix) {
    if (n > max_n_matrix) {
      warning(sprintf("n=%d exceeds max_n_matrix=%d; skipping distance computations.", n, max_n_matrix))
    } else {
      dm <- sf::st_distance(pts)
      greatest_distance <- max(dm)
      
      if (nearest) {
        m <- as.matrix(dm)
        dnum <- as.numeric(m)
        dim(dnum) <- dim(m)
        diag(dnum) <- Inf
        nearest_index <- apply(dnum, 1L, which.min)
      }
    }
  }
  
  if (drop_units && requireNamespace("units", quietly = TRUE)) {
    if (!is.null(dm)) dm <- units::drop_units(dm)
    if (!is.na(greatest_distance)[1]) greatest_distance <- units::drop_units(greatest_distance)
  }
  
  list(
    centroid = centroid,
    convex_hull = hull,
    distance_matrix = dm,
    nearest_index = nearest_index,
    greatest_distance = greatest_distance,
    n_points = n
  )
}

# -----------------------
# Quick per-column stats
# -----------------------

#' Column statistics for a data frame
#'
#' Computes null counts, unique counts, and basic range/length summaries per column.
#'
#' @param data data.frame
#' @return data.table with one row per column
#' @export
col_stats <- function(data) {
  .require_pkg("data.table")
  if (!is.data.frame(data)) .stopf("data must be a data.frame.")
  
  dt <- data.table::as.data.table(data)
  n_rows <- nrow(dt)
  
  out <- lapply(names(dt), function(col) {
    x <- dt[[col]]
    nulls <- sum(is.na(x))
    pct_non_null <- round(100 * .safe_div(n_rows - nulls, n_rows), 2)
    uniq <- data.table::uniqueN(x, na.rm = FALSE)
    
    type <- if (inherits(x, "Date") || inherits(x, "POSIXt")) "date" else
      if (is.numeric(x)) "numeric" else
        if (is.factor(x)) "factor" else
          if (is.character(x)) "character" else class(x)[1L]
    
    highest <- NA_character_
    lowest <- NA_character_
    if (is.numeric(x) || inherits(x, "Date")) {
      highest <- as.character(.safe_max(x))
      lowest  <- as.character(.safe_min(x))
    } else if (is.character(x)) {
      xx <- x[!is.na(x)]
      if (!.is_empty(xx)) {
        highest <- max(xx)
        lowest  <- min(xx)
      }
    }
    
    avg_len <- max_len <- min_len <- NA_real_
    if (is.character(x)) {
      nx <- nchar(x)
      avg_len <- .safe_mean(nx)
      max_len <- .safe_max(nx)
      min_len <- .safe_min(nx)
    }
    
    data.table::data.table(
      column = col,
      type = type,
      n = n_rows,
      nulls = nulls,
      pct_non_null = pct_non_null,
      unique_levels = uniq,
      highest_value = highest,
      lowest_value = lowest,
      avg_length = avg_len,
      max_length = max_len,
      min_length = min_len
    )
  })
  
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

# -----------------------
# Numeric stats
# -----------------------

#' Numeric descriptive statistics
#'
#' Computes common descriptive statistics from a numeric vector, including
#' mean, median, standard deviation, quantiles, and sums. Skewness and kurtosis
#' are included when the moments package is available and moments=TRUE.
#'
#' @param x Numeric vector (or coercible when coerce=TRUE).
#' @param coerce Convert input to numeric using to_numeric().
#' @param moments Compute skewness/kurtosis when moments is available.
#'
#' @return Named numeric vector.
#' @export
num_stats <- function(x, coerce = FALSE, moments = TRUE) {
  if (coerce && !is.numeric(x)) x <- to_numeric(x)
  if (!is.numeric(x)) .stopf("x must be numeric (or set coerce=TRUE).")
  
  v <- x[!is.na(x)]
  if (.is_empty(v)) {
    return(c(
      Mean = NA_real_, Median = NA_real_, Mode = NA_real_,
      Sample_SD = NA_real_, Pop_SD = NA_real_,
      Kurtosis = NA_real_, Skewness = NA_real_,
      Sum = NA_real_, Abs_Sum = NA_real_,
      Min = NA_real_, Max = NA_real_,
      Q1 = NA_real_, Q3 = NA_real_,
      IQR = NA_real_, MAD = NA_real_,
      N = 0
    ))
  }
  
  n <- length(v)
  mode1 <- mode_value(v, return_type = "first", ties = "first", tie_break = "first")
  
  sample_sd <- if (n >= 2L) stats::sd(v) else NA_real_
  pop_sd <- if (n >= 2L) sample_sd * sqrt((n - 1) / n) else NA_real_
  
  kurt <- skew <- NA_real_
  if (moments && requireNamespace("moments", quietly = TRUE)) {
    kurt <- tryCatch(moments::kurtosis(v), error = function(e) NA_real_)
    skew <- tryCatch(moments::skewness(v), error = function(e) NA_real_)
  }
  
  q1 <- as.numeric(stats::quantile(v, 0.25, names = FALSE, type = 7))
  q3 <- as.numeric(stats::quantile(v, 0.75, names = FALSE, type = 7))
  
  c(
    Mean = mean(v),
    Median = stats::median(v),
    Mode = as.numeric(mode1),
    Sample_SD = sample_sd,
    Pop_SD = pop_sd,
    Kurtosis = kurt,
    Skewness = skew,
    Sum = sum(v),
    Abs_Sum = sum(abs(v)),
    Min = min(v),
    Max = max(v),
    Q1 = q1,
    Q3 = q3,
    IQR = stats::IQR(v),
    MAD = stats::mad(v),
    N = n
  )
}

# -----------------------
# All stats
# -----------------------

#' Statistics for all columns in one or more data frames
#'
#' Computes per-column statistics grouped by inferred or user-provided column type.
#' Results are returned per data frame with separate tables for numeric, factor,
#' date, character, and other columns.
#'
#' @param dfs data.frame or named list of data.frames
#' @param types Optional:
#'   - for a single data.frame: named character vector of column types
#'   - for a list of data.frames: named list of named character vectors
#'   Values in: c("numeric","factor","date","character","other","skip")
#' @param coerce_numeric Convert numeric-mapped columns using to_numeric().
#' @param moments Compute skewness/kurtosis when available.
#'
#' @return Named list of results per data frame.
#' @export
all_stats <- function(dfs, types = NULL, coerce_numeric = TRUE, moments = TRUE) {
  .require_pkg("data.table")
  
  allowed <- c("numeric", "factor", "date", "character", "other", "skip")
  
  if (inherits(dfs, "data.frame")) {
    df_name <- deparse(substitute(dfs))
    dfs <- setNames(list(dfs), df_name)
  }
  
  if (!is.list(dfs) || .is_empty(dfs)) .stopf("dfs must be a data.frame or a non-empty list of data.frames.")
  if (is.null(names(dfs)) || any(names(dfs) == "")) .stopf("dfs must be a named list (or a single data.frame).")
  
  if (!is.null(types) && is.character(types) && !is.list(types)) {
    types <- setNames(list(types), names(dfs)[1])
  }
  
  res <- lapply(names(dfs), function(df_name) {
    df <- dfs[[df_name]]
    if (!is.data.frame(df)) .stopf("Element '%s' is not a data.frame.", df_name)
    
    mapping <- NULL
    
    if (!is.null(types) && !is.null(types[[df_name]])) {
      m <- types[[df_name]]
      if (is.null(names(m))) .stopf("types[['%s']] must be a named character vector.", df_name)
      
      bad <- setdiff(unique(unname(m)), allowed)
      if (length(bad) > 0L) .stopf("Invalid types for '%s': %s", df_name, paste(bad, collapse = ", "))
      
      for (col in intersect(names(m), names(df))) {
        target <- m[[col]]
        if (target == "skip") next
        if (target == "numeric" && coerce_numeric) df[[col]] <- to_numeric(df[[col]])
        if (target == "factor") df[[col]] <- as.factor(df[[col]])
        if (target == "character") df[[col]] <- as.character(df[[col]])
        if (target == "date") df[[col]] <- as_date_safe(df[[col]])
      }
      
      mapping <- m[m != "skip"]
      mapping <- mapping[names(mapping) %in% names(df)]
    }
    
    if (is.null(mapping)) {
      mapping <- vapply(df, function(x) {
        if (inherits(x, "Date") || inherits(x, "POSIXt")) "date" else
          if (is.numeric(x)) "numeric" else
            if (is.factor(x)) "factor" else
              if (is.character(x)) "character" else "other"
      }, character(1))
    }
    
    num_cols <- names(mapping)[mapping == "numeric"]
    fac_cols <- names(mapping)[mapping == "factor"]
    dat_cols <- names(mapping)[mapping == "date"]
    chr_cols <- names(mapping)[mapping == "character"]
    oth_cols <- names(mapping)[mapping == "other"]
    
    num_out <- if (length(num_cols) > 0L) {
      lst <- lapply(num_cols, function(col) {
        st <- num_stats(df[[col]], coerce = FALSE, moments = moments)
        dt <- data.table::as.data.table(as.list(st))
        dt[, Column := col]
        dt[, c("Column", setdiff(names(dt), "Column")), with = FALSE]
      })
      data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    } else NULL
    
    fac_out <- if (length(fac_cols) > 0L) {
      lst <- lapply(fac_cols, function(col) {
        st <- factor_stats(df[[col]], strict = FALSE)
        dt <- data.table::as.data.table(st)
        dt[, Column := col]
        dt[, c("Column", setdiff(names(dt), "Column")), with = FALSE]
      })
      data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    } else NULL
    
    dat_out <- if (length(dat_cols) > 0L) {
      lst <- lapply(dat_cols, function(col) {
        st <- date_stats(df[[col]], middle_as = "character")
        dt <- data.table::as.data.table(st)
        dt[, Column := col]
        dt[, c("Column", setdiff(names(dt), "Column")), with = FALSE]
      })
      data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    } else NULL
    
    chr_out <- if (length(chr_cols) > 0L) {
      lst <- lapply(chr_cols, function(col) {
        x <- as.character(df[[col]])
        nx <- nchar(x)
        data.table::data.table(
          Column = col,
          Nulls = sum(is.na(x)),
          Unique = data.table::uniqueN(x, na.rm = FALSE),
          Avg_Length = .safe_mean(nx),
          Max_Length = .safe_max(nx),
          Min_Length = .safe_min(nx)
        )
      })
      data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    } else NULL
    
    oth_out <- if (length(oth_cols) > 0L) {
      lst <- lapply(oth_cols, function(col) {
        x <- df[[col]]
        data.table::data.table(
          Column = col,
          Nulls = sum(is.na(x)),
          Unique = data.table::uniqueN(x, na.rm = FALSE),
          Class = paste(class(x), collapse = "|")
        )
      })
      data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    } else NULL
    
    list(
      Numeric = num_out,
      Factor = fac_out,
      Date = dat_out,
      Character = chr_out,
      Other = oth_out
    )
  })
  
  names(res) <- names(dfs)
  res
}

# -----------------------
# Group stats
# -----------------------

#' Grouped statistics for a data frame
#'
#' Splits a data frame by a grouping column and computes statistics per group.
#' The grouping column is excluded from group-level computations.
#'
#' @param data data.frame
#' @param group Column name to group by
#' @param na_label Label used for NA group in output names
#' @param ... Passed to all_stats()
#'
#' @return Named list of group -> stats
#' @export
group_stats <- function(data, group, na_label = "NA", ...) {
  .require_pkg("data.table")
  
  if (!is.data.frame(data)) .stopf("data must be a data.frame.")
  if (!is.character(group) || length(group) != 1L) .stopf("group must be a single column name.")
  if (!(group %in% names(data))) .stopf("group column '%s' not present in data.", group)
  
  dt <- data.table::as.data.table(data)
  g <- dt[[group]]
  
  key <- as.character(g)
  key[is.na(g)] <- na_label
  
  idx <- split(seq_len(nrow(dt)), key)
  
  out <- lapply(names(idx), function(k) {
    sub_dt <- dt[idx[[k]], , drop = FALSE]
    sub_dt[, (group) := NULL]
    all_stats(as.data.frame(sub_dt), ...)
  })
  
  names(out) <- names(idx)
  out
}

# -----------------------
# Wrapper
# -----------------------

#' Get common stats in one call
#'
#' Returns a combined result containing optional column summaries, full per-type
#' statistics, and optional grouped statistics.
#'
#' @param data data.frame
#' @param group Optional grouping column name.
#' @param types Optional mapping passed to all_stats().
#' @param quick If TRUE, include col_stats().
#' @param moments If TRUE, compute skewness/kurtosis when available.
#' @param coerce_numeric If TRUE, coerce numeric-mapped columns via to_numeric().
#'
#' @return List with components:
#'   - columns (optional): output from col_stats()
#'   - all: output from all_stats()
#'   - groups (optional): output from group_stats()
#' @export
get_stats <- function(
    data,
    group = NULL,
    types = NULL,
    quick = TRUE,
    moments = TRUE,
    coerce_numeric = TRUE
) {
  if (!is.data.frame(data)) .stopf("data must be a data.frame.")
  
  out <- list()
  
  if (quick) out$columns <- col_stats(data)
  
  out$all <- all_stats(data, types = types, moments = moments, coerce_numeric = coerce_numeric)
  
  if (!is.null(group)) {
    out$groups <- group_stats(
      data,
      group,
      types = types,
      moments = moments,
      coerce_numeric = coerce_numeric
    )
  }
  
  out
}

# -----------------------
# UAT print helper
# -----------------------

#' Print UAT output
#'
#' Prints a simple, consistent display for test results.
#'
#' @param test_name Test name.
#' @param result Result to display.
#' @param expected Optional expected result description.
#' @param note Optional note.
#'
#' @export
uat_print <- function(test_name, result, expected = NULL, note = "") {
  cat(sprintf("TEST: %s\n", test_name))
  if (!is.null(expected)) cat(sprintf("  Expected: %s\n", expected))
  cat(sprintf("  Result:   %s\n", result))
  if (nzchar(note)) cat(sprintf("  Note: %s\n", note))
  cat("------------------------------------------------------\n")
}

# -----------------------
# UAT runner
# -----------------------

#' Run UAT checks
#'
#' Executes a small set of checks that validate expected behavior for the key functions.
#'
#' @param verbose If TRUE, prints test results using uat_print().
#' @return TRUE if all checks pass, otherwise FALSE.
#' @export
run_uat <- function(verbose = TRUE) {
  pass <- TRUE
  
  check <- function(name, ok, got = NULL, expected = NULL) {
    if (!isTRUE(ok)) pass <<- FALSE
    if (verbose) {
      uat_print(
        test_name = name,
        result = if (isTRUE(ok)) "PASS" else paste0("FAIL", if (!is.null(got)) paste0(" (got: ", got, ")") else ""),
        expected = expected
      )
    }
  }
  
  # mode_value
  v <- c(1, 2, 2, 3, 3, NA)
  ms <- mode_value(v, "string")
  mv <- mode_value(v, "vector")
  mf <- mode_value(v, "first", ties = "first")
  mc <- mode_value(v, "count")
  
  check("mode_value(string) contains 2 and 3",
        grepl("2", ms) && grepl("3", ms),
        got = ms,
        expected = "contains both 2 and 3")
  
  check("mode_value(vector) equals c(2,3) ignoring order",
        identical(sort(as.numeric(mv)), c(2, 3)),
        got = paste(mv, collapse = ", "),
        expected = "2, 3")
  
  check("mode_value(first) is 2 or 3",
        mf %in% c(2, 3),
        got = as.character(mf),
        expected = "2 or 3")
  
  check("mode_value(count) == 2",
        identical(mc, 2L),
        got = as.character(mc),
        expected = "2")
  
  # to_numeric
  x1 <- factor(c("1", "2", "3"))
  y1 <- to_numeric(x1)
  check("to_numeric(factor) parses numbers",
        identical(y1, c(1, 2, 3)),
        got = paste(y1, collapse = ", "),
        expected = "1, 2, 3")
  
  x2 <- c("1,000", "2,500", NA)
  y2 <- to_numeric(x2)
  check("to_numeric removes commas",
        identical(y2, c(1000, 2500, NA_real_)),
        got = paste(y2, collapse = ", "),
        expected = "1000, 2500, NA")
  
  x3 <- c("abc", "def")
  y3 <- to_numeric(x3, strict = TRUE)
  check("to_numeric(strict) returns all NA when parsing fails",
        all(is.na(y3)),
        got = paste(y3, collapse = ", "),
        expected = "all NA")
  
  # date_stats
  d <- c("2020-01-01", "2020-01-03", "2020-01-02", NA)
  ds <- date_stats(d, middle_as = "character")
  check("date_stats span == 2",
        identical(ds$span, 2),
        got = as.character(ds$span),
        expected = "2")
  check("date_stats invalid >= 1",
        ds$invalid >= 1,
        got = as.character(ds$invalid),
        expected = ">= 1")
  check("date_stats max_gap == 1",
        identical(ds$max_gap, 1),
        got = as.character(ds$max_gap),
        expected = "1")
  
  # factor_stats
  f <- factor(c("a", "a", "b", NA))
  fs <- factor_stats(f, strict = TRUE, include_na = TRUE)
  check("factor_stats levels_count == 2",
        identical(fs$levels_count, 2L),
        got = as.character(fs$levels_count),
        expected = "2")
  
  # col_stats / all_stats / group_stats
  .require_pkg("data.table")
  df <- data.frame(
    grp = c("g1", "g1", "g2", NA),
    num = c(1, 2, NA, 4),
    chr = c("x", "yy", NA, "zzz"),
    stringsAsFactors = FALSE
  )
  
  cs <- col_stats(df)
  check("col_stats returns one row per column",
        nrow(cs) == ncol(df),
        got = as.character(nrow(cs)),
        expected = as.character(ncol(df)))
  
  as1 <- all_stats(df)
  check("all_stats returns list with expected keys",
        all(c("Numeric", "Factor", "Date", "Character", "Other") %in% names(as1[[1]])),
        got = paste(names(as1[[1]]), collapse = ", "),
        expected = "Numeric, Factor, Date, Character, Other")
  
  gs <- group_stats(df, "grp")
  check("group_stats returns groups including NA label",
        all(c("g1", "g2", "NA") %in% names(gs)),
        got = paste(names(gs), collapse = ", "),
        expected = "g1, g2, NA")
  
  # gis_stats (optional)
  if (requireNamespace("sf", quietly = TRUE)) {
    coords <- data.frame(longitude = c(-87.62, -87.63), latitude = c(41.88, 41.89))
    gss <- gis_stats(coords, distance_matrix = TRUE, nearest = TRUE, max_n_matrix = 1000L)
    check("gis_stats returns n_points == 2",
          identical(gss$n_points, 2L),
          got = as.character(gss$n_points),
          expected = "2")
    check("gis_stats nearest_index length == 2",
          length(gss$nearest_index) == 2L,
          got = as.character(length(gss$nearest_index)),
          expected = "2")
  } else {
    if (verbose) cat("NOTE: sf not installed; skipping gis_stats checks.\n")
  }
  
  if (verbose) cat(if (pass) "\nALL UAT CHECKS PASSED\n" else "\nSOME UAT CHECKS FAILED\n")
  pass
}
