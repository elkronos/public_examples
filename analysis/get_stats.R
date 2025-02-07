# --- Calculate the Mode(s) of a Vector ---
#' Calculate the Mode(s) of a Vector
#'
#' This function computes the mode(s) of an input vector by removing any missing values and then
#' identifying the element(s) that occur most frequently. Depending on the `return_type` parameter,
#' the mode(s) are returned as a concatenated string, a numeric vector, the first mode, or a count of modes.
#'
#' @param x A numeric or factor vector. NA values are removed prior to computation.
#' @param return_type A character string specifying the format of the returned value. Options include:
#'   \itemize{
#'     \item \code{"string"}: Returns the modes as a comma-separated string.
#'     \item \code{"vector"}: Returns the modes as a numeric vector.
#'     \item \code{"first"}: Returns the first mode found as a numeric value.
#'     \item \code{"count"}: Returns the number of modes.
#'   }
#'
#' @return Depending on \code{return_type}, one of:
#'   \itemize{
#'     \item A string with the mode(s).
#'     \item A numeric vector of mode(s).
#'     \item A numeric value representing the first mode.
#'     \item A numeric value representing the count of modes.
#'   }
#'
#' @export
calculate_mode <- function(x, return_type = "string") {
  valid_return_types <- c("string", "vector", "first", "count")
  if (!(return_type %in% valid_return_types)) {
    stop("Invalid return_type. Choose from: ", paste(valid_return_types, collapse = ", "))
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  freq <- table(x)
  modes <- names(freq)[freq == max(freq)]
  if (return_type == "vector") {
    return(as.numeric(as.character(modes)))
  } else if (return_type == "first") {
    return(as.numeric(as.character(modes[1])))
  } else if (return_type == "count") {
    return(length(modes))
  } else {
    return(paste(modes, collapse = ", "))
  }
}

# --- Safely Convert Input to a Numeric Vector ---
#' Safely Convert Input to a Numeric Vector
#'
#' This function attempts to convert an input vector to numeric. It handles inputs that are already numeric,
#' factors, or character vectors. For factors, the conversion is done via character conversion. If the conversion
#' fails, an NA vector is returned.
#'
#' @param x The input vector to be converted. It can be numeric, factor, or character.
#'
#' @return A numeric vector if conversion is successful; otherwise, a vector of NA values of the same length as \code{x}.
#'
#' @export
safe_numeric <- function(x) {
  if (DEBUG) {
    flog.info("safe_numeric: Original class: %s", class(x))
    flush.console()
    flog.info("safe_numeric: Head: %s", paste(utils::head(x), collapse = ", "))
    flush.console()
    flog.debug("safe_numeric: Structure:")
    str(x)
    flush.console()
  }
  if (is.numeric(x)) {
    flog.info("safe_numeric: Already numeric.")
    flush.console()
    return(x)
  }
  if (is.factor(x)) {
    flog.info("safe_numeric: Converting factor via as.character.")
    flush.console()
    y <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(y)) && !all(is.na(x))) {
      flog.error("safe_numeric: Conversion from factor resulted in all NAs.")
      flush.console()
      return(rep(NA_real_, length(x)))
    }
    return(y)
  }
  if (is.character(x)) {
    flog.info("safe_numeric: Attempting conversion from character.")
    flush.console()
    y <- suppressWarnings(as.numeric(x))
    if ((all(is.na(y)) && any(!is.na(x))) || length(y) == 0) {
      flog.error("safe_numeric: Conversion from character failed; input head: %s", paste(utils::head(x), collapse = ", "))
      flush.console()
      return(rep(NA_real_, length(x)))
    }
    return(y)
  }
  flog.error("safe_numeric: Unhandled type; returning NA vector.")
  flush.console()
  rep(NA_real_, length(x))
}

# --- Compute Date Statistics from a Vector of Dates ---
#' Compute Date Statistics from a Vector of Dates
#'
#' This function calculates various statistics for a vector of dates, such as the span between the minimum and maximum dates,
#' the number of invalid (NA) entries, the middle date, average and median gaps between consecutive dates, and details on streaks (longest and shortest)
#' as well as the maximum and minimum gaps. The input is coerced to \code{Date} if necessary.
#'
#' @param dates A vector containing date values or date-convertable strings.
#'
#' @return A list with the following elements:
#'   \item{span}{The numeric difference (in days) between the maximum and minimum valid dates.}
#'   \item{invalid}{The count of NA values in the original input.}
#'   \item{middle_date}{The date representing the middle of the span.}
#'   \item{avg_gap}{The average gap (in days) between consecutive dates.}
#'   \item{median_gap}{The median gap (in days) between consecutive dates.}
#'   \item{longest_streak}{The longest consecutive streak of dates (days with a one-day gap) plus one (to count the start date).}
#'   \item{shortest_streak}{The shortest consecutive streak of dates (days with a one-day gap) plus one.}
#'   \item{max_gap}{The maximum gap (in days) between consecutive dates.}
#'   \item{min_gap}{The minimum gap (in days) between consecutive dates.}
#'
#' @export
get_date_stats <- function(dates) {
  assertthat::assert_that(is.vector(dates), msg = "dates must be a vector")
  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  invalid_count <- sum(is.na(dates))
  valid_dates <- sort(unique(dates[!is.na(dates)]))
  if (length(valid_dates) < 2) {
    warning("Not enough valid dates to compute gaps and streaks.")
    return(list(span = NA, invalid = invalid_count, middle_date = NA,
                avg_gap = NA, median_gap = NA,
                longest_streak = NA, shortest_streak = NA,
                max_gap = NA, min_gap = NA))
  }
  span <- as.numeric(max(valid_dates) - min(valid_dates))
  middle_date <- min(valid_dates) + span / 2
  gaps <- as.numeric(diff(valid_dates))
  median_gap <- median(gaps)
  # Calculate consecutive date streaks (where gap equals 1 day)
  r <- rle(gaps == 1)
  if (any(r$values)) {
    longest_streak <- max(r$lengths[r$values]) + 1
    shortest_streak <- min(r$lengths[r$values]) + 1
  } else {
    longest_streak <- NA
    shortest_streak <- NA
  }
  list(
    span = span,
    invalid = invalid_count,
    middle_date = as.character(middle_date),
    avg_gap = mean(gaps),
    median_gap = median_gap,
    longest_streak = longest_streak,
    shortest_streak = shortest_streak,
    max_gap = max(gaps),
    min_gap = min(gaps)
  )
}

# --- Get Statistics for a Factor Variable ---
#' Get Statistics for a Factor Variable
#'
#' This function returns a list of statistics for a factor variable including the number of levels,
#' a formatted string of observation counts per level, the percentage of observations per level,
#' and the mode (using \code{calculate_mode()}).
#'
#' @param factor_col A factor vector. Must not be \code{NULL}.
#'
#' @return A list with the following elements:
#'   \item{levels_count}{Number of levels in the factor.}
#'   \item{obs_count}{A string summarizing the count of observations per level.}
#'   \item{obs_perc}{A string representing the percentage of observations per level.}
#'   \item{mode}{The mode of the factor as computed by \code{calculate_mode()}.}
#'
#' @export
get_factor_stats <- function(factor_col) {
  assertthat::assert_that(!is.null(factor_col), msg = "factor_col must not be NULL")
  if (!is.factor(factor_col)) stop("Input must be a factor vector.")
  obs_count <- table(factor_col, useNA = "ifany")
  list(
    levels_count = length(levels(factor_col)),
    obs_count = paste(names(obs_count), obs_count, sep = ": ", collapse = "; "),
    obs_perc = paste(round(100 * prop.table(obs_count), 2), collapse = ", "),
    mode = calculate_mode(factor_col)
  )
}

# --- Compute Geographic Statistics from Coordinate Data ---
#' Compute Geographic Statistics from Coordinate Data
#'
#' This function calculates various geographic statistics from a data frame containing coordinates.
#' The statistics include the centroid of all points, a distance matrix between points, the nearest neighboring point
#' for each coordinate, the convex hull, and the greatest distance among points. Coordinates can be transformed
#' into a projected CRS if desired.
#'
#' @param coords_df A data frame containing coordinate columns.
#' @param lon_var A character string indicating the column name for longitude (case-insensitive). Default is \code{"longitude"}.
#' @param lat_var A character string indicating the column name for latitude (case-insensitive). Default is \code{"latitude"}.
#' @param use_projected Logical. If \code{TRUE}, the points are transformed into the projected CRS specified by \code{output_crs}. Default is \code{TRUE}.
#' @param input_crs An integer specifying the input Coordinate Reference System (CRS) (e.g., 4326 for WGS84). Default is 4326.
#' @param output_crs An integer specifying the output (projected) CRS (e.g., 3857 for Web Mercator). Default is 3857.
#'
#' @return A list containing:
#'   \item{centroid}{An \code{sf} object representing the centroid of the union of all points.}
#'   \item{distance_matrix}{A matrix of distances between each pair of points.}
#'   \item{nearest_points}{A list where each element is an \code{sf} object representing the nearest point for the corresponding input point.}
#'   \item{convex_hull}{An \code{sf} object representing the convex hull around the points.}
#'   \item{greatest_distance}{The maximum distance found among the points.}
#'
#' @export
get_gis_stats <- function(coords_df, lon_var = "longitude", lat_var = "latitude",
                          use_projected = TRUE, input_crs = 4326, output_crs = 3857) {
  assertthat::assert_that(is.data.frame(coords_df), msg = "coords_df must be a data frame")
  cols_lower <- tolower(names(coords_df))
  assertthat::assert_that(lon_var %in% cols_lower, lat_var %in% cols_lower,
                          msg = "Coordinates data frame must contain columns for longitude and latitude")
  lon_name <- names(coords_df)[which(tolower(names(coords_df)) == lon_var)][1]
  lat_name <- names(coords_df)[which(tolower(names(coords_df)) == lat_var)][1]
  
  sf_points <- sf::st_as_sf(coords_df, coords = c(lon_name, lat_name), crs = input_crs, remove = FALSE)
  if (use_projected) sf_points <- sf::st_transform(sf_points, crs = output_crs)
  
  centroid <- sf::st_centroid(sf::st_union(sf_points))
  distance_matrix <- sf::st_distance(sf_points)
  nearest_points <- lapply(1:nrow(sf_points), function(i) {
    d <- as.numeric(sf::st_distance(sf_points[i, ], sf_points))
    d[i] <- Inf
    nearest_index <- which.min(d)
    sf::st_nearest_points(sf_points[i, ], sf_points[nearest_index, ])
  })
  convex_hull <- sf::st_convex_hull(sf::st_union(sf_points))
  greatest_distance <- max(distance_matrix)
  
  list(
    centroid = centroid,
    distance_matrix = distance_matrix,
    nearest_points = nearest_points,
    convex_hull = convex_hull,
    greatest_distance = greatest_distance
  )
}

# --- Compute Multiple Column Statistics for a Data Frame ---
#' Compute Multiple Column Statistics for a Data Frame
#'
#' This function computes various statistics for each column in a data frame.
#' For numeric columns: it returns null count, percentage of non-null values, unique level count,
#' and highest/lowest values. For character columns: it computes null count, unique count, and statistics on string lengths.
#' Factor columns are handled via \code{get_factor_stats()}.
#'
#' @param data A data frame for which the column statistics will be computed.
#'
#' @return A \code{data.table} object containing a row per column with computed statistics.
#'
#' @export
get_multi_stats <- function(data) {
  assertthat::assert_that(is.data.frame(data), msg = "data must be a data frame")
  dt <- data.table::as.data.table(data)
  stats_list <- lapply(names(dt), function(col) {
    x <- dt[[col]]
    data.table::data.table(
      column = col,
      nulls = sum(is.na(x)),
      pct_non_null = round(100 * (length(x) - sum(is.na(x))) / length(x), 2),
      unique_levels = length(unique(x)),
      highest_value = if (is.numeric(x) || is.character(x)) as.character(max(x, na.rm = TRUE)) else NA_character_,
      lowest_value = if (is.numeric(x) || is.character(x)) as.character(min(x, na.rm = TRUE)) else NA_character_,
      avg_length = if (is.character(x)) mean(nchar(x), na.rm = TRUE) else NA_real_,
      max_length = if (is.character(x)) max(nchar(x), na.rm = TRUE) else NA_real_,
      min_length = if (is.character(x)) min(nchar(x), na.rm = TRUE) else NA_real_
    )
  })
  data.table::rbindlist(stats_list, use.names = TRUE, fill = TRUE)
}

# --- Compute Descriptive Statistics for a Numeric Vector ---
#' Compute Descriptive Statistics for a Numeric Vector
#'
#' This function calculates descriptive statistics for a numeric vector including mean, median,
#' mode, sample and population standard deviation, kurtosis, skewness, sum, and absolute sum.
#' In addition, it returns the minimum, maximum, first and third quartiles, interquartile range (IQR),
#' and the median absolute deviation (MAD). If the input vector contains only NA values, all returned statistics will be NA.
#'
#' @param num_vec A numeric vector. NA values are ignored.
#'
#' @return A named numeric vector containing the following statistics:
#'   \itemize{
#'     \item \code{Mean}
#'     \item \code{Median}
#'     \item \code{Mode} (first mode if multiple exist)
#'     \item \code{Sample_SD}
#'     \item \code{Pop_SD}
#'     \item \code{Kurtosis}
#'     \item \code{Skewness}
#'     \item \code{Sum}
#'     \item \code{Abs_Sum}
#'     \item \code{Min}
#'     \item \code{Max}
#'     \item \code{Q1} (25th percentile)
#'     \item \code{Q3} (75th percentile)
#'     \item \code{IQR} (interquartile range)
#'     \item \code{MAD} (median absolute deviation)
#'   }
#'
#' @export
get_numeric_stats <- function(num_vec) {
  if (DEBUG) {
    flog.info("get_numeric_stats: Received vector (head): %s", paste(utils::head(num_vec), collapse = ", "))
    flush.console()
    flog.debug("get_numeric_stats: Structure:")
    str(num_vec)
    flush.console()
  }
  assertthat::assert_that(is.numeric(num_vec), msg = "num_vec must be numeric")
  if (all(is.na(num_vec))) {
    return(c(Mean = NA, Median = NA, Mode = NA, Sample_SD = NA,
             Pop_SD = NA, Kurtosis = NA, Skewness = NA, Sum = NA, Abs_Sum = NA,
             Min = NA, Max = NA, Q1 = NA, Q3 = NA, IQR = NA, MAD = NA))
  }
  n_non_na <- sum(!is.na(num_vec))
  mode_val <- calculate_mode(num_vec)
  c(
    Mean = mean(num_vec, na.rm = TRUE),
    Median = median(num_vec, na.rm = TRUE),
    Mode = as.numeric(as.character(mode_val)),
    Sample_SD = sd(num_vec, na.rm = TRUE),
    Pop_SD = sd(num_vec, na.rm = TRUE) * sqrt((n_non_na - 1) / n_non_na),
    Kurtosis = moments::kurtosis(num_vec, na.rm = TRUE),
    Skewness = moments::skewness(num_vec, na.rm = TRUE),
    Sum = sum(num_vec, na.rm = TRUE),
    Abs_Sum = sum(abs(num_vec), na.rm = TRUE),
    Min = min(num_vec, na.rm = TRUE),
    Max = max(num_vec, na.rm = TRUE),
    Q1 = as.numeric(quantile(num_vec, 0.25, na.rm = TRUE, names = FALSE)),
    Q3 = as.numeric(quantile(num_vec, 0.75, na.rm = TRUE, names = FALSE)),
    IQR = IQR(num_vec, na.rm = TRUE),
    MAD = mad(num_vec, na.rm = TRUE)
  )
}

# --- Compute Statistics for All Columns in One or More Data Frames ---
#' Compute Statistics for All Columns in One or More Data Frames
#'
#' This function computes statistics for each column in a data frame or a list of data frames.
#' It first determines or applies a user-specified mapping for each column type (e.g., numeric, factor,
#' date, character, or other). Based on this mapping, it calls helper functions (\code{get_numeric_stats()},
#' \code{get_factor_stats()}, \code{get_date_stats()}, etc.) to compute the appropriate statistics.
#'
#' @param dfs Either a single data frame or a named list of data frames.
#' @param col_types An optional named list that provides manual type mappings for columns in each data frame.
#'   If provided, the mapping should be a named character vector where the names correspond to column names
#'   and the values are one of \code{"numeric"}, \code{"factor"}, \code{"date"}, or \code{"character"}.
#'
#' @return A named list where each element corresponds to a data frame and contains a list with components:
#'   \itemize{
#'     \item \code{Numeric}: Statistics for numeric columns.
#'     \item \code{Factor}: Statistics for factor columns.
#'     \item \code{Date}: Statistics for date columns.
#'     \item \code{Character}: Statistics for character columns.
#'     \item \code{Other}: Statistics for columns that do not fall into the above categories.
#'   }
#'
#' @export
get_all_stats <- function(dfs, col_types = NULL) {
  if (inherits(dfs, "data.frame")) {
    df_name <- deparse(substitute(dfs))
    original_df <- dfs
    dfs <- list()
    dfs[[df_name]] <- original_df
  }
  result <- lapply(names(dfs), function(df_name) {
    flog.info("Processing data frame: %s", df_name)
    flush.console()
    df <- dfs[[df_name]]
    if (!is.null(col_types) && !is.null(col_types[[df_name]])) {
      mapping_manual <- col_types[[df_name]]
      flog.info("Manual mapping for %s: %s", df_name,
                paste(names(mapping_manual), mapping_manual, sep = "=", collapse = ", "))
      flush.console()
      for (col in names(mapping_manual)) {
        if (col %in% names(df)) {
          target_type <- mapping_manual[[col]]
          flog.info("Processing column %s with target type %s", col, target_type)
          flog.info("Before conversion, head: %s", paste(utils::head(df[[col]]), collapse = ", "))
          flog.debug("Structure:")
          str(df[[col]])
          flush.console()
          if (target_type == "numeric") {
            conv <- safe_numeric(df[[col]])
            if (!is.numeric(conv) || (all(is.na(conv)) && !all(is.na(df[[col]])))) {
              flog.error("Column %s failed conversion to numeric; skipping.", col)
              flush.console()
              mapping_manual[[col]] <- "skip"
            } else {
              df[[col]] <- conv
            }
          } else if (target_type == "factor") {
            df[[col]] <- as.factor(df[[col]])
          } else if (target_type == "date") {
            df[[col]] <- tryCatch(as.Date(df[[col]]), error = function(e) NA)
          } else if (target_type == "character") {
            df[[col]] <- as.character(df[[col]])
          }
          flog.info("After conversion, head of %s: %s", col, paste(utils::head(df[[col]]), collapse = ", "))
          flog.debug("Structure:")
          str(df[[col]])
          flush.console()
        }
      }
      mapping <- mapping_manual[mapping_manual != "skip"]
    } else {
      mapping <- sapply(df, function(x) {
        if (inherits(x, "Date") || inherits(x, "POSIXt")) "date" else
          if (is.numeric(x)) "numeric" else
            if (is.factor(x)) "factor" else
              if (is.character(x)) "character" else "other"
      })
      flog.info("Inferred mapping for %s: %s", df_name,
                paste(names(mapping), mapping, sep = "=", collapse = ", "))
      flush.console()
    }
    numeric_cols <- names(mapping)[mapping == "numeric"]
    factor_cols  <- names(mapping)[mapping == "factor"]
    date_cols    <- names(mapping)[mapping == "date"]
    char_cols    <- names(mapping)[mapping == "character"]
    other_cols   <- names(mapping)[mapping == "other"]
    numeric_stats <- if (length(numeric_cols) > 0) {
      ns_list <- lapply(numeric_cols, function(col) {
        stats <- get_numeric_stats(df[[col]])
        data.table::as.data.table(cbind(Column = col, t(as.data.frame(stats))))
      })
      data.table::rbindlist(ns_list, use.names = TRUE, fill = TRUE)
    } else NULL
    factor_stats <- if (length(factor_cols) > 0) {
      fs_list <- lapply(factor_cols, function(col) {
        stats <- get_factor_stats(df[[col]])
        dt <- data.table::as.data.table(as.list(sapply(stats, as.character)))
        dt[, Column := col]
        dt[, c("Column", setdiff(names(dt), "Column")), with = FALSE]
      })
      data.table::rbindlist(fs_list, use.names = TRUE, fill = TRUE)
    } else NULL
    date_stats <- if (length(date_cols) > 0) {
      ds_list <- lapply(date_cols, function(col) {
        stats <- get_date_stats(df[[col]])
        dt <- data.table::as.data.table(as.list(sapply(stats, as.character)))
        dt[, Column := col]
        dt[, c("Column", setdiff(names(dt), "Column")), with = FALSE]
      })
      data.table::rbindlist(ds_list, use.names = TRUE, fill = TRUE)
    } else NULL
    char_stats <- if (length(char_cols) > 0) {
      cs_list <- lapply(char_cols, function(col) {
        x <- as.character(df[[col]])
        data.table::data.table(
          Column = col,
          Nulls = sum(is.na(x)),
          Unique = length(unique(x)),
          Avg_Length = mean(nchar(x), na.rm = TRUE),
          Max_Length = max(nchar(x), na.rm = TRUE),
          Min_Length = min(nchar(x), na.rm = TRUE)
        )
      })
      data.table::rbindlist(cs_list, use.names = TRUE, fill = TRUE)
    } else NULL
    other_stats <- if (length(other_cols) > 0) {
      os_list <- lapply(other_cols, function(col) {
        x <- df[[col]]
        data.table::data.table(
          Column = col,
          Nulls = sum(is.na(x)),
          Unique = length(unique(x))
        )
      })
      data.table::rbindlist(os_list, use.names = TRUE, fill = TRUE)
    } else NULL
    list(
      Numeric = numeric_stats,
      Factor = factor_stats,
      Date = date_stats,
      Character = char_stats,
      Other = other_stats
    )
  })
  names(result) <- names(dfs)
  return(result)
}

# --- Compute Group-Based Statistics for a Data Frame ---
#' Compute Group-Based Statistics for a Data Frame
#'
#' This function computes statistics for a data frame grouped by a specified column.
#' For each group, detailed debug information is logged at each processing step:
#' the input is converted to a data.table, rows are subset using indices obtained via \code{which()},
#' the subset is converted to a data frame, the grouping column is removed,
#' and finally \code{get_all_stats()} is called on the resulting subset to compute statistics.
#'
#' @param data A data frame containing the data.
#' @param group A character string specifying the name of the column to group by.
#'
#' @return A named list where each element corresponds to a group from the \code{group} column,
#' and the value is the output of \code{get_all_stats()} for that subgroup.
#'
#' @details Detailed debug logging is performed using \code{futile.logger} to facilitate troubleshooting.
#'
#' @export
get_group_stats <- function(data, group) {
  # Validate input
  assertthat::assert_that(is.data.frame(data), msg = "data must be a data frame")
  assertthat::assert_that(group %in% names(data), msg = "group column must be present in data")
  
  futile.logger::flog.info("Converting input data to data.table.")
  dt <- data.table::as.data.table(data)
  futile.logger::flog.info("Data.table conversion complete. Number of rows: %s", nrow(dt))
  
  # Get unique groups and log them
  groups <- unique(dt[[group]])
  futile.logger::flog.info("Unique groups in column '%s': %s", group, paste(groups, collapse = ", "))
  
  # Process each group using which() for subsetting
  group_stats <- lapply(groups, function(g) {
    futile.logger::flog.info("Processing group: %s", g)
    
    # Extract the grouping column as a vector to avoid indexing issues
    group_col <- dt[[group]]
    futile.logger::flog.debug("Group column vector: %s", paste(group_col, collapse = ", "))
    
    # Determine the indices for the current group (handle NA groups explicitly)
    if (is.na(g)) {
      futile.logger::flog.info("Group is NA. Subsetting rows where '%s' is NA.", group)
      subset_indices <- which(is.na(group_col))
    } else {
      futile.logger::flog.info("Subsetting rows where '%s' equals %s.", group, g)
      subset_indices <- which(group_col == g)
    }
    futile.logger::flog.info("Subset indices for group %s: %s", g, paste(subset_indices, collapse = ", "))
    
    # Subset dt using the indices
    subset_dt <- dt[subset_indices]
    futile.logger::flog.info("Subset data.table for group %s has %s rows.", g, nrow(subset_dt))
    futile.logger::flog.debug("Structure of subset data.table for group %s:", g)
    str(subset_dt)
    
    # Convert the subset to a data.frame for easier column removal
    futile.logger::flog.info("Converting subset data.table for group %s to data.frame.", g)
    subset_df <- as.data.frame(subset_dt)
    futile.logger::flog.info("Data.frame conversion complete. Dimensions: %s x %s", 
                             nrow(subset_df), ncol(subset_df))
    
    # Remove the grouping column using base R subsetting
    futile.logger::flog.info("Removing grouping column '%s' from subset.", group)
    subset_df <- subset_df[, setdiff(names(subset_df), group), drop = FALSE]
    futile.logger::flog.info("After removal, dimensions: %s x %s", nrow(subset_df), ncol(subset_df))
    futile.logger::flog.debug("Final subset data.frame for group %s:", g)
    str(subset_df)
    
    # Compute statistics for the subset using get_all_stats()
    futile.logger::flog.info("Computing stats for group %s using get_all_stats().", g)
    stats <- get_all_stats(subset_df)
    futile.logger::flog.info("Stats computed for group %s.", g)
    
    return(stats)
  })
  
  names(group_stats) <- as.character(groups)
  futile.logger::flog.info("Group stats computation complete for groups: %s", 
                           paste(names(group_stats), collapse = ", "))
  return(group_stats)
}

# --- Helper Function for UAT Output ---
#' Print UAT (User Acceptance Testing) Output
#'
#' This helper function prints formatted output for UAT tests.
#'
#' @param test_name A character string specifying the test name.
#' @param result A character string representing the result of the test.
#' @param expected An optional character string describing the expected result.
#' @param note An optional note to provide additional information.
#'
#' @export
print_uat <- function(test_name, result, expected = NULL, note = "") {
  cat(sprintf("TEST: %s\n", test_name))
  if (!is.null(expected)) {
    cat(sprintf("  Expected: %s\n", expected))
  }
  cat(sprintf("  Result:   %s\n", result))
  if (note != "") {
    cat(sprintf("  Note: %s\n", note))
  }
  cat("------------------------------------------------------\n")
}

# --- Library Loads and Logger Setup ---
library(moments)
library(sf)
library(assertthat)
library(futile.logger)
library(data.table)

# Set up a global debug flag and logging configuration
DEBUG <- TRUE
futile.logger::flog.threshold(INFO)
futile.logger::flog.appender(futile.logger::appender.console())

# --- Run User Acceptance Tests (UAT) ---
#' Run User Acceptance Tests (UAT)
#'
#' This script block runs a series of UAT tests for the functions defined in the package.
#' It tests each function with different inputs and prints out formatted results.
#' The UAT tests include checks for correct output, error handling, and type conversions.
#' Detailed debug output is provided to facilitate troubleshooting.
#'
#' @examples
#' \dontrun{
#'   # Run the UAT tests:
#'   all_tests_passed <- TRUE
#'
#'   cat("==== Testing calculate_mode() ====\n")
#'   vec <- c(1, 2, 2, 3, 3, NA)
#'   res_string <- calculate_mode(vec, return_type = "string")
#'   res_vector <- calculate_mode(vec, return_type = "vector")
#'   res_first  <- calculate_mode(vec, return_type = "first")
#'   res_count  <- calculate_mode(vec, return_type = "count")
#'
#'   if (grepl("2", res_string) && grepl("3", res_string)) {
#'     print_uat("calculate_mode (string)", res_string, "contains both 2 and 3")
#'   } else {
#'     print_uat("calculate_mode (string)", res_string, "contains both 2 and 3", "FAIL")
#'     all_tests_passed <- FALSE
#'   }
#'
#'   if (all(sort(res_vector) == c(2, 3))) {
#'     print_uat("calculate_mode (vector)", paste(res_vector, collapse = ", "), "2, 3")
#'   } else {
#'     print_uat("calculate_mode (vector)", paste(res_vector, collapse = ", "), "2, 3", "FAIL")
#'     all_tests_passed <- FALSE
#'   }
#'
#'   if (res_first %in% c(2, 3)) {
#'     print_uat("calculate_mode (first)", res_first, "2 or 3")
#'   } else {
#'     print_uat("calculate_mode (first)", res_first, "2 or 3", "FAIL")
#'     all_tests_passed <- FALSE
#'   }
#'
#'   if (res_count == 2) {
#'     print_uat("calculate_mode (count)", res_count, "2")
#'   } else {
#'     print_uat("calculate_mode (count)", res_count, "2", "FAIL")
#'     all_tests_passed <- FALSE
#'   }
#'
#'   # Additional tests for safe_numeric(), get_date_stats(), get_factor_stats(), get_gis_stats(),
#'   # get_multi_stats(), get_numeric_stats(), get_all_stats(), and get_group_stats() are included.
#'
#'   if (all_tests_passed) {
#'     cat("\n===== ALL UAT TESTS PASSED =====\n")
#'   } else {
#'     cat("\n===== SOME UAT TESTS FAILED =====\n")
#'   }
#' }
#'
#' @export
NULL
