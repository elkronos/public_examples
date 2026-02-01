# ------------------------------------------------------------------------------
# FUZZY MATCHING WORKFLOW UTILITIES
# ------------------------------------------------------------------------------

# Notes
# - These functions support repeatable matching workflows:
#   1) optional text preprocessing
#   2) fuzzy matching across one or more metrics
#   3) optional top-N selection per query & method
#   4) summary statistics across query-candidate pairs
# - All functions use explicit namespaces (pkg::fn) so packages do not need to be attached.

# ------------------------------------------------------------------------------
# DEPENDENCIES
# ------------------------------------------------------------------------------

#' Ensure required packages are available
#'
#' @param pkgs Character vector of package names.
#' @return Invisibly TRUE if all packages are available.
ensure_packages <- function(pkgs = c("stringdist", "stringr")) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# TEXT PREPROCESSING
# ------------------------------------------------------------------------------

#' Escape regex metacharacters in a literal string
#'
#' @param x Character vector.
#' @return Character vector safe for regex patterns.
escape_regex <- function(x) {
  stringr::str_replace_all(x, "([\\\\.\\^\\$\\|\\(\\)\\[\\]\\{\\}\\*\\+\\?])", "\\\\\\1")
}

#' Text Preprocessing
#'
#' This function performs preprocessing on a given character vector. It can remove
#' specified strings, remove strings at the beginning or end, convert to lowercase,
#' remove punctuation, normalize whitespace, and optionally transliterate to ASCII.
#'
#' @param text_column Character vector of text data.
#' @param remove_strings Character vector of strings/patterns to remove anywhere.
#' @param remove_start Character vector of strings/patterns to remove from start.
#' @param remove_end Character vector of strings/patterns to remove from end.
#' @param to_lowercase Logical; convert to lowercase.
#' @param remove_punctuation Logical; remove punctuation.
#' @param strip_whitespace Logical; squish internal whitespace and trim ends.
#' @param ascii_transliterate Logical; convert accented characters to closest ASCII.
#' @param pattern_type One of c("fixed","regex"). Controls how remove_* values are interpreted.
#' @param punctuation_replacement Replacement string for punctuation removal (default "").
#'
#' @return Character vector.
#'
#' @export
text_preprocessing <- function(text_column,
                               remove_strings = NULL,
                               remove_start = NULL,
                               remove_end = NULL,
                               to_lowercase = TRUE,
                               remove_punctuation = TRUE,
                               strip_whitespace = TRUE,
                               ascii_transliterate = FALSE,
                               pattern_type = c("fixed", "regex"),
                               punctuation_replacement = "") {
  ensure_packages(c("stringr"))
  
  pattern_type <- match.arg(pattern_type)
  
  if (is.factor(text_column)) text_column <- as.character(text_column)
  if (!is.character(text_column)) stop("text_column must be a character vector.", call. = FALSE)
  
  x <- text_column
  
  if (ascii_transliterate) {
    x <- ifelse(is.na(x), NA_character_, iconv(x, from = "", to = "ASCII//TRANSLIT"))
  }
  
  # Remove specified strings/patterns anywhere
  if (!is.null(remove_strings) && length(remove_strings) > 0) {
    for (p in remove_strings) {
      if (pattern_type == "fixed") {
        x <- stringr::str_replace_all(x, stringr::fixed(p), "")
      } else {
        x <- stringr::str_replace_all(x, p, "")
      }
    }
  }
  
  # Remove specified strings/patterns at the start
  if (!is.null(remove_start) && length(remove_start) > 0) {
    for (p in remove_start) {
      pat <- if (pattern_type == "fixed") paste0("^", escape_regex(p)) else paste0("^", p)
      x <- stringr::str_replace(x, pat, "")
    }
  }
  
  # Remove specified strings/patterns at the end
  if (!is.null(remove_end) && length(remove_end) > 0) {
    for (p in remove_end) {
      pat <- if (pattern_type == "fixed") paste0(escape_regex(p), "$") else paste0(p, "$")
      x <- stringr::str_replace(x, pat, "")
    }
  }
  
  if (to_lowercase) x <- tolower(x)
  
  if (remove_punctuation) {
    x <- stringr::str_replace_all(x, "[[:punct:]]+", punctuation_replacement)
  }
  
  if (strip_whitespace) {
    x <- stringr::str_squish(x)
  }
  
  x
}

# ------------------------------------------------------------------------------
# METHOD SPECS
# ------------------------------------------------------------------------------

#' Build method specifications
#'
#' @param methods Character vector of stringdist methods.
#' @param thresholds Named list or named numeric vector of thresholds.
#' @param method_params Named list of per-method parameter lists, passed to stringdist calls.
#'
#' @return Named list where each element contains method, threshold, and params.
build_method_specs <- function(methods,
                               thresholds = NULL,
                               method_params = NULL) {
  ensure_packages(c("stringdist"))
  
  valid_methods <- c("jw", "lv", "dl", "hamming", "lcs", "cosine")
  if (any(!methods %in% valid_methods)) {
    stop("Invalid method(s): ", paste(setdiff(methods, valid_methods), collapse = ", "),
         "\nValid methods: ", paste(valid_methods, collapse = ", "), call. = FALSE)
  }
  
  default_thresholds <- list(jw = 0.85, lv = 2, dl = 2, hamming = 2, lcs = 2, cosine = 0.2)
  
  if (is.null(thresholds)) thresholds <- default_thresholds
  if (is.numeric(thresholds)) thresholds <- as.list(thresholds)
  
  for (m in methods) {
    if (is.null(thresholds[[m]])) thresholds[[m]] <- default_thresholds[[m]]
  }
  
  if (is.null(method_params)) method_params <- list()
  
  specs <- lapply(methods, function(m) {
    list(
      method = m,
      threshold = thresholds[[m]],
      params = if (!is.null(method_params[[m]])) method_params[[m]] else list()
    )
  })
  names(specs) <- methods
  specs
}

#' Convert a threshold to an allowable distance (used for filtering matches)
#'
#' @param method Distance method.
#' @param threshold Threshold value (jw treated as similarity threshold).
#' @return Numeric allowable distance.
threshold_to_allowed_distance <- function(method, threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) {
    stop("threshold must be a single non-NA numeric value.", call. = FALSE)
  }
  if (method == "jw") {
    if (threshold < 0 || threshold > 1) {
      stop("For method 'jw', threshold is a similarity value in [0, 1].", call. = FALSE)
    }
    return(1 - threshold)
  }
  threshold
}

# ------------------------------------------------------------------------------
# INTERNAL MATCHING HELPERS
# ------------------------------------------------------------------------------

#' Split indices into batches
#'
#' @param n Integer size.
#' @param batch_size Integer batch size, or NULL for a single batch.
#' @return List of integer index vectors.
split_indices <- function(n, batch_size = NULL) {
  if (is.null(batch_size) || batch_size <= 0 || batch_size >= n) {
    return(list(seq_len(n)))
  }
  starts <- seq(1, n, by = batch_size)
  lapply(starts, function(s) s:min(s + batch_size - 1, n))
}

#' Compute matches for one method using distance filtering then similarity scoring
#'
#' @param queries Character vector (processed).
#' @param candidates Character vector (processed).
#' @param spec A single method spec from build_method_specs().
#' @param query_batch_size Optional integer for batching queries.
#' @param include_distance Logical; include distance in output.
#'
#' @return Data frame with query_index, candidate_index, method, similarity_percentage, distance.
match_one_method <- function(queries,
                             candidates,
                             spec,
                             query_batch_size = NULL,
                             include_distance = TRUE) {
  ensure_packages(c("stringdist"))
  
  m <- spec$method
  thr <- spec$threshold
  params <- spec$params
  
  allowed <- threshold_to_allowed_distance(m, thr)
  
  q_batches <- split_indices(length(queries), query_batch_size)
  out <- vector("list", length(q_batches))
  out_i <- 0L
  
  for (q_idx in q_batches) {
    q_chunk <- queries[q_idx]
    
    dmat <- do.call(
      stringdist::stringdistmatrix,
      c(list(a = q_chunk, b = candidates, method = m), params)
    )
    
    hits <- which(!is.na(dmat) & dmat <= allowed, arr.ind = TRUE)
    if (nrow(hits) == 0) next
    
    out_i <- out_i + 1L
    qi <- q_idx[hits[, 1]]
    ci <- hits[, 2]
    
    sim <- stringdist::stringsim(queries[qi], candidates[ci], method = m) * 100
    
    df <- data.frame(
      query_index = qi,
      candidate_index = ci,
      method = m,
      similarity_percentage = as.numeric(sim),
      stringsAsFactors = FALSE
    )
    
    if (include_distance) {
      df$distance <- as.numeric(dmat[cbind(hits[, 1], hits[, 2])])
    }
    
    df$threshold <- thr
    df$allowed_distance <- allowed
    
    out[[out_i]] <- df
  }
  
  if (out_i == 0L) return(data.frame())
  do.call(rbind, out[seq_len(out_i)])
}

# ------------------------------------------------------------------------------
# PUBLIC FUZZY MATCHING FUNCTIONS
# ------------------------------------------------------------------------------

#' Filter candidates for a single query using one method
#'
#' @param query Single character string.
#' @param candidates Character vector.
#' @param method One of supported methods.
#' @param threshold Threshold value.
#' @param method_params Named list of additional parameters passed to stringdist.
#'
#' @return Character vector of candidate strings that meet the matching criteria.
fuzzy_match_internal <- function(query, candidates, method, threshold, method_params = list()) {
  ensure_packages(c("stringdist"))
  
  if (is.factor(query)) query <- as.character(query)
  if (length(query) != 1L || !is.character(query)) stop("query must be a single character string.", call. = FALSE)
  
  if (is.factor(candidates)) candidates <- as.character(candidates)
  if (!is.character(candidates)) stop("candidates must be a character vector.", call. = FALSE)
  
  allowed <- threshold_to_allowed_distance(method, threshold)
  
  d <- do.call(
    stringdist::stringdist,
    c(list(a = query, b = candidates, method = method), method_params)
  )
  
  idx <- which(!is.na(d) & d <= allowed)
  candidates[idx]
}

#' Fuzzy Match with Jaro-Winkler
#' @export
fuzzy_match_jw <- function(query, candidates, threshold = 0.85, method_params = list()) {
  fuzzy_match_internal(query, candidates, method = "jw", threshold = threshold, method_params = method_params)
}

#' Fuzzy Match with Levenshtein
#' @export
fuzzy_match_lv <- function(query, candidates, threshold = 2, method_params = list()) {
  fuzzy_match_internal(query, candidates, method = "lv", threshold = threshold, method_params = method_params)
}

#' Fuzzy Match with Damerau-Levenshtein
#' @export
fuzzy_match_dl <- function(query, candidates, threshold = 2, method_params = list()) {
  fuzzy_match_internal(query, candidates, method = "dl", threshold = threshold, method_params = method_params)
}

#' Fuzzy Match with Hamming (equal-length strings)
#' @export
fuzzy_match_ham <- function(query, candidates, threshold = 2, method_params = list()) {
  fuzzy_match_internal(query, candidates, method = "hamming", threshold = threshold, method_params = method_params)
}

#' Fuzzy Match with Longest Common Subsequence
#' @export
fuzzy_match_lcs <- function(query, candidates, threshold = 2, method_params = list()) {
  fuzzy_match_internal(query, candidates, method = "lcs", threshold = threshold, method_params = method_params)
}

#' Fuzzy Match with Cosine Distance
#' @export
fuzzy_match_cosine <- function(query, candidates, threshold = 0.2, method_params = list()) {
  fuzzy_match_internal(query, candidates, method = "cosine", threshold = threshold, method_params = method_params)
}

# ------------------------------------------------------------------------------
# DATA FRAME MATCHING WRAPPER
# ------------------------------------------------------------------------------

#' Fuzzy matching across data frames
#'
#' @param query_df Data frame containing query values.
#' @param candidates_df Data frame containing candidate values.
#' @param query_col Column name in query_df containing queries.
#' @param candidates_col Column name in candidates_df containing candidates.
#' @param query_id_col Optional column name for query identifiers.
#' @param candidate_id_col Optional column name for candidate identifiers.
#' @param methods Character vector of methods.
#' @param thresholds Named list/numeric vector of thresholds per method.
#' @param method_params Named list of per-method parameter lists for stringdist.
#' @param preprocess Logical; preprocess query and candidate text.
#' @param preprocess_params Named list passed to text_preprocessing().
#' @param keep_original Logical; include original (unprocessed) text columns.
#' @param top_n Optional integer; keep only top N matches per query & method.
#' @param include_distance Logical; include distance column.
#' @param query_batch_size Optional integer; batching for query-side distance calculations.
#' @param timestamp_fn Function returning a timestamp; default Sys.time().
#'
#' @return Data frame with match results.
#' @export
fuzzy_match_df <- function(query_df,
                           candidates_df,
                           query_col,
                           candidates_col,
                           query_id_col = NULL,
                           candidate_id_col = NULL,
                           methods = c("jw", "lv", "dl", "hamming", "lcs", "cosine"),
                           thresholds = NULL,
                           method_params = NULL,
                           preprocess = TRUE,
                           preprocess_params = list(),
                           keep_original = TRUE,
                           top_n = NULL,
                           include_distance = TRUE,
                           query_batch_size = NULL,
                           timestamp_fn = Sys.time) {
  ensure_packages(c("stringdist", "stringr"))
  
  if (!is.data.frame(query_df)) stop("query_df must be a data frame.", call. = FALSE)
  if (!is.data.frame(candidates_df)) stop("candidates_df must be a data frame.", call. = FALSE)
  if (!(query_col %in% names(query_df))) stop("query_col not found in query_df.", call. = FALSE)
  if (!(candidates_col %in% names(candidates_df))) stop("candidates_col not found in candidates_df.", call. = FALSE)
  
  if (!is.null(query_id_col) && !(query_id_col %in% names(query_df))) {
    stop("query_id_col not found in query_df.", call. = FALSE)
  }
  if (!is.null(candidate_id_col) && !(candidate_id_col %in% names(candidates_df))) {
    stop("candidate_id_col not found in candidates_df.", call. = FALSE)
  }
  
  q_raw <- query_df[[query_col]]
  c_raw <- candidates_df[[candidates_col]]
  if (is.factor(q_raw)) q_raw <- as.character(q_raw)
  if (is.factor(c_raw)) c_raw <- as.character(c_raw)
  
  if (!is.character(q_raw)) q_raw <- as.character(q_raw)
  if (!is.character(c_raw)) c_raw <- as.character(c_raw)
  
  q_proc <- q_raw
  c_proc <- c_raw
  
  if (preprocess) {
    q_proc <- do.call(text_preprocessing, c(list(text_column = q_proc), preprocess_params))
    c_proc <- do.call(text_preprocessing, c(list(text_column = c_proc), preprocess_params))
  }
  
  specs <- build_method_specs(methods = methods, thresholds = thresholds, method_params = method_params)
  
  all_results <- vector("list", length(methods))
  for (k in seq_along(methods)) {
    m <- methods[k]
    all_results[[k]] <- match_one_method(
      queries = q_proc,
      candidates = c_proc,
      spec = specs[[m]],
      query_batch_size = query_batch_size,
      include_distance = include_distance
    )
  }
  
  results <- do.call(rbind, all_results)
  if (is.null(results) || nrow(results) == 0) {
    return(data.frame())
  }
  
  results$query <- q_proc[results$query_index]
  results$candidate <- c_proc[results$candidate_index]
  
  if (keep_original) {
    results$query_original <- q_raw[results$query_index]
    results$candidate_original <- c_raw[results$candidate_index]
  }
  
  if (!is.null(query_id_col)) {
    results$query_id <- query_df[[query_id_col]][results$query_index]
  }
  if (!is.null(candidate_id_col)) {
    results$candidate_id <- candidates_df[[candidate_id_col]][results$candidate_index]
  }
  
  results$timestamp <- timestamp_fn()
  
  key <- paste(results$query_index, results$method, sep = "\u001F")
  results$match_rank <- ave(
    results$similarity_percentage,
    key,
    FUN = function(x) rank(-x, ties.method = "min")
  )
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n <= 0) {
      stop("top_n must be a single positive number.", call. = FALSE)
    }
    results <- results[results$match_rank <= top_n, , drop = FALSE]
  }
  
  ord_cols <- c(
    if (!is.null(query_id_col)) "query_id" else NULL,
    "query",
    if (keep_original) "query_original" else NULL,
    if (!is.null(candidate_id_col)) "candidate_id" else NULL,
    "candidate",
    if (keep_original) "candidate_original" else NULL,
    "method",
    "similarity_percentage",
    if (include_distance) "distance" else NULL,
    "threshold",
    "allowed_distance",
    "match_rank",
    "timestamp"
  )
  ord_cols <- ord_cols[ord_cols %in% names(results)]
  
  results <- results[, ord_cols, drop = FALSE]
  results <- results[order(results$query, results$method, results$match_rank, -results$similarity_percentage), , drop = FALSE]
  rownames(results) <- NULL
  
  results
}

# Compatibility wrapper matching the original signature
# @export
fuzzy_match_wrapper <- function(query_df, candidates_df,
                                query_col, candidates_col,
                                methods = c("jw", "lv", "dl", "hamming", "lcs", "cosine"),
                                thresholds = NULL,
                                preprocess = TRUE,
                                top_n = NULL,
                                preprocess_params = list()) {
  fuzzy_match_df(
    query_df = query_df,
    candidates_df = candidates_df,
    query_col = query_col,
    candidates_col = candidates_col,
    methods = methods,
    thresholds = thresholds,
    preprocess = preprocess,
    preprocess_params = preprocess_params,
    top_n = top_n,
    keep_original = TRUE,
    include_distance = TRUE,
    query_batch_size = NULL
  )
}

# ------------------------------------------------------------------------------
# CALCULATE STATISTICS OF FUZZY MATCHING
# ------------------------------------------------------------------------------

#' Calculate statistics for fuzzy matching output
#'
#' Expects columns: query, candidate, similarity_percentage.
#'
#' @param fuzzy_output Data frame from fuzzy_match_df() / fuzzy_match_wrapper().
#' @param group_cols Character vector of column names used for grouping.
#' @param key_sep Separator used to build grouping keys (non-printing by default).
#'
#' @return Data frame with agreement, avg_similarity, median_similarity.
#' @export
calculate_stats <- function(fuzzy_output,
                            group_cols = c("query", "candidate"),
                            key_sep = "\u001F") {
  if (is.null(fuzzy_output) || nrow(fuzzy_output) == 0) {
    warning("Input data frame is empty or NULL.")
    return(NULL)
  }
  
  required_cols <- c(group_cols, "similarity_percentage")
  missing <- setdiff(required_cols, names(fuzzy_output))
  if (length(missing) > 0) {
    stop("Input data frame is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  gdf <- fuzzy_output[group_cols]
  gdf[] <- lapply(gdf, function(x) {
    if (is.factor(x)) x <- as.character(x)
    as.character(x)
  })
  
  key <- do.call(paste, c(gdf, sep = key_sep))
  sim <- fuzzy_output$similarity_percentage
  
  sim_split <- split(sim, key)
  
  agreement <- lengths(sim_split)
  avg_sim <- vapply(sim_split, function(x) mean(x, na.rm = TRUE), numeric(1))
  med_sim <- vapply(sim_split, function(x) median(x, na.rm = TRUE), numeric(1))
  
  groups <- names(sim_split)
  if (length(groups) == 0) {
    return(data.frame())
  }
  
  parts <- strsplit(groups, key_sep, fixed = TRUE)
  group_mat <- do.call(rbind, parts)
  if (is.null(dim(group_mat))) {
    group_mat <- matrix(group_mat, nrow = 1)
  }
  
  group_out <- as.data.frame(group_mat, stringsAsFactors = FALSE)
  names(group_out) <- group_cols
  
  out <- data.frame(
    group_out,
    agreement = as.integer(agreement),
    avg_similarity = as.numeric(avg_sim),
    median_similarity = as.numeric(med_sim),
    stringsAsFactors = FALSE
  )
  
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------------------------
# REPEATABLE WORKFLOW WRAPPER
# ------------------------------------------------------------------------------

#' End-to-end fuzzy matching workflow
#'
#' @param query_df Data frame with queries.
#' @param candidates_df Data frame with candidates.
#' @param query_col Query column name.
#' @param candidates_col Candidate column name.
#' @param ... Additional parameters forwarded to fuzzy_match_df().
#'
#' @return List with matches, stats, and inputs metadata.
#' @export
run_fuzzy_workflow <- function(query_df,
                               candidates_df,
                               query_col,
                               candidates_col,
                               ...) {
  matches <- fuzzy_match_df(
    query_df = query_df,
    candidates_df = candidates_df,
    query_col = query_col,
    candidates_col = candidates_col,
    ...
  )
  
  stats <- calculate_stats(matches)
  
  list(
    matches = matches,
    stats = stats,
    meta = list(
      query_n = nrow(query_df),
      candidate_n = nrow(candidates_df),
      query_col = query_col,
      candidates_col = candidates_col
    )
  )
}

# ------------------------------------------------------------------------------
# USER ACCEPTANCE TESTING (UAT)
# ------------------------------------------------------------------------------

run_uat <- function() {
  cat("\n====================\n")
  cat("UAT: text_preprocessing\n")
  cat("====================\n")
  test_text <- c("  Hello, World!  ", "Goodbye, World!")
  cat("Original text:\n")
  print(test_text)
  preprocessed <- text_preprocessing(
    test_text,
    remove_strings = "World",
    to_lowercase = TRUE,
    remove_punctuation = TRUE,
    strip_whitespace = TRUE,
    pattern_type = "fixed"
  )
  cat("Preprocessed text:\n")
  print(preprocessed)
  
  cat("\n====================\n")
  cat("UAT: Single-query match helpers\n")
  cat("====================\n")
  query <- "apple"
  candidates <- c("apple", "apples", "banana", "pineapple", "aple")
  
  cat("\nJaro-Winkler (threshold = 0.85):\n")
  print(fuzzy_match_jw(query, candidates, threshold = 0.85))
  
  cat("\nLevenshtein (threshold = 2):\n")
  print(fuzzy_match_lv(query, candidates, threshold = 2))
  
  cat("\nDamerau-Levenshtein (threshold = 2):\n")
  print(fuzzy_match_dl(query, candidates, threshold = 2))
  
  cat("\nHamming (threshold = 1) [equal-length candidates]:\n")
  ham_candidates <- c("apple", "appla", "aplex", "app")
  print(fuzzy_match_ham(query, ham_candidates, threshold = 1))
  
  cat("\nLCS (threshold = 2):\n")
  print(fuzzy_match_lcs(query, candidates, threshold = 2))
  
  cat("\nCosine (threshold = 0.2):\n")
  print(fuzzy_match_cosine(query, candidates, threshold = 0.2))
  
  cat("\n====================\n")
  cat("UAT: Data frame wrapper + stats\n")
  cat("====================\n")
  query_df <- data.frame(
    id = 1:3,
    query = c("apple pie", "banana bread", "cherry cobbler"),
    stringsAsFactors = FALSE
  )
  candidates_df <- data.frame(
    id = 1:4,
    candidate = c("apple", "banana", "cherry", "blueberry"),
    stringsAsFactors = FALSE
  )
  
  workflow <- run_fuzzy_workflow(
    query_df = query_df,
    candidates_df = candidates_df,
    query_col = "query",
    candidates_col = "candidate",
    query_id_col = "id",
    candidate_id_col = "id",
    methods = c("jw", "lv", "dl", "hamming", "lcs", "cosine"),
    thresholds = list(jw = 0.85, lv = 2, dl = 2, hamming = 2, lcs = 2, cosine = 0.2),
    preprocess = TRUE,
    preprocess_params = list(to_lowercase = TRUE, remove_punctuation = TRUE, strip_whitespace = TRUE),
    top_n = 2,
    query_batch_size = NULL
  )
  
  cat("Matches:\n")
  print(workflow$matches)
  cat("\nStats:\n")
  print(workflow$stats)
  
  invisible(workflow)
}

# Uncomment to run
# run_uat()
