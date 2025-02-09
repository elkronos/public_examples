# Load required packages for all functions
library(dplyr)
library(stringr)
library(stringdist)
library(magrittr)
library(tidyr)

# ------------------------------------------------------------------------------
# TEXT PREPROCESSING
# ------------------------------------------------------------------------------

#' Text Preprocessing
#'
#' This function performs preprocessing on a given text column. It can remove specified
#' strings, remove strings at the beginning or end of text, convert the text to lowercase,
#' remove punctuation, and strip extra whitespace.
#'
#' @param text_column A character vector of text data to be preprocessed.
#' @param remove_strings A character vector of strings to be removed from the text.
#' @param remove_start A character vector of strings to be removed from the beginning of the text.
#' @param remove_end A character vector of strings to be removed from the end of the text.
#' @param to_lowercase A logical value indicating whether to convert the text to lowercase (default: TRUE).
#' @param remove_punctuation A logical value indicating whether to remove punctuation from the text (default: TRUE).
#' @param strip_whitespace A logical value indicating whether to strip extra whitespace from the text (default: TRUE).
#'
#' @return A character vector of preprocessed text data.
#'
#' @examples
#' text <- c("  Hello, World!  ", "Goodbye, World!")
#' text_preprocessing(text, remove_strings = "World")
#'
#' @export
text_preprocessing <- function(text_column,
                               remove_strings = NULL,
                               remove_start = NULL,
                               remove_end = NULL,
                               to_lowercase = TRUE,
                               remove_punctuation = TRUE,
                               strip_whitespace = TRUE) {
  if (!is.character(text_column)) {
    stop("text_column must be a character vector.")
  }
  preprocessed_text <- text_column
  
  # Remove specified strings anywhere in the text
  if (!is.null(remove_strings)) {
    for (rm_str in remove_strings) {
      preprocessed_text <- gsub(rm_str, "", preprocessed_text, fixed = TRUE)
    }
  }
  
  # Remove specified strings from the start of the text
  if (!is.null(remove_start)) {
    for (start_str in remove_start) {
      preprocessed_text <- sub(paste0("^", start_str), "", preprocessed_text)
    }
  }
  
  # Remove specified strings from the end of the text
  if (!is.null(remove_end)) {
    for (end_str in remove_end) {
      preprocessed_text <- sub(paste0(end_str, "$"), "", preprocessed_text)
    }
  }
  
  # Convert to lowercase if requested
  if (to_lowercase) {
    preprocessed_text <- tolower(preprocessed_text)
  }
  
  # Remove punctuation if requested
  if (remove_punctuation) {
    preprocessed_text <- gsub("[[:punct:]]", "", preprocessed_text)
  }
  
  # Strip extra whitespace if requested
  if (strip_whitespace) {
    preprocessed_text <- gsub("\\s+", " ", preprocessed_text)  # Replace multiple spaces with one
    preprocessed_text <- trimws(preprocessed_text)             # Remove leading and trailing whitespace
  }
  
  return(preprocessed_text)
}

# ------------------------------------------------------------------------------
# INTERNAL HELPER FUNCTION FOR FUZZY MATCHING
# ------------------------------------------------------------------------------

#' Internal Fuzzy Matching Function
#'
#' This internal helper function computes fuzzy matches for a single query against a
#' vector of candidate strings using the specified method and threshold.
#'
#' For the "jw" (Jaro-Winkler) method, the provided threshold is interpreted as a similarity
#' threshold (e.g., 0.85) and converted to an allowable distance of 1 - threshold.
#' For all other methods, the threshold is taken as an absolute distance.
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param method A character string specifying the distance metric to use.
#' @param threshold A numeric value specifying the matching threshold.
#'
#' @return A character vector of candidate strings that meet the matching criteria.
fuzzy_match_internal <- function(query, candidates, method, threshold) {
  # Compute distance matrix: query (length 1) x candidates
  distances <- stringdist::stringdistmatrix(query, candidates, method = method)
  
  # For JW, interpret threshold as similarity and convert it to an allowable distance.
  if (method == "jw") {
    allowed <- 1 - threshold
  } else {
    allowed <- threshold
  }
  
  # For methods like "hamming", if lengths differ, stringdist returns NA.
  matching_indexes <- which(!is.na(distances) & distances <= allowed, arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

# ------------------------------------------------------------------------------
# FUZZY MATCHING FUNCTIONS (PER METRIC)
# ------------------------------------------------------------------------------

#' Fuzzy Match with Jaro-Winkler Distance
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the similarity threshold (default: 0.85).
#'
#' @return A character vector of candidate strings matching the query.
#'
#' @export
fuzzy_match_jw <- function(query, candidates, threshold = 0.85) {
  fuzzy_match_internal(query, candidates, method = "jw", threshold = threshold)
}

#' Fuzzy Match with Levenshtein Distance
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum edit distance allowed (default: 2).
#'
#' @return A character vector of candidate strings matching the query.
#'
#' @export
fuzzy_match_lv <- function(query, candidates, threshold = 2) {
  fuzzy_match_internal(query, candidates, method = "lv", threshold = threshold)
}

#' Fuzzy Match with Damerau-Levenshtein Distance
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum allowed distance (default: 2).
#'
#' @return A character vector of candidate strings matching the query.
#'
#' @export
fuzzy_match_dl <- function(query, candidates, threshold = 2) {
  fuzzy_match_internal(query, candidates, method = "dl", threshold = threshold)
}

#' Fuzzy Match with Hamming Distance
#'
#' Note: Hamming distance is only defined for strings of equal length.
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum allowed Hamming distance (default: 2).
#'
#' @return A character vector of candidate strings matching the query.
#'
#' @export
fuzzy_match_ham <- function(query, candidates, threshold = 2) {
  fuzzy_match_internal(query, candidates, method = "hamming", threshold = threshold)
}

#' Fuzzy Match with Longest Common Subsequence Distance
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum allowed LCS distance (default: 2).
#'
#' @return A character vector of candidate strings matching the query.
#'
#' @export
fuzzy_match_lcs <- function(query, candidates, threshold = 2) {
  fuzzy_match_internal(query, candidates, method = "lcs", threshold = threshold)
}

#' Fuzzy Match with Cosine Distance
#'
#' @param query A single character string representing the query.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum allowed cosine distance (default: 0.2).
#'
#' @return A character vector of candidate strings matching the query.
#'
#' @export
fuzzy_match_cosine <- function(query, candidates, threshold = 0.2) {
  fuzzy_match_internal(query, candidates, method = "cosine", threshold = threshold)
}

# ------------------------------------------------------------------------------
# FUZZY MATCHING WRAPPER FUNCTION
# ------------------------------------------------------------------------------

#' Fuzzy Matching Wrapper Function
#'
#' This function performs fuzzy matching between query and candidate data frames using multiple
#' distance metrics. It preprocesses the text (if requested), applies the specified matching methods
#' (with individual thresholds), and returns a data frame of matching results including a similarity
#' percentage and a match rank (per query & method).
#'
#' @param query_df A data frame containing query strings.
#' @param candidates_df A data frame containing candidate strings.
#' @param query_col A character string indicating the column in query_df with query strings.
#' @param candidates_col A character string indicating the column in candidates_df with candidate strings.
#' @param methods A character vector of distance metrics to use (default: c("jw", "lv", "dl", "hamming", "lcs", "cosine")).
#' @param thresholds A named list of thresholds for each method. For example: list(jw = 0.85, lv = 2, dl = 2, hamming = 2, lcs = 2, cosine = 0.2).
#'                  If not provided, default thresholds are used.
#' @param preprocess A logical value indicating whether to preprocess text before matching (default: TRUE).
#' @param top_n An integer value indicating the maximum number of top matching results to return per query per method (default: NULL, meaning no filtering).
#' @param preprocess_params A named list of additional parameters to pass to text_preprocessing (default: empty list).
#'
#' @return A data frame with columns: query, candidate, method, similarity_percentage, date, and match_rank.
#'
#' @examples
#' query_df <- data.frame(id = 1:3, query = c("apple pie", "banana bread", "cherry cobbler"), stringsAsFactors = FALSE)
#' candidates_df <- data.frame(id = 1:4, candidate = c("apple", "banana", "cherry", "blueberry"), stringsAsFactors = FALSE)
#' fm <- fuzzy_match_wrapper(query_df, candidates_df, query_col = "query", candidates_col = "candidate",
#'                           thresholds = list(jw = 0.85, lv = 2, dl = 2, hamming = 2, lcs = 2, cosine = 0.2),
#'                           preprocess = TRUE, top_n = 2)
#' print(fm)
#'
#' @export
fuzzy_match_wrapper <- function(query_df, candidates_df,
                                query_col, candidates_col,
                                methods = c("jw", "lv", "dl", "hamming", "lcs", "cosine"),
                                thresholds = NULL,
                                preprocess = TRUE,
                                top_n = NULL,
                                preprocess_params = list()) {
  # Validate inputs
  if (!is.data.frame(query_df)) stop("query_df must be a data frame.")
  if (!is.data.frame(candidates_df)) stop("candidates_df must be a data frame.")
  if (!(query_col %in% names(query_df))) stop("query_col not found in query_df.")
  if (!(candidates_col %in% names(candidates_df))) stop("candidates_col not found in candidates_df.")
  
  valid_methods <- c("jw", "lv", "dl", "hamming", "lcs", "cosine")
  if (any(!methods %in% valid_methods)) {
    stop("Invalid method specified. Valid methods are: ", paste(valid_methods, collapse = ", "))
  }
  
  # Set default thresholds if not provided
  default_thresholds <- list(jw = 0.85, lv = 2, dl = 2, hamming = 2, lcs = 2, cosine = 0.2)
  if (is.null(thresholds)) {
    thresholds <- default_thresholds
  } else {
    # For any method not explicitly provided, use the default.
    for (m in valid_methods) {
      if (m %in% methods && is.null(thresholds[[m]])) {
        thresholds[[m]] <- default_thresholds[[m]]
      }
    }
  }
  
  # Extract and (optionally) preprocess text columns
  query_text <- query_df[[query_col]]
  candidates_text <- candidates_df[[candidates_col]]
  if (preprocess) {
    query_text <- do.call(text_preprocessing, c(list(text_column = query_text), preprocess_params))
    candidates_text <- do.call(text_preprocessing, c(list(text_column = candidates_text), preprocess_params))
  }
  
  # Initialize a list to store results
  results_list <- list()
  
  # Loop over each method and each query value
  for (m in methods) {
    method_func <- switch(m,
                          jw = fuzzy_match_jw,
                          lv = fuzzy_match_lv,
                          dl = fuzzy_match_dl,
                          hamming = fuzzy_match_ham,
                          lcs = fuzzy_match_lcs,
                          cosine = fuzzy_match_cosine)
    for (i in seq_along(query_text)) {
      q <- query_text[i]
      # Get matching candidates using the selected method and threshold
      matches <- method_func(q, candidates_text, threshold = thresholds[[m]])
      if (length(matches) > 0) {
        # Calculate similarity percentage using stringsim
        percentages <- stringdist::stringsim(q, matches, method = m) * 100
        temp_df <- data.frame(
          query = q,
          candidate = matches,
          method = m,
          similarity_percentage = percentages,
          date = Sys.Date(),
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- temp_df
      }
    }
  }
  
  # Combine all results
  if (length(results_list) == 0) {
    results <- data.frame()
  } else {
    results <- do.call(rbind, results_list)
  }
  
  # Compute match rank (per query and method) based on similarity_percentage (highest first)
  if (nrow(results) > 0) {
    results <- results %>%
      group_by(query, method) %>%
      mutate(match_rank = rank(-similarity_percentage, ties.method = "min")) %>%
      ungroup()
    
    # If top_n is specified, filter to keep only the top_n matches per query per method
    if (!is.null(top_n)) {
      results <- results %>%
        group_by(query, method) %>%
        filter(match_rank <= top_n) %>%
        ungroup()
    }
  }
  
  return(results)
}

# ------------------------------------------------------------------------------
# CALCULATE STATISTICS OF FUZZY MATCHING
# ------------------------------------------------------------------------------

#' Calculate Statistics of Fuzzy Matching
#'
#' This function calculates statistics based on fuzzy matching results. It expects a data frame
#' (typically produced by fuzzy_match_wrapper) that contains the columns "query", "candidate", and
#' "similarity_percentage". For each unique query-candidate pair, it computes the number of matches,
#' the average similarity, and the median similarity.
#'
#' @param fuzzy_output A data frame containing fuzzy matching results.
#'
#' @return A data frame with statistics: query, candidate, agreement (number of matches), average similarity,
#'         and median similarity.
#'
#' @examples
#' # Assuming fm is the output from fuzzy_match_wrapper:
#' stats <- calculate_stats(fm)
#' print(stats)
#'
#' @export
calculate_stats <- function(fuzzy_output) {
  if (is.null(fuzzy_output) || nrow(fuzzy_output) == 0) {
    warning("Input data frame is empty or NULL.")
    return(NULL)
  }
  
  required_cols <- c("query", "candidate", "similarity_percentage")
  if (!all(required_cols %in% colnames(fuzzy_output))) {
    stop("Input data frame must contain 'query', 'candidate', and 'similarity_percentage' columns.")
  }
  
  stats <- fuzzy_output %>%
    group_by(query, candidate) %>%
    summarize(agreement = n(),
              avg_similarity = mean(similarity_percentage, na.rm = TRUE),
              median_similarity = median(similarity_percentage, na.rm = TRUE),
              .groups = "drop")
  return(stats)
}

# ------------------------------------------------------------------------------
# USER ACCEPTANCE TESTING (UAT)
# ------------------------------------------------------------------------------

if (interactive() || Sys.getenv("UAT_RUN") == "TRUE") {
  
  cat("\n====================\n")
  cat("UAT: text_preprocessing\n")
  cat("====================\n")
  test_text <- c("  Hello, World!  ", "Goodbye, World!")
  cat("Original text:\n")
  print(test_text)
  preprocessed <- text_preprocessing(test_text,
                                     remove_strings = "World",
                                     remove_start = "  ",
                                     remove_end = "  ",
                                     to_lowercase = TRUE,
                                     remove_punctuation = TRUE,
                                     strip_whitespace = TRUE)
  cat("Preprocessed text:\n")
  print(preprocessed)
  
  cat("\n====================\n")
  cat("UAT: Fuzzy Matching Functions\n")
  cat("====================\n")
  query <- "apple"
  candidates <- c("apple", "apples", "banana", "pineapple", "aple")
  
  cat("\nJaro-Winkler Matching (threshold = 0.85):\n")
  print(fuzzy_match_jw(query, candidates, threshold = 0.85))
  
  cat("\nLevenshtein Matching (threshold = 2):\n")
  print(fuzzy_match_lv(query, candidates, threshold = 2))
  
  cat("\nDamerau-Levenshtein Matching (threshold = 2):\n")
  print(fuzzy_match_dl(query, candidates, threshold = 2))
  
  # For Hamming, only use candidates of equal length as the query.
  cat("\nHamming Matching (threshold = 1) [only equal-length candidates]:\n")
  ham_candidates <- c("apple", "appla", "aplex", "app")
  print(fuzzy_match_ham(query, ham_candidates, threshold = 1))
  
  cat("\nLongest Common Subsequence Matching (threshold = 2):\n")
  print(fuzzy_match_lcs(query, candidates, threshold = 2))
  
  cat("\nCosine Matching (threshold = 0.2):\n")
  print(fuzzy_match_cosine(query, candidates, threshold = 0.2))
  
  cat("\n====================\n")
  cat("UAT: fuzzy_match_wrapper\n")
  cat("====================\n")
  query_df <- data.frame(id = 1:3,
                         query = c("apple pie", "banana bread", "cherry cobbler"),
                         stringsAsFactors = FALSE)
  candidates_df <- data.frame(id = 1:4,
                              candidate = c("apple", "banana", "cherry", "blueberry"),
                              stringsAsFactors = FALSE)
  
  # You can pass extra parameters to text_preprocessing via preprocess_params if desired.
  fm_results <- fuzzy_match_wrapper(query_df, candidates_df,
                                    query_col = "query",
                                    candidates_col = "candidate",
                                    methods = c("jw", "lv", "dl", "hamming", "lcs", "cosine"),
                                    thresholds = list(jw = 0.85, lv = 2, dl = 2, hamming = 2, lcs = 2, cosine = 0.2),
                                    preprocess = TRUE,
                                    top_n = 2,
                                    preprocess_params = list(to_lowercase = TRUE, remove_punctuation = TRUE, strip_whitespace = TRUE))
  cat("Fuzzy Match Wrapper Results:\n")
  print(fm_results)
  
  cat("\n====================\n")
  cat("UAT: calculate_stats\n")
  cat("====================\n")
  stats <- calculate_stats(fm_results)
  cat("Calculated Statistics:\n")
  print(stats)
}
