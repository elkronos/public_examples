#' #################### Table of Contents and Descriptions
#' 
#' # Line 041 - text_preprocessing - Used to remove user specified strings, convert the text to lowercase, remove punctuation, and strip whitespace.
#' # Line 105 - fuzzy_match_jw - jw: Jaro-Winkler algorithm measures string similarity with a focus on character matching and transpositions.
#' # Line 131 - fuzzy_match_lv - lv: Levenshtein distance calculates the minimum number of single-character edits required to transform one string into another.
#' # Line 157 - fuzzy_match_dl - dl: Damerau-Levenshtein distance is an extension of Levenshtein distance, accounting for transpositions of adjacent characters.
#' # Line 183 - fuzzy_match_ham - hamming: Hamming distance measures the number of differing characters between two equal-length strings.
#' # Line 209 - fuzzy_match_lcs - lcs: Longest Common Subsequence determines the longest subsequence of characters shared by two strings.
#' # Line 235 - fuzzy_match_cosine - cosine: Cosine similarity calculates the angle between two n-dimensional vectors to determine the similarity of their respective string representations.
#' # Line 269 - fuzzy_match_wrapper - Wrapper for fuzzy string matching using multiple distance metrics.
#' # Line 337 - calculate_stats - Calculates the statistics of fuzzy matching between two data frames based on the matching percentage
#' 
# Load required packages for all functions
library(dplyr)
library(stringr)
library(stringdist)
library(magrittr)
library(tidyr)
#' Text Preprocessing
#'
#' This function performs preprocessing on a given text column. It can remove specified
#' strings, convert the text to lowercase, remove punctuation, and strip whitespace.
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
#' @importFrom stringr gsub sub trimws
#'
#' @examples
#' text <- c("Hello, World!", "Goodbye, World!")
#' text_preprocessing(text)
#'
#' @export
text_preprocessing <- function(text_column, remove_strings = NULL, remove_start = NULL, remove_end = NULL, 
                               to_lowercase = TRUE, remove_punctuation = TRUE, strip_whitespace = TRUE) {
  preprocessed_text <- text_column
  
  # Remove specified strings
  if (!is.null(remove_strings)) {
    for (remove_string in remove_strings) {
      preprocessed_text <- gsub(remove_string, "", preprocessed_text, fixed = TRUE)
    }
  }
  
  # Remove specified strings from the start
  if (!is.null(remove_start)) {
    for (start_string in remove_start) {
      preprocessed_text <- sub(paste0("^", start_string), "", preprocessed_text)
    }
  }
  
  # Remove specified strings from the end
  if (!is.null(remove_end)) {
    for (end_string in remove_end) {
      preprocessed_text <- sub(paste0(end_string, "$"), "", preprocessed_text)
    }
  }
  
  # Convert to lowercase
  if (to_lowercase) {
    preprocessed_text <- tolower(preprocessed_text)
  }
  
  # Remove punctuation
  if (remove_punctuation) {
    preprocessed_text <- gsub("[[:punct:]]", "", preprocessed_text)
  }
  
  # Strip extra whitespace
  if (strip_whitespace) {
    preprocessed_text <- gsub("\\s+", " ", preprocessed_text) # Replace multiple spaces with a single space
    preprocessed_text <- trimws(preprocessed_text) # Remove leading and trailing whitespace
  }
  
  return(preprocessed_text)
}

#' Fuzzy Match with Jaro-Winkler Distance
#'
#' This function performs fuzzy string matching using the Jaro-Winkler distance metric.
#' It calculates the distance between a query string and a set of candidate strings and returns
#' the candidates that have a distance less than or equal to the specified threshold.
#'
#' @param query A character vector representing the query string.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum distance allowed between the query and candidate strings (default: 0.85).
#'
#' @return A character vector of candidate strings that match the query string with a distance less than or equal to the threshold.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' query <- "apple"
#' candidates <- c("apples", "banana", "orange", "pineapple")
#' fuzzy_match_jw(query, candidates, threshold = 0.9)
#'
#' @export
fuzzy_match_jw <- function(query, candidates, threshold = 0.85) {
  distances <- stringdist::stringdistmatrix(query, candidates, method = "jw")
  matching_indexes <- which(distances <= (1 - threshold), arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

#' Fuzzy Match with Levenshtein Distance
#'
#' This function performs fuzzy string matching using the Levenshtein distance metric.
#' It calculates the distance between a query string and a set of candidate strings and returns
#' the candidates that have a distance less than or equal to the specified threshold.
#'
#' @param query A character vector representing the query string.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum distance allowed between the query and candidate strings (default: 2).
#'
#' @return A character vector of candidate strings that match the query string with a distance less than or equal to the threshold.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' query <- "apple"
#' candidates <- c("apples", "banana", "orange", "pineapple")
#' fuzzy_match_lv(query, candidates, threshold = 1)
#'
#' @export
fuzzy_match_lv <- function(query, candidates, threshold = 2) {
  distances <- stringdist::stringdistmatrix(query, candidates, method = "lv")
  matching_indexes <- which(distances <= threshold, arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

#' Fuzzy Match with Damerau-Levenshtein Distance
#'
#' This function performs fuzzy string matching using the Damerau-Levenshtein distance metric.
#' It calculates the distance between a query string and a set of candidate strings and returns
#' the candidates that have a distance less than or equal to the specified threshold.
#'
#' @param query A character vector representing the query string.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum distance allowed between the query and candidate strings (default: 2).
#'
#' @return A character vector of candidate strings that match the query string with a distance less than or equal to the threshold.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' query <- "apple"
#' candidates <- c("apples", "banana", "orange", "pineapple")
#' fuzzy_match_dl(query, candidates, threshold = 1)
#'
#' @export
fuzzy_match_dl <- function(query, candidates, threshold = 2) {
  distances <- stringdist::stringdistmatrix(query, candidates, method = "dl")
  matching_indexes <- which(distances <= threshold, arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

#' Fuzzy Match with Hamming Distance
#'
#' This function performs fuzzy string matching using the Hamming distance metric.
#' It calculates the distance between a query string and a set of candidate strings and returns
#' the candidates that have a distance less than or equal to the specified threshold.
#'
#' @param query A character vector representing the query string.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum distance allowed between the query and candidate strings (default: 2).
#'
#' @return A character vector of candidate strings that match the query string with a distance less than or equal to the threshold.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' query <- "apple"
#' candidates <- c("apples", "banana", "orange", "pineapple")
#' fuzzy_match_ham(query, candidates, threshold = 1)
#'
#' @export
fuzzy_match_ham <- function(query, candidates, threshold = 2) {
  distances <- stringdist::stringdistmatrix(query, candidates, method = "hamming")
  matching_indexes <- which(distances <= threshold, arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

#' Fuzzy Match with Longest Common Substring Distance
#'
#' This function performs fuzzy string matching using the Longest Common Substring (LCS) distance metric.
#' It calculates the distance between a query string and a set of candidate strings and returns
#' the candidates that have a distance less than or equal to the specified threshold.
#'
#' @param query A character vector representing the query string.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum distance allowed between the query and candidate strings (default: 2).
#'
#' @return A character vector of candidate strings that match the query string with a distance less than or equal to the threshold.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' query <- "apple"
#' candidates <- c("apples", "banana", "orange", "pineapple")
#' fuzzy_match_lcs(query, candidates, threshold = 1)
#'
#' @export
fuzzy_match_lcs <- function(query, candidates, threshold = 2) {
  distances <- stringdist::stringdistmatrix(query, candidates, method = "lcs")
  matching_indexes <- which(distances <= threshold, arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

#' Fuzzy Match with Cosine Distance
#'
#' This function performs fuzzy string matching using the Cosine distance metric.
#' It calculates the distance between a query string and a set of candidate strings and returns
#' the candidates that have a distance less than or equal to the specified threshold.
#'
#' @param query A character vector representing the query string.
#' @param candidates A character vector of candidate strings.
#' @param threshold A numeric value representing the maximum distance allowed between the query and candidate strings (default: 0.2).
#'
#' @return A character vector of candidate strings that match the query string with a distance less than or equal to the threshold.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' query <- "apple"
#' candidates <- c("apples", "banana", "orange", "pineapple")
#' fuzzy_match_cosine(query, candidates, threshold = 0.1)
#'
#' @export
fuzzy_match_cosine <- function(query, candidates, threshold = 0.2) {
  distances <- stringdist::stringdistmatrix(query, candidates, method = "cosine")
  matching_indexes <- which(distances <= threshold, arr.ind = TRUE)[, 2]
  return(candidates[matching_indexes])
}

#' Fuzzy Matching Wrapper Function
#'
#' This function provides a wrapper for performing fuzzy string matching using multiple distance metrics.
#' It takes two data frames, one containing query strings and the other containing candidate strings,
#' and returns a data frame with the matching results for each specified method.
#'
#' @param query_df A data frame containing the query strings.
#' @param candidates_df A data frame containing the candidate strings.
#' @param query_col A character string indicating the column in query_df that contains the query strings.
#' @param candidates_col A character string indicating the column in candidates_df that contains the candidate strings.
#' @param methods A character vector specifying the distance metrics to use for fuzzy matching (default: c("jw", "lv", "dl", "hamming", "lcs", "cosine")).
#' @param threshold_percentage A numeric value representing the maximum distance allowed between the query and candidate strings as a percentage of the query string length (default: NULL).
#' @param preprocess A logical value indicating whether to apply text preprocessing to the strings before fuzzy matching (default: TRUE).
#' @param top_n An integer value indicating the number of top matching results to return for each query string (default: NULL).
#'
#' @return A data frame containing the matching results for each specified method.
#'
#' @importFrom stringdist stringsim
#' @importFrom dplyr mutate filter arrange
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @examples
#' query_df <- data.frame(id = 1:3, query = c("apple pie", "banana bread", "cherry cobbler"))
#' candidates_df <- data.frame(id = 1:4, candidate = c("apple", "banana", "cherry", "blueberry"))
#' fuzzy_match_wrapper(query_df, candidates_df, "query", "candidate", threshold_percentage = 20, top_n = 2)
#'
#' @export
fuzzy_match_wrapper <- function(query_df, candidates_df, query_col, candidates_col, methods = c("jw", "lv", "dl", "hamming", "lcs", "cosine"), threshold_percentage, preprocess = TRUE, top_n = NULL) {
  # Ensure the input methods are valid
  valid_methods <- c("jw", "lv", "dl", "hamming", "lcs", "cosine")
  if (any(!methods %in% valid_methods)) {
    stop("Invalid method specified. Please choose from 'jw', 'lv', 'dl', 'hamming', 'lcs', or 'cosine'.")
  }
  
  results <- data.frame()
  
  for (method in methods) {
    method_func <- switch(method,
                          "jw" = fuzzy_match_jw,
                          "lv" = fuzzy_match_lv,
                          "dl" = fuzzy_match_dl,
                          "hamming" = fuzzy_match_ham,
                          "lcs" = fuzzy_match_lcs,
                          "cosine" = fuzzy_match_cosine)
    
    method_results <- data.frame()
    
    for (query_value in query_df[[query_col]]) {
      matches <- method_func(query_value, candidates_df[[candidates_col]], threshold_percentage / 100)
      
      if (length(matches) > 0) {
        match_rows <- data.frame(
          original_name = query_col,
          original_value = query_value,
          matched_name = candidates_col,
          matched_value = matches,
          method = method,
          percentage = stringdist::stringsim(query_value, matches, method = method) * 100,
          date = Sys.Date(),
          stringsAsFactors = FALSE
        )
        method_results <- rbind(method_results, match_rows)
      }
    }
    
    # Calculate match rank based on percentage
    method_results$match_rank <- ave(ifelse(!is.na(method_results$percentage), -method_results$percentage, method_results$percentage), method_results$original_value, method_results$method, FUN = rank)
    
    results <- rbind(results, method_results)
  }
  
  if (!is.null(top_n)) {
    results <- results[order(results$original_value, results$match_rank),]
    results <- results[ave(results$match_rank, results$original_value, FUN = seq_along) <= top_n,]
  }
  
  return(results)
}

#' Calculate Statistics of Fuzzy Matching
#'
#' The function calculates the statistics of fuzzy matching between two data frames based on the matching percentage.
#'
#' @param fuzzy_output A data frame containing the result of fuzzy matching. It should have "original_value", "matched_value", and "percentage" columns.
#' @return A data frame containing the statistics of fuzzy matching, including the number of agreements, average and median percentage of matching.
#' @import dplyr
#' @importFrom utils packageDescription
#'
#' @examples
#' query_df <- data.frame(id = 1:3, query = c("apple pie", "banana bread", "cherry cobbler"))
#' candidates_df <- data.frame(id = 1:4, candidate = c("apple", "banana", "cherry", "blueberry"))
#' fuzzy_output <- fuzzy_match_wrapper(query_df, candidates_df, "query", "candidate", threshold_percentage = 20, top_n = 2)
#' calculate_stats(fuzzy_output)
#'
#' @export
calculate_stats <- function(fuzzy_output) {
  if (is.null(fuzzy_output) || nrow(fuzzy_output) == 0) {
    warning("Input data frame is empty or NULL.")
    return(NULL)
  }
  
  if (!all(c("original_value", "matched_value", "percentage") %in% colnames(fuzzy_output))) {
    stop("Input data frame must contain 'original_value', 'matched_value', and 'percentage' columns.")
  }
  
  # Calculate agreement
  agreement <- fuzzy_output %>%
    dplyr::group_by(original_value, matched_value) %>%
    dplyr::summarize(agreement = n(), .groups = "drop")
  
  # Calculate average and median percentage
  avg_median_percentage <- fuzzy_output %>%
    dplyr::group_by(original_value, matched_value) %>%
    dplyr::summarize(avg_percentage = mean(percentage, na.rm = TRUE),
                     median_percentage = median(percentage, na.rm = TRUE), .groups = "drop")
  
  # Merge the agreement, average, and median percentage
  stats <- dplyr::inner_join(agreement, avg_median_percentage, by = c("original_value", "matched_value"))
  
  return(stats)
}