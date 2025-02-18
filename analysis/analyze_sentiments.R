# Load required libraries
library(tidyverse)
library(tidyr)
library(tidytext)
library(ggplot2)
library(syuzhet)
library(tm)

#' Analyze sentiments for text columns in a data frame
#'
#' This function takes a data frame, a vector of text column names, and a vector of sentiment analysis methods,
#' and returns a new data frame with sentiment scores and labels for each method as well as an overall majority
#' sentiment label for each text column per row.
#'
#' @param data A data frame containing the text columns to be analyzed.
#' @param text_columns A character vector specifying the names of the text columns in the data frame to analyze.
#' @param methods A character vector specifying the sentiment analysis methods to use.
#'                Default is c("syuzhet", "afinn", "bing"). Supported methods include "syuzhet", "afinn", "bing", and "nrc".
#' @param preprocess A logical flag indicating whether to preprocess the text (convert to lowercase, remove special characters,
#'                   and trim whitespace). Default is FALSE.
#' @param remove_stopwords A logical flag indicating whether to remove stop words from the text data. Default is FALSE.
#' @param thresholds A named list of thresholds for sentiment labeling for each method.
#'                   Each element should be a numeric vector with named elements "positive" and "negative".
#'                   Defaults to:
#'                   list(
#'                     syuzhet = c(positive = 0.5, negative = -0.5),
#'                     afinn   = c(positive = 5,   negative = -5),
#'                     bing    = c(positive = 1,   negative = -1),
#'                     nrc     = c(positive = 1,   negative = -1)
#'                   )
#'
#' @return A data frame with sentiment scores and labels for each method and an overall majority sentiment label
#'         for each text column per row.
#'
#' @examples
#' sample_data <- tibble(
#'   product = c("I love the product! It's amazing.", "I loved the product.", "I did not care about the product", "I really liked it. Easy to use!"),
#'   service = c("The customer service is terrible.", "It was a bad experience. I got angry.", "I hated the customer service! So frustrating!", "It was so annoying and I got pretty upset for a while"),
#'   loyalty = c("I don't know.", "I would buy my next product from you.", "Never.", "Probably"),
#'   recommend = c("Yes, I would recommend the product.", "Yes, I was really pleased.", "I hated everything! Grr!", "Sure, I would see what else you had")
#' )
#'
#' sample_data_with_scores <- analyze_sentiments(
#'   sample_data,
#'   text_columns = c("product", "service", "loyalty", "recommend"),
#'   methods = c("syuzhet", "afinn", "bing", "nrc"),
#'   preprocess = TRUE,
#'   remove_stopwords = TRUE,
#'   thresholds = list(
#'     syuzhet = c(positive = 0.5, negative = -0.5),
#'     afinn   = c(positive = 5,   negative = -5),
#'     bing    = c(positive = 1,   negative = -1),
#'     nrc     = c(positive = 1,   negative = -1)
#'   )
#' )
#'
#' head(sample_data_with_scores)
#' str(sample_data_with_scores)
analyze_sentiments <- function(data,
                               text_columns,
                               methods = c("syuzhet", "afinn", "bing"),
                               preprocess = FALSE,
                               remove_stopwords = FALSE,
                               thresholds = list(
                                 syuzhet = c(positive = 0.5, negative = -0.5),
                                 afinn   = c(positive = 5,   negative = -5),
                                 bing    = c(positive = 1,   negative = -1),
                                 nrc     = c(positive = 1,   negative = -1)
                               )) {
  
  # Check that data is a data.frame
  if(!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Load sentiment lexicons and stop words
  afinn <- get_sentiments("afinn")
  lexicon_bing <- get_sentiments("bing")
  nrc_lex <- get_sentiments("nrc") %>% filter(sentiment %in% c("positive", "negative"))
  stop_words <- get_stopwords()
  
  # Check that provided text_columns exist in data
  if (!all(text_columns %in% names(data))) {
    stop("One or more specified text_columns do not exist in the data.")
  }
  
  # Preprocess text columns if requested
  if (preprocess || remove_stopwords) {
    data <- data %>%
      mutate(across(all_of(text_columns), ~ {
        x <- .
        if (preprocess) {
          x <- str_to_lower(x)
          x <- str_replace_all(x, "[^[:alnum:][:space:]]", " ")
          x <- str_squish(x)
        }
        if (remove_stopwords) {
          x <- removeWords(x, stop_words$word)
        }
        x
      }))
  }
  
  # Add a row number column to track original rows
  data <- data %>% mutate(rn = row_number())
  
  # Helper function to compute sentiment score for a given column and method
  compute_sentiment <- function(df, col, method) {
    df_tidy <- df %>% unnest_tokens(word, all_of(col))
    
    if (method == "syuzhet") {
      df_tidy <- df_tidy %>% mutate(sentiment = as.numeric(get_sentiment(word)))
    } else if (method == "afinn") {
      df_tidy <- df_tidy %>% 
        left_join(afinn, by = "word") %>% 
        mutate(sentiment = as.numeric(replace_na(value, 0))) %>% 
        select(-value)
    } else if (method == "bing") {
      df_tidy <- df_tidy %>% 
        left_join(lexicon_bing, by = "word") %>% 
        mutate(sentiment = if_else(is.na(sentiment), "neutral", sentiment)) %>% 
        mutate(sentiment = as.numeric(if_else(sentiment == "positive", 1,
                                              if_else(sentiment == "negative", -1, 0))))
    } else if (method == "nrc") {
      df_tidy <- df_tidy %>% 
        left_join(nrc_lex, by = "word") %>% 
        mutate(sentiment = as.numeric(if_else(sentiment == "positive", 1,
                                              if_else(sentiment == "negative", -1, 0)))) %>% 
        replace_na(list(sentiment = 0))
    } else {
      stop("Unsupported sentiment method: ", method)
    }
    
    df_tidy %>% 
      group_by(rn) %>% 
      summarize(score = sum(sentiment, na.rm = TRUE), .groups = "drop")
  }
  
  # Iterate over each text column and sentiment method
  for (col in text_columns) {
    for (method in methods) {
      sentiment_df <- compute_sentiment(data, col, method)
      
      score_col_name <- paste0(col, "_", method, "_score")
      label_col_name <- paste0(col, "_", method, "_label")
      
      data <- data %>%
        left_join(sentiment_df, by = "rn") %>%
        rename(!!score_col_name := score) %>%
        mutate(!!label_col_name := case_when(
          .data[[score_col_name]] > thresholds[[method]]["positive"] ~ "positive",
          .data[[score_col_name]] < thresholds[[method]]["negative"] ~ "negative",
          TRUE ~ "neutral"
        ))
    }
    
    # Calculate an overall (majority) sentiment label for the text column
    score_cols <- paste0(col, "_", methods, "_score")
    maj_label_col <- paste0(col, "_majority_label")
    
    data <- data %>%
      mutate(!!maj_label_col := case_when(
        rowSums(across(all_of(score_cols), ~ . > 0)) > rowSums(across(all_of(score_cols), ~ . < 0)) ~ "positive",
        rowSums(across(all_of(score_cols), ~ . < 0)) > rowSums(across(all_of(score_cols), ~ . > 0)) ~ "negative",
        TRUE ~ "neutral"
      ))
  }
  
  # Remove the row number column before returning
  data <- data %>% select(-rn)
  
  return(data)
}

# Define sample data for testing
sample_data <- tibble(
  product = c("I love the product! It's amazing.", 
              "I loved the product.", 
              "I did not care about the product", 
              "I really liked it. Easy to use!"),
  service = c("The customer service is terrible.", 
              "It was a bad experience. I got angry.", 
              "I hated the customer service! So frustrating!", 
              "It was so annoying and I got pretty upset for a while"),
  loyalty = c("I don't know.", 
              "I would buy my next product from you.", 
              "Never.", 
              "Probably"),
  recommend = c("Yes, I would recommend the product.", 
                "Yes, I was really pleased.", 
                "I hated everything! Grr!", 
                "Sure, I would see what else you had")
)

# ========================
# Begin Testthat UAT Block
# ========================
# Set non-interactive mode for textdata to prevent interactive prompts
Sys.setenv(TEXTDATA_INTERACTIVE = "FALSE")

# Suppress package startup messages and load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyr)
  library(tidytext)
  library(ggplot2)
  library(syuzhet)
  library(tm)
  library(testthat)
  library(textdata)
  library(stopwords)
})

test_that("Valid data parameter", {
  result <- analyze_sentiments(
    data = sample_data,
    text_columns = c("product", "service"),
    methods = c("syuzhet", "afinn", "bing", "nrc")
  )
  expect_s3_class(result, "data.frame")
  expect_true("product_syuzhet_score" %in% names(result))
  expect_true("service_bing_label" %in% names(result))
})

test_that("Invalid data parameter (non-data frame)", {
  expect_error(analyze_sentiments(
    data = c("Not", "a", "data", "frame"),
    text_columns = c("product"),
    methods = c("syuzhet")
  ))
})

test_that("Valid text_columns parameter", {
  result <- analyze_sentiments(
    data = sample_data,
    text_columns = c("loyalty", "recommend"),
    methods = c("syuzhet", "afinn", "bing", "nrc")
  )
  expect_true("loyalty_afinn_label" %in% names(result))
  expect_true("recommend_nrc_score" %in% names(result))
})

test_that("Non-existent text_columns parameter", {
  expect_error(analyze_sentiments(
    data = sample_data,
    text_columns = c("nonexistent"),
    methods = c("syuzhet")
  ))
})

test_that("Valid methods parameter with additional method 'nrc'", {
  result <- analyze_sentiments(
    data = sample_data,
    text_columns = c("product"),
    methods = c("syuzhet", "afinn", "bing", "nrc")
  )
  expect_true("product_nrc_label" %in% names(result))
  expect_true("product_nrc_score" %in% names(result))
})

test_that("Unsupported method should throw error", {
  expect_error(analyze_sentiments(
    data = sample_data,
    text_columns = c("service"),
    methods = c("unsupported_method")
  ))
})

test_that("Preprocess parameter works (text cleaning)", {
  result <- analyze_sentiments(
    data = sample_data,
    text_columns = c("product"),
    methods = c("syuzhet"),
    preprocess = TRUE
  )
  expect_true("product_syuzhet_score" %in% names(result))
})

test_that("Remove stopwords parameter works", {
  result <- analyze_sentiments(
    data = sample_data,
    text_columns = c("recommend"),
    methods = c("afinn"),
    remove_stopwords = TRUE
  )
  expect_true("recommend_afinn_score" %in% names(result))
})

test_that("Custom thresholds parameter works", {
  custom_thresholds <- list(
    syuzhet = c(positive = 0.2, negative = -0.2),
    afinn   = c(positive = 2, negative = -2),
    bing    = c(positive = 0.5, negative = -0.5),
    nrc     = c(positive = 0.5, negative = -0.5)
  )
  result <- analyze_sentiments(
    data = sample_data,
    text_columns = c("service"),
    methods = c("syuzhet", "afinn", "bing", "nrc"),
    thresholds = custom_thresholds
  )
  expect_true("service_syuzhet_label" %in% names(result))
})
