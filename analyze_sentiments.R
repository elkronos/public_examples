#' Analyze sentiments for text columns in a data frame
#'
#' This function takes a data frame, a vector of text column names, and a vector of sentiment analysis methods, and
#' returns a new data frame with sentiment scores and labels for each method and overall majority sentiment labels
#' for each text column per row.
#'
#' @param data A data frame containing the text columns to be analyzed.
#' @param text_columns A character vector specifying the names of the text columns in the data frame to analyze.
#' @param methods A character vector specifying the sentiment analysis methods to use. Default is c("syuzhet", "afinn", "bing").
#' @param preprocess A logical flag indicating whether to preprocess the text data by converting it to lowercase,
#'                   removing special characters, and trimming white spaces. Default is FALSE.
#' @param remove_stopwords A logical flag indicating whether to remove stop words from the text data. Default is FALSE.
#'
#' @return A data frame with sentiment scores and labels for each method and overall majority sentiment labels for
#'         each text column per row.
#'
#' @import tidyverse
#' @import tidyr
#' @import tidytext
#' @import ggplot2
#' @import syuzhet
#'
#' @examples
#'
#' # Sample data
#' sample_data <- tibble(
#'   product = c("I love the product! It's amazing.", "I loved the product.", "I did not care about the product", "I really liked it. Easy to use!"),
#'   service = c("The customer service is terrible.", "It was a bad experience. I got angry.", "I hated the customer service! So frustrating!", "It was so annyoing and I got pretty upset for a while"),
#'   loyalty = c("I don't know.", "I would buy my next product from you.", "Never.", "Probably"),
#'   recommend = c("Yes, I would recommend the product.", "Yes, I was really pleased.", "I hated everything! Grr!", "Sure, I would see what else you had"),
#' )
#'
#' # Analyze sentiments for the sample_data
#' sample_data_with_scores <- analyze_sentiments(
#'   sample_data,
#'   text_columns = c("product", "service", "loyalty", "recommend"),
#'   methods = c("syuzhet", "afinn", "bing"),
#'   preprocess = TRUE,
#'   remove_stopwords = TRUE
#' )
#'
#' # View the resulting data frame
#' head(sample_data_with_scores)
#'
#' # Examine the structure of the data frame
#' str(sample_data_with_scores)

# Load the required libraries
library(tidyverse)
library(tidyr)
library(tidytext)
library(ggplot2)
library(syuzhet)
library(tm)

# Function to analyze sentiments
analyze_sentiments <- function(data, text_columns, methods = c("syuzhet", "afinn", "bing"), preprocess = FALSE, remove_stopwords = FALSE) {
  afinn <- get_sentiments("afinn")
  lexicon_bing <- get_sentiments("bing")
  stop_words <- get_stopwords()
  
  if (preprocess || remove_stopwords) {
    for (col_name in text_columns) {
      if (preprocess) {
        data[[col_name]] <- str_to_lower(data[[col_name]])
        data[[col_name]] <- str_replace_all(data[[col_name]], "[^[:alnum:][:space:]]", " ")
        data[[col_name]] <- str_squish(data[[col_name]])
      }
      
      if (remove_stopwords) {
        data <- data %>%
          mutate(!!col_name := removeWords(!!sym(col_name), stop_words$word))
      }
    }
  }
  
  data <- data %>% mutate(rn = row_number())
  
  for (col_name in text_columns) {
    for (method in methods) {
      df_tidy <- data %>% unnest_tokens(word, !!sym(col_name))
      
      if (method == "syuzhet") {
        df_tidy <- df_tidy %>% mutate(sentiment = as.numeric(get_sentiment(word)))
      } else if (method == "afinn") {
        df_tidy <- df_tidy %>% left_join(afinn, by = "word") %>% replace_na(list(value = 0)) %>% mutate(sentiment = as.numeric(value)) %>% select(-value)
      } else if (method == "bing") {
        df_tidy <- df_tidy %>% left_join(lexicon_bing, by = "word") %>% replace_na(list(sentiment = "neutral")) %>% mutate(sentiment = as.numeric(if_else(sentiment == "positive", 1, if_else(sentiment == "negative", -1, 0))))
      }
      
      df_tidy_sum <- df_tidy %>% group_by(rn) %>% summarize(sentiment_sum = sum(sentiment, na.rm = TRUE), .groups = "drop")
      
      data <- left_join(data, df_tidy_sum, by = "rn") %>% rename_with(~ paste0(col_name, "_", method, "_score"), .cols = "sentiment_sum")
      
      # Add sentiment labels for each cell using method-specific thresholds
      data <- data %>% mutate(!!paste0(col_name, "_", method, "_label") := case_when(
        method == "syuzhet" & !!sym(paste0(col_name, "_", method, "_score")) > 0.5 ~ "positive",
        method == "syuzhet" & !!sym(paste0(col_name, "_", method, "_score")) < -0.5 ~ "negative",
        method == "afinn" & !!sym(paste0(col_name, "_", method, "_score")) > 5 ~ "positive",
        method == "afinn" & !!sym(paste0(col_name, "_", method, "_score")) < -5 ~ "negative",
        method == "bing" & !!sym(paste0(col_name, "_", method, "_score")) > 1 ~ "positive",
        method == "bing" & !!sym(paste0(col_name, "_", method, "_score")) < -1 ~ "negative",
        TRUE ~ "neutral"
      ))
    }
    
    # Add an overall label for each text column per row based on the majority sentiment
    data <- data %>% mutate(!!paste0(col_name, "_majority_label") := case_when(
      rowSums(select(., starts_with(paste0(col_name, "_")), -ends_with("_label")) > 0) > 1 ~ "positive",
      rowSums(select(., starts_with(paste0(col_name, "_")), -ends_with("_label")) < 0) > 1 ~ "negative",
      TRUE ~ "neutral"
    ))
  }
  
  return(data)
}