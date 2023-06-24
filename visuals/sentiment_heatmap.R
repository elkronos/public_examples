#' Generate a sentiment heatmap for text data
#'
#' This function takes text data with pre-calculated sentiment scores and generates a heatmap
#' showing the sentiment scores for each text item, method, and text column combination.
#'
#' @param sentiment_data A data frame containing text data with pre-calculated sentiment scores.
#' @param text_columns A vector of column names in the \code{sentiment_data} data frame containing text data.
#' @param methods A vector of sentiment analysis methods to include in the heatmap. Default is \code{c("syuzhet", "afinn", "bing")}.
#'
#' @return A ggplot2 object containing the sentiment heatmap.
#'
#' @examples
#' \dontrun{
#' # Load sample data
#' data(sample_data_with_scores)
#'
#' # Generate sentiment heatmap
#' sentiment_plot <- sentiment_heatmap(sample_data_with_scores, 
#'                                     text_columns = c("product", "customer_service",
#'                                                      "loyalty", "recommend"))
#'
#' # Display the plot
#' print(sentiment_plot)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom stringr gsub grepl
#' @export

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Save function
sentiment_heatmap <- function(sentiment_data, text_columns, methods = c("syuzhet", "afinn", "bing")) {
  # Pre-process data to extract scores
  scores_data <- sentiment_data %>%
    select(starts_with(paste0(text_columns, "_")), ends_with("_score")) %>%
    mutate(text_item = row_number()) %>%
    gather(key = "variable", value = "score", -text_item)
  
  # Remove non-numeric score values
  scores_data <- scores_data %>%
    filter(grepl("^[0-9.-]+$", score))
  
  # Convert scores to numeric
  scores_data <- scores_data %>%
    mutate(score = as.numeric(score))
  
  # Extract method and text column from variable names
  scores_data <- scores_data %>%
    mutate(method = gsub(".*_([a-z]+)_(score|label)", "\\1", variable),
           text_column = gsub("([^_]+)_.*", "\\1", variable)) %>%
    select(-variable) %>%
    filter(method %in% methods)  # Filter out rows with the "majority" method
  
  # Calculate average score for each text item by method and text_column
  avg_score_data <- scores_data %>%
    group_by(text_item, method, text_column) %>%
    summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
    distinct()
  
  # Calculate overall average score for each unique method and text_column combination
  overall_avg_score_data <- avg_score_data %>%
    group_by(method, text_column) %>%
    summarise(overall_avg_score = mean(avg_score, na.rm = TRUE), .groups = "drop")
  
  # Merge overall average scores with the avg_score_data
  avg_score_data <- left_join(avg_score_data, overall_avg_score_data, by = c("method", "text_column"))
  
  # Function to calculate labels based on custom score cut-offs
  get_label <- function(method, avg_score) {
    cut_offs <- c(-Inf, Inf)
    if (method == "syuzhet") {
      cut_offs <- c(-0.5, 0.5)
    } else if (method == "afinn") {
      cut_offs <- c(-5, 5)
    } else if (method == "bing") {
      cut_offs <- c(-1, 1)
    }
    if (avg_score < cut_offs[1]) {
      return("negative")
    } else if (avg_score > cut_offs[2]) {
      return("positive")
    } else {
      return("neutral")
    }
  }
  
  # Recalculate labels based on overall average scores and custom score cut-offs
  avg_score_data <- avg_score_data %>%
    mutate(label = mapply(get_label, method, overall_avg_score))
  
  # Create the heatmap
  plot <- ggplot(avg_score_data, aes(x = method, y = text_column, fill = label)) +
    geom_tile() +
    geom_text(aes(label = round(overall_avg_score, 2)), color = "white", size = 4) +
    scale_fill_manual(values = c("negative" = "yellow", "neutral" = "gray", "positive" = "blue")) + # Corrected color mapping
    labs(title = "Sentiment Heatmap",
         x = "Method",
         y = "Text Item") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))
  
  return(plot)
}