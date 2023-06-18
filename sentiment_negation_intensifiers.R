# Install and load necessary libraries
library(udpipe)
library(dplyr)
library(purrr)
library(syuzhet)
library(textdata)

#' Detect Negation and Intensifiers in Text
#'
#' This function detects negation words and intensifiers in the input text. It performs dependency parsing on the text using the udpipe package and determines the sentiment scores of words using various lexicons. Negation words reverse the sentiment scores of subsequent words, while intensifiers double the sentiment scores of subsequent words. The function returns a data frame containing the words, their sentiment scores, and indicators for negation words and intensifiers.
#'
#' @param input_text A character string containing the input text.
#' @param ud_model The pre-loaded udpipe model for performing dependency parsing. Default is NULL.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{word}{The word extracted from the input text.}
#'   \item{upos}{The Universal Part-of-Speech tag of the word.}
#'   \item{head_token_id}{The token ID of the word's head (based on dependency parsing).}
#'   \item{value_afinn}{The sentiment score of the word from the AFINN lexicon.}
#'   \item{value_nrc}{The sentiment score of the word from the NRC lexicon.}
#'   \item{value_bing}{The sentiment score of the word from the Bing lexicon.}
#'   \item{value_loughran}{The sentiment score of the word from the Loughran lexicon.}
#'   \item{is_negation}{A logical indicator for negation words. TRUE if the word is a negation word, FALSE otherwise.}
#'   \item{is_intensifier}{A logical indicator for intensifiers. TRUE if the word is an intensifier, FALSE otherwise.}
#' }
#'
#' @importFrom udpipe udpipe_annotate udpipe_download_model udpipe_load_model
#' @importFrom dplyr left_join rename mutate
#' @importFrom purrr map
#' @importFrom syuzhet get_sentiments
#' @importFrom textdata get_sentiments
#'
#' @examples
#' # Define input text and load the udpipe model
#' input_text <- "I am not unhappy about this."
#' ud_model <- udpipe_download_model(language = "english")
#' ud_model <- udpipe_load_model(file = ud_model$file_model)
#'
#' # Apply the function to the input text
#' result <- detect_negation(input_text, ud_model)
#' print(result)
#'
#' @export
# Download and load the English model for udpipe
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(file = ud_model$file_model)
# Define negation words
negation_words <- c("not", "no", "never", "neither", "nor", "none", "nothing", "nowhere", "hardly", "barely", "rarely", "seldom", "scarcely")
# Define intensity modifiers
intensifiers <- c("very", "so", "really", "quite", "extremely")
# Load AFINN lexicon
afinn <- get_sentiments("afinn")
# Load NRC lexicon
nrc <- get_sentiments("nrc") %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1))  # Assign numeric scores to NRC sentiments
# Load Bing lexicon
bing <- get_sentiments("bing") %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1))  # Assign numeric scores to Bing sentiments
# Load Loughran lexicon
loughran <- get_sentiments("loughran") %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1))  # Assign numeric scores to Loughran sentiments
# Define function
detect_negation <- function(input_text, ud_model) {
  
  if (!is.character(input_text)) {
    stop("Input must be a character string.")
  }
  
  # Perform dependency parsing
  annotated_text <- udpipe_annotate(ud_model, x = input_text)
  annotated_text <- as.data.frame(annotated_text)
  
  # Create a data frame to store words and their sentiment scores
  words_df <- data.frame(word = annotated_text$lemma, upos = annotated_text$upos, head_token_id = annotated_text$head_token_id, stringsAsFactors = FALSE)
  
  # Add a column for sentiment scores from AFINN, NRC, Bing, and Loughran lexicons
  words_df <- words_df %>%
    left_join(afinn, by = c("word" = "word")) %>%
    rename(value_afinn = value) %>%
    left_join(nrc, by = c("word" = "word")) %>%
    rename(value_nrc = score) %>%
    left_join(bing, by = c("word" = "word")) %>%
    rename(value_bing = score) %>%
    left_join(loughran, by = c("word" = "word")) %>%
    rename(value_loughran = score)
  
  # Add a column to indicate whether the word is a negation word or an intensifier
  words_df <- words_df %>%
    mutate(is_negation = ifelse(word %in% negation_words, TRUE, FALSE),
           is_intensifier = ifelse(word %in% intensifiers, TRUE, FALSE))
  
  # Handle negations and intensifiers
  for (i in 1:nrow(words_df)) {
    if (words_df$is_negation[i]) {
      words_df$value_afinn[i + 1] <- -words_df$value_afinn[i + 1]
      words_df$value_nrc[i + 1] <- -words_df$value_nrc[i + 1]
      words_df$value_bing[i + 1] <- -words_df$value_bing[i + 1]
      words_df$value_loughran[i + 1] <- -words_df$value_loughran[i + 1]
    }
    if (words_df$is_intensifier[i]) {
      words_df$value_afinn[i + 1] <- 2 * words_df$value_afinn[i + 1]
      words_df$value_nrc[i + 1] <- 2 * words_df$value_nrc[i + 1]
      words_df$value_bing[i + 1] <- 2 * words_df$value_bing[i + 1]
      words_df$value_loughran[i + 1] <- 2 * words_df$value_loughran[i + 1]
    }
  }
  
  return(words_df)
}

#' Create plots for each lexicon
#'
#' This function generates a list of plots, with each plot representing a lexicon. The plots visualize the sentiment scores for words in the lexicons.
#'
#' @param lexicons A character vector specifying the lexicons to be plotted.
#' @param scores_long A data frame containing the sentiment scores for words.
#'
#' @return A grid plot object that displays the plots for each lexicon.
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_hline labs scale_fill_manual theme_bw ggtitle
#' @import cowplot::plot_grid
#'
#' @examples
#' 
#' plots <- lapply(lexicons, function(lexicon) {
#'   plot <- ggplot(scores_long %>% filter(Lexicon == lexicon), aes(x = word, y = Score)) +
#'     geom_col(aes(fill = ifelse(Score >= 0, "positive", "negative")), position = "identity") +
#'     geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
#'     labs(x = "Word", y = "Sentiment Score") +
#'     scale_fill_manual(values = c("positive" = "skyblue", "negative" = "lightcoral")) +
#'     theme_bw() +
#'     theme(legend.position = "none") +
#'     ggtitle(lexicon)  # Add lexicon name as the plot title
#' })
#' 
#' # Arrange the plots in a grid
#' grid_plot <- cowplot::plot_grid(plotlist = plots, ncol = 1)
#' 
#' # Display the grid of plots
#' print(grid_plot)
#' 
plots <- lapply(lexicons, function(lexicon) {
  plot <- ggplot(scores_long %>% filter(Lexicon == lexicon), aes(x = word, y = Score)) +
    geom_col(aes(fill = ifelse(Score >= 0, "positive", "negative")), position = "identity") +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    labs(x = "Word", y = "Sentiment Score") +
    scale_fill_manual(values = c("positive" = "skyblue", "negative" = "lightcoral")) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(lexicon)  # Add lexicon name as the plot title
})