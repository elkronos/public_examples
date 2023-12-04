# Load libraries
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(stringr)

########################################################### GPT API Call Functions

# Add API key
api_key <- "sk-your-key"

# Function to call the GPT API
call_gpt_api <- function(api_key, model, temperature, max_tokens, prompt) {
  url <- "https://api.openai.com/v1/chat/completions"
  headers <- c(
    'Authorization' = paste('Bearer', api_key),
    'Content-Type' = 'application/json'
  )
  body <- list(
    model = model,
    messages = list(list("role" = "user", "content" = prompt))
  )
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  response <- RETRY("POST", url, add_headers(.headers = headers), body = body_json, encode = "json")
  stop_for_status(response)
  content(response, "parsed")
}

# Function to call the API multiple times for a single prompt
send_prompts <- function(api_key, model, temperature, max_tokens, prompt, times) {
  responses <- vector("list", times)
  for (i in 1:times) {
    cat("Sending prompt", i, "of", times, "...\n")
    tryCatch({
      response <- call_gpt_api(api_key, model, temperature, max_tokens, prompt)
      responses[[i]] <- response$choices[[1]]$message$content
      cat("Received response for prompt", i, "\n")
      Sys.sleep(1)  # Rate limit delay
    }, error = function(e) {
      cat("Error on request", i, ":", e$message, "\n")
      responses[[i]] <- NA
    })
  }
  return(responses)
}

# Main function to process multiple prompts
process_prompts <- function(prompts, times, api_key, model = "gpt-3.5-turbo", temperature = 0.5, max_tokens = 2000) {
  results <- data.frame(prompt = character(), response = character(), stringsAsFactors = FALSE)
  for (prompt in prompts) {
    responses <- send_prompts(api_key, model, temperature, max_tokens, prompt, times)
    prompt_responses <- data.frame(prompt = rep(prompt, length(responses)), response = unlist(responses), stringsAsFactors = FALSE)
    results <- rbind(results, prompt_responses)
  }
  return(results)
}

# User settings
prompts <- c("I need you to write an essay. In this essay, succinctly compare and contrast Albert Bandura's Social Learning Theory with B.F. Skinner's Operant Conditioning. Outline the main principles of each theory, focusing on Bandura's emphasis on observational learning and modeling, and Skinner's focus on behavior shaping through reinforcement and punishment. Examine their differing perspectives on cognitive involvement and environmental influence in learning. Conclude by assessing the strengths and weaknesses of each theory and their implications in understanding human behavior, drawing on relevant examples from your life.", 
             "I need you to write an essay and I will give you a $20 tip if you do well. In this essay, succinctly compare and contrast Albert Bandura's Social Learning Theory with B.F. Skinner's Operant Conditioning. Outline the main principles of each theory, focusing on Bandura's emphasis on observational learning and modeling, and Skinner's focus on behavior shaping through reinforcement and punishment. Examine their differing perspectives on cognitive involvement and environmental influence in learning. Conclude by assessing the strengths and weaknesses of each theory and their implications in understanding human behavior, drawing on relevant examples from your life.",
             "I need you to write an essay and I will give you a $200 tip if you do well. In this essay, succinctly compare and contrast Albert Bandura's Social Learning Theory with B.F. Skinner's Operant Conditioning. Outline the main principles of each theory, focusing on Bandura's emphasis on observational learning and modeling, and Skinner's focus on behavior shaping through reinforcement and punishment. Examine their differing perspectives on cognitive involvement and environmental influence in learning. Conclude by assessing the strengths and weaknesses of each theory and their implications in understanding human behavior, drawing on relevant examples from your life.",
             "I need you to write an essay but I cannot give you a reward. In this essay, succinctly compare and contrast Albert Bandura's Social Learning Theory with B.F. Skinner's Operant Conditioning. Outline the main principles of each theory, focusing on Bandura's emphasis on observational learning and modeling, and Skinner's focus on behavior shaping through reinforcement and punishment. Examine their differing perspectives on cognitive involvement and environmental influence in learning. Conclude by assessing the strengths and weaknesses of each theory and their implications in understanding human behavior, drawing on relevant examples from your life.",
             "I need you to write an essay and I will give you anything you want if you do well. In this essay, succinctly compare and contrast Albert Bandura's Social Learning Theory with B.F. Skinner's Operant Conditioning. Outline the main principles of each theory, focusing on Bandura's emphasis on observational learning and modeling, and Skinner's focus on behavior shaping through reinforcement and punishment. Examine their differing perspectives on cognitive involvement and environmental influence in learning. Conclude by assessing the strengths and weaknesses of each theory and their implications in understanding human behavior, drawing on relevant examples from your life.",
             "I need you to write an essay and I need you to do well or I might lose my job. In this essay, succinctly compare and contrast Albert Bandura's Social Learning Theory with B.F. Skinner's Operant Conditioning. Outline the main principles of each theory, focusing on Bandura's emphasis on observational learning and modeling, and Skinner's focus on behavior shaping through reinforcement and punishment. Examine their differing perspectives on cognitive involvement and environmental influence in learning. Conclude by assessing the strengths and weaknesses of each theory and their implications in understanding human behavior, drawing on relevant examples from your life.")
num_times <- 50  # Number of times each prompt is sent

# Process prompts
all_responses <- process_prompts(prompts, num_times, api_key)

# Save the results to a CSV file
write.csv(all_responses, "gpt_responses.csv", row.names = FALSE)

# Save as new object
data <- all_responses

########################################################### Analysis ETL

# Assuming 'data' is your data frame and 'prompt' is the column with the prompts
data$categorical_prompt <- ifelse(data$prompt == prompts[1], "Baseline",
                                  ifelse(data$prompt == prompts[2], "20 Reward",
                                         ifelse(data$prompt == prompts[3], "200 Reward",
                                                ifelse(data$prompt == prompts[4], "No Reward",
                                                       ifelse(data$prompt == prompts[5], "Any Reward",
                                                              ifelse(data$prompt == prompts[6], "Job Loss", NA))))))

# Print to check
print(data$categorical_prompt)

# Gunning Fog Index
gunning_fog_index <- function(words, sentences, complex_words) {
  return(0.4 * ((words / sentences) + 100 * (complex_words / words)))
}

# Coleman-Liau Index
coleman_liau_index <- function(characters, words, sentences) {
  return(0.0588 * (characters / words * 100) - 0.296 * (sentences / words * 100) - 15.8)
}

# SMOG Index
smog_index <- function(poly_words, sentences) {
  return(sqrt(30 * (poly_words / sentences)) + 3)
}

# Lexical Diversity
lexical_diversity <- function(words) {
  return(length(unique(words)) / length(words))
}

# Modified Reading Level Metrics Function
reading_level_metrics <- function(df, column_name) {
  metrics_df <- data.frame()
  
  for(i in 1:nrow(df)) {
    text <- as.character(df[i, column_name])
    
    # Split the text into words
    words <- unlist(strsplit(text, "\\W+"))
    num_words <- length(words)
    num_sentences <- length(gregexpr("[[:punct:]]+[[:space:]]+[[:upper:]]", text)[[1]]) + 1
    num_complex_words <- sum(sapply(words, function(w) {nchar(w) > 2 & nchar(gsub("[aeiouy]", "", w)) > 1}))
    num_characters <- nchar(gsub("\\s", "", text)) # remove spaces for character count
    lex_diversity <- lexical_diversity(words)
    
    # Calculate metrics
    fk_grade_level <- 0.39 * (num_words / num_sentences) + 11.8 * (num_characters / num_words) - 15.59
    gf_index <- gunning_fog_index(num_words, num_sentences, num_complex_words)
    cl_index <- coleman_liau_index(num_characters, num_words, num_sentences)
    smog_index <- smog_index(num_complex_words, num_sentences)
    
    # Add to the dataframe
    metrics_df <- rbind(metrics_df, data.frame(
      Row = i,
      Num_Words = num_words,
      Num_Sentences = num_sentences,
      Num_Complex_Words = num_complex_words,
      Num_Characters = num_characters,
      Lexical_Diversity = lex_diversity,
      Flesch_Kincaid_Grade_Level = fk_grade_level,
      Gunning_Fog_Index = gf_index,
      Coleman_Liau_Index = cl_index,
      SMOG_Index = smog_index
    ))
  }
  
  return(metrics_df)
}

# Applying it to your data
results <- reading_level_metrics(data, "response")

# Assume df is your existing dataframe
data$RowNumber <- 1:nrow(data)

# Join results to file
left_join(data, results, by = c("RowNumber" = "Row")) -> df


# Z-score normalization function
z_normalize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Calculate Z-score normalized means for each metric
means_df <- df %>%
  group_by(categorical_prompt) %>%
  summarise(across(Num_Words:SMOG_Index, ~ mean(., na.rm = TRUE))) %>%
  mutate(across(Num_Words:SMOG_Index, z_normalize))

# Convert categorical_prompt to an ordered factor, with 'Baseline' first
means_df$categorical_prompt <- factor(means_df$categorical_prompt, levels = c("Baseline", unique(df$categorical_prompt[df$categorical_prompt != "Baseline"])))

# Melting data for the faceted barplot
long_df <- pivot_longer(means_df, cols = Num_Words:SMOG_Index, names_to = "Metric", values_to = "Value")

# Faceted barplot with Z-score normalization
ggplot(long_df, aes(x = categorical_prompt, y = Value, fill = categorical_prompt)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_y_continuous(limits = function(x) c(min(x), max(x) + 1)) + # Adjust buffer here
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of Z-Score Normalized Metrics Across Categories", x = "Category", y = "Z-Score Normalized Mean Value") +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3)

# Calculate baseline means
baseline_means <- means_df %>% filter(categorical_prompt == "Baseline")

# Calculating differences from baseline (Z-score normalized)
difference_df <- means_df %>%
  filter(categorical_prompt != "Baseline") %>%
  rowwise() %>%
  mutate(across(Num_Words:SMOG_Index, ~ . - baseline_means[[cur_column()]]))

# Convert categorical_prompt to an ordered factor for difference_df
difference_df$categorical_prompt <- factor(difference_df$categorical_prompt, levels = c("Baseline", unique(df$categorical_prompt[df$categorical_prompt != "Baseline"])))

# Melting data for divergent barplot
long_diff_df <- pivot_longer(difference_df, cols = Num_Words:SMOG_Index, names_to = "Metric", values_to = "Difference")

# Creating divergent barplot with Z-score normalization
ggplot(long_diff_df, aes(x = categorical_prompt, y = Difference, fill = categorical_prompt)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_y_continuous(limits = function(x) c(min(x) - 1, max(x) + 1)) + # Adding buffer to y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Z-Score Normalized Mean Difference from Baseline for Each Category", x = "Category", y = "Z-Score Normalized Mean Difference") +
  geom_text(aes(label = round(Difference, 2)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3)
