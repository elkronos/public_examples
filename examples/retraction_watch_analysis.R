# Get data here: https://www.crossref.org/blog/news-crossref-and-retraction-watch/
# Shiny applicaiton here: https://github.com/elkronos/shiny_examples/blob/main/analysis/retraction_watch_by_country.R

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(stringr)
library(forcats)

# Load your data
data <- read_csv("data.csv")

# Check if 'RetractionDate', 'Country', and 'Reason' columns exist
required_columns <- c('RetractionDate', 'Country', 'Reason')
if (!all(required_columns %in% colnames(data))) {
  stop("Required column not found in the dataset")
}

# Expand data so that each country in the 'Country' field gets its own row
data <- data %>%
  mutate(Country = str_split(Country, ";")) %>%
  unnest(Country)

# Exclude entries where Country is "Unknown"
data <- data %>% filter(Country != "Unknown")

# Convert 'RetractionDate' to datetime and extract the year
data$RetractionYear <- year(mdy_hm(data$RetractionDate, tz="UTC", quiet = TRUE))

# Define categories and their corresponding keywords
categories <- list(
  Plagiarism_and_Duplication = c("+Duplication of Article", "+Plagiarism of Data", "+Plagiarism of Text",
                                 "+Euphemisms for Plagiarism", "+Euphemisms for Duplication",
                                 "+Concerns about Referencing/Attributions", "+Taken from Dissertation/Thesis",
                                 "+Duplication of Text"),
  Data_Integrity = c("+Falsification/Fabrication of Data", "+Unreliable Data", "+Error in Data",
                     "+Original Data not Provided", "+Duplication of Image", "+Manipulation of Images"),
  Authorship_and_Ethical_Concerns = c("+Concerns about Authorship", "+Objections by Author(s)", "+Withdrawal",
                                      "+False/Forged Authorship", "+False Affiliation", "+Complaints about Author",
                                      "+Conflict of Interest", "+Ethical Violations by Author",
                                      "+Lack of IRB/IACUC Approval", "+Informed/Patient Consent - None/Withdrawn"),
  Publication_and_Review_Processes = c("+Duplicate Publication through Error by Journal/Publisher",
                                       "+Publishing Ban", "+Complaints about Company/Institution",
                                       "+Complaints about Third Party", "+Fake Peer Review", "+Paper Mill",
                                       "+Concerns/Issues with Peer Review"),
  Research_Quality_and_Integrity = c("+Error in Text", "+Unreliable Results", "+Concerns/Issues About Results",
                                     "+Contamination of Materials", "+Error in Analyses", "+Results Not Reproducible",
                                     "+Error in Results and/or Conclusions", "+Hoax Paper", "+Bias Issues or Lack of Balance",
                                     "+Randomly Generated Content"),
  Legal_and_Official_Investigations = c("+Investigation by Journal/Publisher", "+Investigation by Company/Institution",
                                        "+Investigation by Third Party", "+Criminal Proceedings", "+Civil Proceedings",
                                        "+Investigation by ORI", "+Legal Reasons/Legal Threats", "+Misconduct - Official Investigation/Finding"),
  External_Influences_and_Rights = c("+Lack of Approval from Third Party", "+Transfer of Copyright/Ownership",
                                     "+Copyright Claims", "+Rogue Editor")
)

# Create new categories with counts for each article
for (category in names(categories)) {
  data[[category]] <- sapply(data$Reason, function(x) {
    keyword_in_x <- sapply(categories[[category]], function(keyword) grepl(keyword, x))
    as.integer(any(keyword_in_x))
  })
}

# Filtering data for the last 10 years
recent_data <- data %>% 
  filter(RetractionYear >= max(RetractionYear, na.rm = TRUE) - 10)

# Filtering countries with at least 1000 entries
country_counts <- recent_data %>% count(Country) %>% filter(n >= 500)
data_filtered <- recent_data %>% filter(Country %in% country_counts$Country)

# Group by country and calculate the top reasons for each country
grouped_data <- data_filtered %>% 
  group_by(Country, RetractionYear) %>% 
  summarise(across(all_of(names(categories)), sum, na.rm = TRUE)) %>%
  # Normalize to percentages
  group_by(Country, RetractionYear) %>%
  mutate(total = rowSums(across(all_of(names(categories))))) %>%
  mutate(across(all_of(names(categories)), ~ . / total * 100)) %>%
  select(-total)

# Melt the data for plotting
melted_data <- melt(grouped_data, id.vars = c("Country", "RetractionYear"))

# Melt the data for plotting
melted_data <- melt(grouped_data, id.vars = c("Country", "RetractionYear"))

# Clean and sort category names
melted_data$variable <- gsub("_", " ", melted_data$variable)
# Sort the factor levels so that A appears at the top
melted_data$variable <- factor(melted_data$variable, levels = sort(unique(melted_data$variable)))

# Explicitly specify the order of categories
ordered_categories <- c("Research Quality and Integrity",
                        "Publication and Review Processes", 
                        "Plagiarism and Duplication", 
                        "Legal and Official Investigations", 
                        "External Influences and Rights",
                        "Data Integrity", 
                        "Authorship and Ethical Concerns")

# Set the factor levels to the specified order
melted_data$variable <- factor(melted_data$variable, levels = ordered_categories)

# Plotting heatmaps
countries_to_plot <- unique(melted_data$Country)
for (country in countries_to_plot) {
  country_data <- melted_data %>% filter(Country == country)
  median_value <- median(country_data$value, na.rm = TRUE)
  
  p <- ggplot(country_data, aes(x = RetractionYear, y = variable, fill = value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.1f%%", value)), vjust = 1) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median_value) +
    labs(title = paste("Heatmap for", country), x = "Year", y = "Category") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}
