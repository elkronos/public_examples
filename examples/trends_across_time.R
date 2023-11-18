# Load data: https://fred.stlouisfed.org/series/COMPUTSA
library(readxl)
completed_houses <- read_excel("completed_houses.xlsx")

# Plot original data
library(ggplot2)
ggplot(completed_houses, aes(x = observation_date, y = COMPUTSA)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
library(dplyr)
library(lubridate)

# Modify original data to include an interval column
completed_houses$interval <- "Original"
completed_houses$aggregation_period <- completed_houses$observation_date

# Function to aggregate data
aggregate_data <- function(data, interval) {
  unit_string <- if (interval == 1) {
    "year"
  } else if (interval == 3) {
    "3 years"
  } else if (interval == 5) {
    "5 years"
  } else {
    stop("Invalid interval")
  }
  
  data %>%
    group_by(aggregation_period = floor_date(observation_date, unit = unit_string)) %>%
    summarize(COMPUTSA = mean(COMPUTSA, na.rm = TRUE), .groups = 'drop') %>%
    mutate(interval = sprintf("%d Years", interval))
}

# Aggregate data into 1, 3, and 5 year increments
data_1_year <- aggregate_data(completed_houses, 1)
data_3_year <- aggregate_data(completed_houses, 3)
data_5_year <- aggregate_data(completed_houses, 5)

# Combine aggregated data with the original data
combined_data <- bind_rows(completed_houses, data_1_year, data_3_year, data_5_year)

# Plot with faceting
ggplot(combined_data, aes(x = aggregation_period, y = COMPUTSA)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~ interval, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

