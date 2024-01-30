# Load the required library
library(lubridate)
library(ggplot2)
library(reshape2)

set.seed(123)

# Generate sequence of dates for 2022
dates <- seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "day")

# Generate a random number between 1 and 5 for each date
random_numbers <- sample(1:5, length(dates), replace = TRUE)

# Combine dates and random numbers
result <- data.frame(Date = dates, RandomNumber = random_numbers)

# Reshape
result_melted <- melt(result, id.vars = "Date")

# Extract day and month from the Date
result$Day <- day(result$Date)
result$Month <- month(result$Date)

# Create a complete grid of days and months
grid <- expand.grid(Day = 1:31, Month = 1:12)
grid$MonthName <- month.abb[grid$Month]

# Merge the grid with the result data
merged_result <- merge(grid, result, by = c("Day", "Month"), all.x = TRUE)

# Mark the non-existent days, e.g., February 31st
merged_result$ValidDay <- !is.na(merged_result$RandomNumber)
merged_result$RandomNumber[!merged_result$ValidDay] <- NA

# Create the heatmap with labels and gray out the invalid days
ggplot(merged_result, aes(x = MonthName, y = Day, fill = RandomNumber)) +
  geom_tile(color = "white", aes(fill = ifelse(ValidDay, RandomNumber, NA))) +
  geom_text(aes(label = ifelse(ValidDay, RandomNumber, "")), color = "black", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "darkgray") +
  scale_y_continuous(breaks = 1:31) + # Add each day to the y-axis
  theme_minimal() +
  labs(x = "Month", y = "Day", fill = "Random Number", 
       title = "Calendar Heatmap of 1 Random Number for Each Day in 2023") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(color = "black"),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))