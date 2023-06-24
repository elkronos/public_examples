#' Compute percentages by group and value and create a bar plot
#' 
#' This function takes an input data frame, a grouping variable, and a variable to
#' compute percentages for. It groups the data frame by both variables, counts the 
#' number of observations in each group, and computes the percentage of observations 
#' in each group for the specified variable. It then creates a bar plot of the 
#' percentages, with each bar representing a combination of the grouping variable and 
#' the specified variable. The plot includes percentage labels on each bar and is 
#' displayed on the screen. Finally, the function returns the percentages data frame.
#' 
#' @param data Input data frame.
#' @param group_var Name of the column to group by.
#' @param value_var Name of the column that contains the values to compute percentages for.
#' 
#' @return A data frame with percentages by two factors.
#' 
#' @import dplyr
#' @import ggplot2
#' @import viridisLite
#' 
#' @examples
#' \dontrun{
#' stacked_percentages(diamonds, color, cut) -> result
#' }
# Load necessary packages
library(viridisLite)
library(ggplot2)
library(dplyr)
stacked_percentages <- function(data, group_var, value_var) {
  
  # Compute counts and percentages by group and value
  percentages <- data %>%
    group_by({{ group_var }}, {{ value_var }}) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by({{ group_var }}, .drop = TRUE) %>%
    mutate(pct = n / sum(n) * 100)
  
  # Create bar plot of percentages with viridis color palette
  p <- ggplot(percentages, aes(x = {{ group_var }}, y = pct, fill = {{ value_var }})) +
    geom_bar(position = "stack", stat = "identity", color = 'black', width = 0.6) +
    geom_text(aes(label = ifelse(pct >= 2, paste0(round(pct,1), "%"), "")),
              position = position_stack(vjust = 0.5), color = "black", size = 3) +
    scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
    theme_bw() +
    labs(title = "Percentages of one factor within another",
         x = quo_name(enquo(group_var)),
         y = "Percentage") +
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1,
             label = "Percentages lower than 2 are not displayed",
             color = "gray20", size = 3)
  
  # Print plot to screen
  print(p)
  
  # Return the percentages data frame
  return(percentages)
}