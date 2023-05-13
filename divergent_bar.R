#' A divergent bar plot
#' 
#' This function creates a divergent barplot using ggplot2 to visualize the positive and negative values of two columns in a data frame.
#' 
#' @param data a data frame containing the columns to be plotted
#' @param col_pos a column name or column index for the positive values
#' @param col_neg a column name or column index for the negative values
#' 
#' @return a ggplot2 object
#' 
#' @import ggplot2
#' @import dplyr
#' 
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data(mtcars)
#' divergent_bar(mtcars, mpg, qsec)
#' 
#' @export
# Load packages
library(ggplot2)
library(dplyr)
# Save function
divergent_bar <- function(data, col_pos, col_neg) {
  # Subset data into positive and negative columns
  data_pos <- data %>% pull({{col_pos}})
  data_neg <- data %>% pull({{col_neg}})
  
  # Calculate the maximum and minimum values for the y-axis limits
  y_limits <- c(-max(data_neg, na.rm = TRUE), max(data_pos, na.rm = TRUE))
  
  # Arrange data by the positive column
  data <- arrange(data, desc({{col_pos}}))
  
  # Create a divergent barplot using ggplot2
  ggplot(data, aes(x = rownames(data), y = data_pos)) +
    geom_bar(stat = "identity", fill = "#0072B2") +
    geom_bar(aes(y = -data_neg), stat = "identity", fill = "#F0E442") +
    coord_flip() +
    ylim(y_limits) +
    theme_classic() +
    labs(x = "", y = "") +
    guides(fill=guide_none()) # Remove the legend for fill
}