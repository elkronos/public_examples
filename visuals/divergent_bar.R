#' Create a Divergent Barplot
#'
#' This function creates a divergent barplot to compare values on the left and right side within different categories.
#'
#' @param data A data frame containing the data to be plotted.
#' @param col_left The column name or index representing the values on the left side.
#' @param col_right The column name or index representing the values on the right side.
#' @param col_cat The column name or index representing the categories.
#' @param title An optional title for the plot. Default is NULL.
#' @param left_color The color for the values on the left side. Default is "#0072B2".
#' @param right_color The color for the values on the right side. Default is "#F0E442".
#' @param left_label The label for the values on the left side. Default is "Left".
#' @param right_label The label for the values on the right side. Default is "Right".
#' @param save_to_file An optional file path to save the plot. Default is NULL.
#'
#' @return If `save_to_file` is specified, the function saves the plot to the specified file and returns NULL.
#'         Otherwise, it returns the generated ggplot2 object.
#'
#' @importFrom tidyverse ggplot2 pull arrange coord_flip scale_fill_manual
#' @importFrom scales pretty_breaks
#' @importFrom grDevices png
#'
#' @examples
#' # Create synthetic data
#' set.seed(42)
#' n_rows <- 10
#' data <- tibble(
#'   customers = LETTERS[1:n_rows],
#'   market_campaign_a = sample(50:150, n_rows),
#'   market_campaign_b = sample(30:100, n_rows)
#' )
#'
#' # Use the `divergent_bar` function with the synthetic data
#' divergent_bar(data, col_left = market_campaign_b, 
#'               col_right = market_campaign_a, col_cat = customers,
#'               title = "Comparison of Campaigns A and B",
#'               left_color = "#F39C12", right_color = "#2E9BDA",
#'               left_label = "Method A", right_label = "Method B")
#'
#' @export
library(tidyverse)

divergent_bar <- function(data, col_left, col_right, col_cat, 
                          title = NULL, 
                          left_color = "#0072B2", right_color = "#F0E442",
                          left_label = "Left", right_label = "Right",
                          save_to_file = NULL) {
  # Subset data into left and right columns
  data_left <- data %>% pull({{col_left}})
  data_right <- data %>% pull({{col_right}})
  data_cat <- data %>% pull({{col_cat}})
  
  # Combine the data into a single data frame
  combined_data <- data.frame(Category = data_cat, Type = rep(c(left_label, right_label), each = nrow(data)),
                              Value = c(-data_left, data_right)) # Negate the values on the left side
  
  # Sort the data based on the categorical variable
  combined_data <- combined_data %>% arrange(desc(Category))
  
  # Create a divergent barplot using ggplot2
  plot <- ggplot(combined_data, aes(x = reorder(Category, as.numeric(desc(Category))), y = Value, fill = Type)) +
    geom_bar(stat = "identity", position = "identity") +
    coord_flip() +
    scale_fill_manual(values = c(left_color, right_color), labels = c(left_label, right_label)) +
    scale_y_continuous(labels = function(x) abs(x), breaks = scales::pretty_breaks()) +
    theme_classic() +
    labs(x = "", y = "", title = title) +
    theme(legend.position = "bottom") # Move the legend to the bottom
  
  # Save the plot to a file if the save_to_file argument is specified
  if (!is.null(save_to_file)) {
    ggsave(filename = save_to_file, plot = plot)
  } else {
    # Return the plot
    return(plot)
  }
}