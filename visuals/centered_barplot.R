# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(glue)
library(ggtext)

#' Centered Bar plot Function
#'
#' This function creates a centered bar plot, with options to display values above and below a specified threshold as different colors. Additional options include adding labels and a title to the plot.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x A string representing the column name for the x-axis variable.
#' @param y A string representing the column name for the y-axis variable.
#' @param threshold A numeric value representing the threshold to separate the bars in the plot. If NULL (default), it calculates the mean of the y column as the threshold.
#' @param add_labels A logical value indicating whether to add labels to the bars. Default is TRUE.
#' @param add_title A logical value indicating whether to add a title to the plot. Default is FALSE.
#'
#' @importFrom ggplot2 ggplot geom_bar geom_hline theme_minimal theme labs scale_fill_manual
#' @importFrom dplyr group_by summarise
#' @importFrom scales percent
#' @importFrom glue glue
#' @importFrom ggtext element_markdown
#' 
#' @examples
#' \dontrun{
#' set.seed(123)
#' example_data <- data.frame(
#'   Channel = sample(letters[1:5], 100, replace = TRUE),
#'   Sales = rnorm(100, 50, 15)
#' )
#' 
#' centered_barplot(example_data, "Channel", "Sales", add_title = TRUE)
#' }
#' 
#' @return A ggplot object representing the centered barplot.
#' @export
centered_barplot <- function(data, x, y, threshold = NULL, add_labels = TRUE, add_title = FALSE) {
  
  # Step 1: Calculate the threshold if not provided
  if (is.null(threshold)) {
    threshold <- mean(data[[y]], na.rm = TRUE)
  }
  
  # Step 2: Prepare data summary
  data_summary <- data %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarise(
      above_threshold = sum(.data[[y]] >= threshold, na.rm = TRUE) / n(),
      below_threshold = sum(.data[[y]] < threshold, na.rm = TRUE) / n(),
      .groups = 'drop'
    )
  
  # Step 3: Create initial plot
  plot <- ggplot(data_summary, aes(x = .data[[x]])) +
    geom_bar(aes(y = above_threshold, fill = "Above"), stat = "identity", show.legend = FALSE) +
    geom_bar(aes(y = -below_threshold, fill = "Below"), stat = "identity", show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype="solid", color = "black") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, by = 0.2)) +
    labs(y = "Percentage", x = paste("Levels of", x)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12)
    ) +
    scale_fill_manual(values = c("Above" = "#1F78B4", "Below" = "#FF7F00"))
  
  # Step 4: Add title if required
  if (add_title) {
    plot <- plot + 
      labs(title = glue::glue("<span style='font-weight: normal;'>Percentage of <span style='color:#3D3D3D;'>{y}</span> above and below <span style='color:#3D3D3D;'>{round(threshold, 2)}</span> for each level of <span style='color:#3D3D3D;'>{x}</span></span>")) + 
      theme(plot.title = element_markdown())
  }
  
  # Step 5: Add labels if required
  if (add_labels) {
    plot <- plot + 
      geom_text(aes(y = above_threshold, label = scales::percent(above_threshold)), vjust = -0.25, color = "black", size = 3.3, angle = 0, lineheight = 0.8) +
      geom_text(aes(y = -below_threshold, label = scales::percent(below_threshold)), vjust = 1.25, color = "black", size = 3.3, angle = 0, lineheight = 0.8)
  }
  
  return(plot)
}