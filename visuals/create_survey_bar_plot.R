#' Create a Stacked Bar Plot from Survey Data
#'
#' This function creates a stacked bar plot to visualize survey data.
#' It supports customization of labels, grouping, and interactive plots with Plotly.
#'
#' @param data A data frame containing survey responses.
#' @param columns A character vector of column names in the data frame to be plotted.
#' @param labels A character vector of labels for the response levels.
#' @param group_labels A named character vector for grouping response levels (default is NULL).
#' @param show_percent_labels A logical indicating whether to show percentage labels on the bars (default is TRUE).
#' @param question_texts A character vector of question texts to use as item labels (default is NULL).
#' @param group_by A character string specifying the column name for grouping (default is NULL).
#' @param use_plotly A logical indicating whether to create an interactive Plotly plot (default is FALSE).
#' @param custom_palette A named character vector specifying custom colors for the response levels (default is NULL).
#'
#' @return A ggplot or Plotly object representing the stacked bar plot.
#' @import ggplot2 dplyr tidyr data.table plotly
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   Q1 = sample(1:5, 100, replace = TRUE),
#'   Q2 = sample(1:5, 100, replace = TRUE),
#'   Q3 = sample(1:5, 100, replace = TRUE),
#'   Q4 = sample(1:5, 100, replace = TRUE),
#'   Group = sample(c("Group A", "Group B"), 100, replace = TRUE)
#' )
#' columns1 <- c("Q1", "Q2", "Q3", "Q4")
#' labels1 <- c("Strongly Disagree", "Disagree", "Neither Disagree Nor Agree", "Agree", "Strongly Agree")
#' question_texts1 <- c(
#'   "Q1: The website is easy to navigate",
#'   "Q2: The content is informative",
#'   "Q3: The website loads quickly",
#'   "Q4: I would recommend this website to others"
#' )
#' custom_palette <- c("Strongly Disagree" = "#FF6347", "Disagree" = "#FF8C00", 
#'                     "Neither Disagree Nor Agree" = "#FFD700", "Agree" = "#9ACD32", 
#'                     "Strongly Agree" = "#4682B4")
#' plot1 <- create_bar_plot(df, columns1, labels1, show_percent_labels = TRUE, 
#'                          question_texts = question_texts1, custom_palette = custom_palette)
#' print(plot1)
#' plot2 <- create_bar_plot(df, columns1, labels1, show_percent_labels = TRUE, 
#'                          question_texts = question_texts1, group_by = "Group", 
#'                          custom_palette = custom_palette)
#' print(plot2)
#' plot3 <- create_bar_plot(df, columns1, labels1, show_percent_labels = TRUE, 
#'                          question_texts = question_texts1, group_by = "Group", 
#'                          use_plotly = TRUE, custom_palette = custom_palette)
#' plot3
#' group_labels2 <- c("Strongly Disagree" = "Disagree", "Disagree" = "Disagree", 
#'                    "Neither Disagree Nor Agree" = "Neutral", "Agree" = "Agree", 
#'                    "Strongly Agree" = "Agree")
#' plot4 <- create_bar_plot(df, columns1, labels1, group_labels = group_labels2, 
#'                          show_percent_labels = TRUE, question_texts = question_texts1, 
#'                          group_by = "Group", use_plotly = TRUE, custom_palette = custom_palette)
#' plot4
#' }
#' @export
create_bar_plot <- function(data, columns, labels, group_labels = NULL, show_percent_labels = TRUE, question_texts = NULL, group_by = NULL, use_plotly = FALSE, custom_palette = NULL) {
  dt <- convert_to_dt(data)
  dt <- select_columns(dt, columns)
  dt <- pivot_longer(dt, columns)
  dt <- omit_na(dt)
  dt <- convert_to_factor(dt, labels)
  
  if (!is.null(group_labels)) {
    dt <- group_responses(dt, group_labels)
    dt <- omit_na(dt)
  }
  
  if (!is.null(group_by)) {
    add_group_var(dt, data, group_by, columns)
  }
  
  dt_summary <- calculate_freq(dt, group_by)
  
  if (!is.null(question_texts)) {
    dt_summary <- rename_items(dt_summary, columns, question_texts)
  }
  
  dt_summary <- sort_items(dt_summary)
  
  plot <- create_plot(dt_summary, show_percent_labels, custom_palette, group_by)
  
  if (use_plotly) {
    plot <- ggplotly(plot)
  }
  
  return(plot)
}

# Helper functions
convert_to_dt <- function(data) {
  as.data.table(data)
}

select_columns <- function(dt, columns) {
  dt[, ..columns]
}

pivot_longer <- function(dt, columns) {
  melt(dt, measure.vars = columns, variable.name = "item", value.name = "response")
}

omit_na <- function(dt) {
  dt[!is.na(response)]
}

convert_to_factor <- function(dt, labels) {
  dt[, response := factor(response, levels = 1:length(labels), labels = labels, ordered = TRUE)]
}

group_responses <- function(dt, group_labels) {
  dt[, response := as.character(response)]
  for (original in names(group_labels)) {
    dt[response == original, response := group_labels[original]]
  }
  dt[, response := factor(response, levels = unique(unlist(group_labels)), ordered = TRUE)]
}

add_group_var <- function(dt, data, group_by, columns) {
  group_data <- data[[group_by]]
  dt[, group := rep(group_data, each = length(columns))]
  dt[, group := factor(group)]
}

calculate_freq <- function(dt, group_by) {
  if (!is.null(group_by)) {
    dt_summary <- dt[, .N, by = .(item, response, group)]
    dt_summary[, freq := N / sum(N) * 100, by = .(item, group)]
  } else {
    dt_summary <- dt[, .N, by = .(item, response)]
    dt_summary[, freq := N / sum(N) * 100, by = item]
  }
  dt_summary
}

rename_items <- function(dt, columns, question_texts) {
  dt[, item := factor(item, levels = columns, labels = question_texts)]
}

sort_items <- function(dt) {
  dt[, item := factor(item, levels = rev(levels(item)))]
}

create_plot <- function(dt, show_percent_labels, custom_palette, group_by) {
  plot <- ggplot(dt, aes(x = item, y = freq, fill = response)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    labs(y = "Percentage", x = "Item", fill = "Response") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) +
    coord_flip()
  
  if (!is.null(custom_palette)) {
    plot <- plot + scale_fill_manual(values = custom_palette)
  } else {
    plot <- plot + scale_fill_brewer(palette = "Set2")
  }
  
  if (show_percent_labels) {
    plot <- plot + geom_text(aes(label = paste0(round(freq, 1), "%")), 
                             position = position_stack(vjust = 0.5, reverse = TRUE), 
                             size = 3)
  }
  
  if (!is.null(group_by)) {
    plot <- plot + facet_grid(~ group, scales = "free_x", space = "free")
  }
  
  plot
}
