#' Calculate Percentages and Generate Bar Plots
#'
#' This script contains functions to calculate percentages of levels in a column of a data frame,
#' and to generate bar plots displaying these percentages. The main functions include:
#' `calculate_percentages`, `create_main_bar_plot`, `create_na_bar_plot`, and `generate_percentage_bar_plot`.
#'
#' @name PercentageBarPlot
#' @import data.table ggplot2 patchwork

#' Calculate Percentages of Levels in a Column
#'
#' This function calculates the percentages of each level in a specified column of the data frame.
#'
#' @param data A data frame containing the data.
#' @param column The name of the column to calculate percentages for.
#' @param include_nulls Logical, whether to include NA values in the percentage calculations. Default is FALSE.
#' @return A data table with levels, counts, and percentages.
#' @examples
#' \dontrun{
#' data <- data.frame(Category = sample(c(rep(LETTERS[1:5], times = c(50, 30, 10, 5, 5)), NA), 100, replace = TRUE))
#' calculate_percentages(data, "Category", include_nulls = TRUE)
#' }
calculate_percentages <- function(data, column, include_nulls = FALSE) {
  dt <- data.table(data)
  
  if (include_nulls) {
    table_data <- dt[, .N, by = .(Level = get(column))]
    na_count <- sum(is.na(dt[[column]]))
    if (na_count > 0) {
      table_data <- rbind(table_data, data.table(Level = "NA's", N = na_count))
    }
    total_count <- sum(table_data$N)
  } else {
    table_data <- dt[!is.na(get(column)), .N, by = .(Level = get(column))]
    total_count <- sum(table_data$N)
  }
  
  table_data[, Percentage := (N / total_count) * 100]
  table_data[is.na(Level), Level := "NA's"]
  
  table_data <- table_data[order(-Percentage)]
  if ("NA's" %in% table_data$Level) {
    na_row <- table_data[Level == "NA's"]
    table_data <- table_data[Level != "NA's"]
    table_data <- rbind(table_data, na_row)
  }
  
  return(table_data)
}

#' Create Main Bar Plot
#'
#' This function creates a bar plot for the main levels in the percentages data frame.
#'
#' @param percentages_df A data table with levels, counts, and percentages.
#' @param title The title of the plot.
#' @param label Logical, whether to add percentage labels to the bars. Default is FALSE.
#' @return A ggplot object representing the bar plot.
#' @examples
#' \dontrun{
#' percentages_df <- calculate_percentages(data, "Category", include_nulls = TRUE)
#' create_main_bar_plot(percentages_df, "Title", label = TRUE)
#' }
create_main_bar_plot <- function(percentages_df, title, label = FALSE) {
  main_data <- percentages_df[Level != "NA's"]
  
  plot <- ggplot(main_data, aes(x = reorder(Level, -Percentage), y = Percentage)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = title, x = NULL, y = "Percentage") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.text.y = element_text(size = 12))
  
  if (label) {
    plot <- plot + geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                             position = position_stack(vjust = 0.5), 
                             color = "black", size = 4)
  }
  
  return(plot)
}

#' Create NA Bar Plot
#'
#' This function creates a bar plot for the NA levels in the percentages data frame.
#'
#' @param percentages_df A data table with levels, counts, and percentages.
#' @param label Logical, whether to add percentage labels to the bars. Default is FALSE.
#' @return A ggplot object representing the bar plot.
#' @examples
#' \dontrun{
#' percentages_df <- calculate_percentages(data, "Category", include_nulls = TRUE)
#' create_na_bar_plot(percentages_df, label = TRUE)
#' }
create_na_bar_plot <- function(percentages_df, label = FALSE) {
  na_data <- percentages_df[Level == "NA's"]
  
  plot <- ggplot(na_data, aes(x = Level, y = Percentage)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.margin = margin(t = -10, unit = "pt"))
  
  if (label) {
    plot <- plot + geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                             vjust = 0.5, 
                             hjust = -0.1, 
                             color = "black", size = 4)
  }
  
  return(plot)
}

#' Generate Percentage Bar Plot
#'
#' This function generates the percentage bar plot by combining the main and NA bar plots if applicable.
#'
#' @param data A data frame containing the data.
#' @param column The name of the column to calculate percentages for.
#' @param include_nulls Logical, whether to include NA values in the percentage calculations. Default is FALSE.
#' @param label Logical, whether to add percentage labels to the bars. Default is FALSE.
#' @examples
#' \dontrun{
#' data <- data.frame(Category = sample(c(rep(LETTERS[1:5], times = c(50, 30, 10, 5, 5)), NA), 100, replace = TRUE))
#' generate_percentage_bar_plot(data, "Category", include_nulls = TRUE, label = TRUE)
#' generate_percentage_bar_plot(data, "Category", include_nulls = FALSE, label = TRUE)
#' }
generate_percentage_bar_plot <- function(data, column, include_nulls = FALSE, label = FALSE) {
  percentages_df <- calculate_percentages(data, column, include_nulls)
  plot_title <- paste("Percentage of Levels in", column)
  main_plot <- create_main_bar_plot(percentages_df, plot_title, label)
  
  if (include_nulls) {
    na_plot <- create_na_bar_plot(percentages_df, label)
    combined_plot <- main_plot / na_plot + plot_layout(heights = c(4, 1))
  } else {
    combined_plot <- main_plot
  }
  
  print(combined_plot)
}
