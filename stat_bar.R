#' Bar plot
#' 
#' This function creates a count bar plot if X is specified and Y is null. If Y has a variable, the user must specify what statistic they want for Y by every level of X.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x A character string representing the name of the categorical variable.
#' @param y An optional character string representing the name of the continuous variable.
#' @param stat_type An optional character string specifying the summary statistic ("sum", "mean", "median", "min", "max"). 
#'        Default is NULL, which will count the number of observations in each group.
#' @param show_labels A logical value indicating whether to display data labels on the bars. Default is TRUE.
#' @param x_labels An optional character string for the x-axis label. Default is NULL, which will use the variable name.
#' @param y_labels An optional character string for the y-axis label. Default is NULL, which will use the stat_type.
#' @param color_palette An optional character vector representing a custom color palette. Default is NULL, which will use the viridis color scale.
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual aes theme_minimal element_text theme
#' @importFrom dplyr group_by summarize
#' @importFrom viridis viridis
#' @importFrom tools toTitleCase
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(diamonds)
#' 
#' # Count bar plot
#' stat_bar(diamonds, "cut")
#' 
#' # Mean price by cut
#' stat_bar(diamonds, "cut", "price", "mean")
#' 
#' # Median price by cut without labels above bars
#' stat_bar(diamonds, "cut", "price", "median", show_labels = F)
#' 
#' # Hide title
#' stat_bar(diamonds, "cut", "price", "median", show_title = F)
#' 
#' }
#' @export
# Load packages
library(ggplot2)
library(dplyr)
library(viridis)

# Save function
stat_bar <- function(data, x, y = NULL, stat_type = NULL, show_labels = TRUE, x_labels = NULL, y_labels = NULL, show_title = TRUE) {
  stat_functions <- c("sum", "mean", "median", "min", "max")
  
  if (!is.null(stat_type) && !stat_type %in% stat_functions) {
    stop("Invalid stat_type. Choose one of the following: sum, mean, median, min, max.")
  }
  
  to_title_case <- function(string) {
    tools::toTitleCase(gsub("_", " ", string))
  }
  
  data[[x]] <- factor(data[[x]], labels = sapply(levels(data[[x]]), to_title_case))
  
  plot <- ggplot()
  
  if (is.null(y) && is.null(stat_type)) {
    plot <- plot + 
      geom_bar(aes(x = data[[x]], fill = data[[x]])) +
      scale_fill_viridis(discrete = TRUE)
    
    if (show_labels) {
      plot <- plot +
        geom_text(stat = 'count', aes(label = after_stat(count), x = data[[x]]), vjust = -1)
    }
    
  } else {
    data_stat <- data %>% 
      group_by(!!sym(x)) %>% 
      summarize(y := ifelse(is.null(stat_type), length(!!sym(y)), get(ifelse(is.character(stat_type), stat_type, "sum"))(!!sym(y))))
    
    if (!is.null(stat_type) && is.numeric(data_stat$y)) {
      data_stat$y <- round(data_stat$y, 2)
    }
    
    plot <- plot +
      geom_bar(data = data_stat, aes(!!sym(x), y, fill = !!sym(x)), position = "dodge", stat = "summary", fun = ifelse(is.null(stat_type), "count", ifelse(is.character(stat_type), stat_type, "sum"))) +
      scale_fill_viridis(discrete = TRUE)
    
    if (show_labels) {
      plot <- plot +
        geom_text(data = data_stat, aes(label = y, x = !!sym(x), y = y), position = position_dodge(width = 0.9), vjust = -0.5)
    }
  }
  
  if (show_title) {
    if (is.null(y)) {
      title <- "Count by " %>% paste0(to_title_case(x))
    } else {
      title <- to_title_case(stat_type) %>% paste0(" of ", to_title_case(y), " by ", to_title_case(x))
    }
    plot <- plot + ggtitle(title)
  }
  
  plot <- plot +
    xlab(ifelse(is.null(x_labels), to_title_case(x), x_labels)) +
    ylab(ifelse(is.null(y_labels), ifelse(is.null(stat_type), "Count", to_title_case(stat_type)), y_labels)) +
    theme_minimal() +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.title = element_blank(),
          legend.position = "right")
  
  return(plot)
}