#' Create Bar Plots for PowerPoint
#'
#' This function generates bar plots of numeric variables grouped by categorical variables
#' and exports them to a PowerPoint file.
#'
#' @param data A data frame containing the data to be plotted.
#' @param statistic A character string specifying the statistic to be plotted.
#'   Options: "all", "mean", "median", "min", "max", "sum", "count". Default: "all".
#' @param unique_levels An integer specifying the maximum number of unique levels a
#'   variable can have to be considered categorical. Default: 10.
#' @param custom_colors A vector of custom colors to use in the plots. Default: NULL.
#' @param file_name A character string specifying the base name of the output PowerPoint file.
#'   Default: "bar_plots".
#' @param custom_theme A ggplot2 theme to apply to the plots. Default: theme_minimal().
#' @param x_axis_label A character string specifying the label for the x-axis. Default: NULL.
#' @param y_axis_label A character string specifying the label for the y-axis. Default: NULL.
#' @param round_digits An integer specifying the number of decimal places to round the values.
#'   Default: 1.
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_text labs theme_minimal element_text
#' @importFrom officer read_pptx add_slide ph_with print
#' @importFrom rvg ph_location_type
#' @importFrom dplyr group_by summarize
#' @importFrom scales hue_pal scale_fill_manual
#' @importFrom tools toTitleCase
#'
#' @examples
#' \dontrun{
#'   present_bar(diamonds, statistic = "all", unique_levels = 10)
#' }
#'
# Load packages
library(ggplot2)
library(officer)
library(rvg)
library(dplyr)
library(scales)
library(tools)
# Save function
present_bar <- function(data,
                        statistic = "all",
                        unique_levels = 10,
                        custom_colors = NULL,
                        file_name = "bar_plots",
                        custom_theme = theme_minimal(),
                        x_axis_label = NULL,
                        y_axis_label = NULL,
                        round_digits = 1) {
  
  # Validate the input statistic
  valid_statistics <- c("all", "mean", "median", "min", "max", "sum", "count")
  if (!statistic %in% valid_statistics) {
    stop("Invalid statistic. Choose from 'all', 'mean', 'median', 'min', 'max', 'sum', or 'count'.")
  }
  
  if (statistic == "all") {
    statistics <- valid_statistics[-1]  # Remove "all" from the list
  } else {
    statistics <- statistic
  }
  
  # Identify variables with the specified number of unique levels or fewer
  categorical_vars <- sapply(data, function(x) length(unique(x)) <= unique_levels)
  categorical_vars <- names(categorical_vars[categorical_vars])
  
  # Identify numeric variables
  numeric_vars <- sapply(data, is.numeric)
  numeric_vars <- names(numeric_vars[numeric_vars])
  
  # Set color palette
  max_unique_levels <- max(sapply(data[categorical_vars], function(x) length(unique(x))))
  if (is.null(custom_colors)) {
    color_func <- hue_pal()(max_unique_levels)
    color_palette <- scale_fill_manual(values = color_func)
  } else {
    color_palette <- scale_fill_manual(values = custom_colors)
  }
  
  # Create a PowerPoint object
  pptx <- read_pptx()
  
  # Loop through the selected statistics
  for (stat in statistics) {
    
    # Loop through the selected categorical and numeric variables
    for (cat_var in categorical_vars) {
      for (num_var in numeric_vars) {
        # Generate a bar plot for the current variable
        if (stat != "count") {
          stat_func <- switch(stat,
                              mean = mean,
                              median = median,
                              min = min,
                              max = max,
                              sum = sum)
          plot_data <- data %>%
            group_by(!!sym(cat_var)) %>%
            summarize(value = stat_func(!!sym(num_var), na.rm = TRUE),
                      .groups = "drop")
        } else {
          plot_data <- data %>%
            group_by(!!sym(cat_var)) %>%
            summarize(value = n(),
                      .groups = "drop")
        }
        
        plot <- ggplot(plot_data, aes_string(x = cat_var, y = "value", fill = cat_var)) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          geom_text(aes(label = round(value, round_digits)), vjust = -0.5) +
          labs(title = toTitleCase(paste(stat, "of", num_var, "by", cat_var)),
               x = ifelse(is.null(x_axis_label), cat_var, x_axis_label),
               y = ifelse(is.null(y_axis_label), num_var, y_axis_label)) +
          custom_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          color_palette
        
        # Add a slide to the PowerPoint object and include the plot
        pptx <- pptx %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = plot, location = ph_location_type(type = "body"))
      }
    }
  }
  
  # Add the system date and time to the end of the file name
  current_datetime <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- paste0(file_name, "_", current_datetime, ".pptx")
  
  # Save the PowerPoint object to a file
  print(pptx, target = file_name)
}