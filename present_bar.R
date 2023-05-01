#' Create bar plots for combinations of categorical and numeric variables
#'
#' This function generates bar plots for each combination of categorical and numeric variables
#' in a dataset, using a specified statistic. It creates a PowerPoint presentation with each plot
#' on a separate slide. The user can customize the colors and other aspects of the plots.
#'
#' @param data A data.frame containing the dataset to create bar plots from.
#' @param statistic A character string specifying the statistic to use for the bar plots
#'   ("mean", "median", "min", "max", or "sum"). Default is "mean".
#' @param unique_levels An integer specifying the maximum number of unique levels for
#'   a variable to be considered categorical. Default is 10.
#' @param custom_colors A named vector of colors to use for customizing the colors of the bars.
#'   Default is NULL, which uses the hue_pal() color palette.
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_text labs theme_minimal theme element_text
#' @importFrom officer read_pptx add_slide ph_with print
#' @importFrom rvg ph_location_type
#' @importFrom dplyr group_by summarize
#' @importFrom scales hue_pal
#'
#' @examples
#' # Create bar plots with default settings
#' present_bar(diamonds, statistic = "mean", unique_levels = 10)
#'
#' # Create bar plots with custom colors
#' custom_colors <- c("Ideal" = "red", "Premium" = "blue", "Good" = "green",
#'                    "Very Good" = "yellow", "Fair" = "purple")
#' present_bar(diamonds, statistic = "mean", unique_levels = 10, custom_colors = custom_colors)
#'
#' @return A PowerPoint presentation saved as "bar_plots.pptx" in the working directory.
#' @export
# Load packages
library(ggplot2)
library(officer)
library(rvg)
library(dplyr)
library(scales)
# Save function
present_bar <- function(data, statistic = "mean", unique_levels = 10, custom_colors = NULL) {
  # Identify variables with the specified number of unique levels or fewer
  categorical_vars <- sapply(data, function(x) length(unique(x)) <= unique_levels)
  categorical_vars <- names(categorical_vars[categorical_vars])
  
  # Identify numeric variables
  numeric_vars <- sapply(data, is.numeric)
  numeric_vars <- names(numeric_vars[numeric_vars])
  
  # Define the statistic function
  stat_func <- switch(statistic,
                      mean = mean,
                      median = median,
                      min = min,
                      max = max,
                      sum = sum,
                      stop("Invalid statistic. Choose from mean, median, min, max, or sum."))
  
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
  
  # Loop through the selected categorical and numeric variables
  for (cat_var in categorical_vars) {
    for (num_var in numeric_vars) {
      # Generate a bar plot for the current variable
      plot_data <- data %>%
        group_by(!!sym(cat_var)) %>%
        summarize(value = stat_func(!!sym(num_var), na.rm = TRUE))
      
      plot <- ggplot(plot_data, aes_string(x = cat_var, y = "value", fill = cat_var)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = round(value, 1)), vjust = -0.5) +
        labs(title = paste(statistic, "of", num_var, "by", cat_var),
             x = cat_var, y = num_var) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        color_palette
      
      # Add a slide to the PowerPoint object and include the plot
      pptx <- pptx %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = plot, location = ph_location_type(type = "body"))
    }
  }
  
  # Save the PowerPoint object to a file
  print(pptx, target = "bar_plots.pptx")
}


create_bar_plots(diamonds, statistic = "mean", unique_levels = 10)
