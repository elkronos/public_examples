#' Create a scatter plot with points colored by a categorical variable
#'
#' This function creates a scatter plot with points colored by a categorical variable.
#'
#' @param data A data.frame containing the variables to plot.
#' @param x_var A string specifying the name of the variable to plot on the x-axis.
#' @param y_var A string specifying the name of the variable to plot on the y-axis.
#' @param categorical_var A string specifying the name of the variable to use for coloring the points.
#' @param facet_var A string specifying the name of the variable to use for faceting the plot (optional).
#' @param smooth_method A string specifying the method to use for smoothing the points (default is "lm").
#' @param smooth_se A logical value indicating whether to add standard error bands to the smoothing line (default is FALSE).
#'
#' @return A ggplot object representing the scatter plot.
#'
#' @import ggplot2
#' @import rlang
#' @import dplyr
#' @export
#'
#' @examples
#' 
#' # Scatter mpg by disp color by cyl
#' scatter_group(mtcars, mpg, disp, cyl)
#' 
#' # Scatter mpg by disp color by cyl with facet by gear
#' scatter_group(mtcars, mpg, disp, cyl, gear)

# Load libraries
library(ggplot2)
library(rlang)
library(dplyr)

# Save function
scatter_group <- function(data, x_var, y_var, categorical_var, facet_var = NULL, smooth_method = "lm", smooth_se = FALSE) {
  
  x_var <- enexpr(x_var)
  y_var <- enexpr(y_var)
  categorical_var <- enexpr(categorical_var)
  facet_var <- enexpr(facet_var)
  
  if (!is.null(facet_var)) {
    data <- data %>%
      mutate(across(all_of(c(as_label(x_var), as_label(y_var), as_label(categorical_var), as_label(facet_var))), as.factor, .names = "f_{.col}"))
  } else {
    data <- data %>%
      mutate(across(all_of(c(as_label(x_var), as_label(y_var), as_label(categorical_var))), as.factor, .names = "f_{.col}"))
  }
  
  p <- ggplot(data, aes(x = !!sym(as_label(x_var)), y = !!sym(as_label(y_var)), color = !!sym(paste0("f_", as_label(categorical_var))))) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = smooth_method, se = smooth_se, linetype = "dashed", linewidth = 1) +
    xlab(as_label(x_var)) +
    ylab(as_label(y_var)) +
    ggtitle(paste("Scatter of", as_label(x_var), "vs", as_label(y_var),
                  "colored by", as_label(categorical_var))) +
    theme_minimal() +
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          strip.background = element_rect(fill = "white", colour = "black", linewidth = 1),
          strip.text.x = element_text(face = "bold")) +
    scale_color_brewer(palette = "Set1")
  
  if (!is.null(facet_var)) {
    prefix_labeller <- function(values) {
      variable <- names(values)
      value <- values
      paste0(variable, ": ", value)
    }
    p <- p + facet_grid(as.formula(paste(". ~ ", paste0("f_", as_label(facet_var)))), labeller = labeller(.default = prefix_labeller))
  }
  
  return(p)
}