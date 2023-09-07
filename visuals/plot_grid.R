# Load packages
library(ggplot2)
library(dplyr)

#' Create a Grid Plot
#'
#' This function generates a grid plot for categorical data, where each grid cell represents the percentage of a specific category in a facet. It is particularly useful for visualizing binary categorical data.
#'
#' @param data A data frame containing the categorical data.
#' @param column_name The name of the column in \code{data} to be visualized. It should contain binary categorical values (0s and 1s).
#' @param label Logical, indicating whether to display labels in the grid cells.
#' @param colors A vector of two colors representing the fill colors for 0 and 1 values, respectively.
#' @param facet_var The name of the column in \code{data} to be used for faceting the grid plot. If \code{NULL}, a default "facet_var" column will be created with all data in one facet.
#' @param facet_order A character vector specifying the order of facets. If provided, it determines the order of facets. If \code{NULL}, facets are sorted alphabetically, or numerically if the variable is not a factor.
#'
#' @return A ggplot2 object representing the grid plot.
#'
#' @import ggplot2
#' @importFrom dplyr colnames is.null stop nrow expand.grid round min factor as_labeller
#' @importFrom stats unique sort
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(ggplot2)
#' library(dplyr)
#'
#' # Generate sample data
#' set.seed(123)
#' data <- data.frame(
#'   percent_male = sample(c(0, 1), 400, replace = TRUE),
#'   region = rep(c("North", "East", "West", "South"), each = 100)
#' )
#'
#' # Non-faceted version
#' plot_grid(data, "percent_male", label = TRUE)
#'
#' # Faceted version with custom order
#' plot_grid(data, "percent_male", label = TRUE, facet_var = "region", facet_order = c("North", "South", "East", "West"))
#' }
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}
#'
#' @export
plot_grid <- function(data, column_name, label = FALSE, colors = c("black", "gray"), facet_var = NULL, facet_order = NULL) {
  if(!column_name %in% colnames(data)) {
    stop("The specified column name is not present in the data frame")
  }
  
  if(!is.null(facet_var) && !facet_var %in% colnames(data)) {
    stop("The specified facet variable is not present in the data frame")
  }
  
  list_of_grids <- list()
  
  if(is.null(facet_var)) {
    facet_var <- "facet_var"
    data[[facet_var]] <- "All Data"
  }
  
  facet_levels <- unique(data[[facet_var]])
  
  if(!is.null(facet_order)) {
    if(!all(facet_order %in% facet_levels)) {
      stop("The specified facet order contains levels not found in the facet variable")
    }
    facet_levels <- facet_order
  } else if(!is.factor(data[[facet_var]])) {
    facet_levels <- sort(facet_levels)
  }
  
  for(facet_level in facet_levels) {
    subset_data <- data[data[[facet_var]] == facet_level,]
    perc <- round(sum(subset_data[[column_name]] == 1) / nrow(subset_data) * 100)
    grid_data <- expand.grid(x = 1:10, y = 1:10)
    filled_dots <- min(100, max(0, round(perc)))
    
    grid_data$filled <- 0
    if(filled_dots > 0) {
      grid_data$filled[1:filled_dots] <- 1
    }
    grid_data$facet_var <- facet_level
    grid_data$label <- paste0(facet_level, " - ", column_name, ": ", perc, "%")
    
    list_of_grids[[as.character(facet_level)]] <- grid_data
  }
  
  grid_data <- do.call(rbind, list_of_grids)
  grid_data$facet_var <- factor(grid_data$facet_var, levels = facet_levels)
  
  p <- ggplot(grid_data, aes(x = x, y = y)) +
    geom_point(aes(color = as.factor(filled)), size = 5, shape = 16) +
    scale_x_continuous(expand = c(0, 0), breaks = 1:10, limits = c(0.5, 10.5)) +
    scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = seq(0, 90, by = 10), limits = c(0.5, 10.5)) +
    scale_color_manual(values = c("0" = colors[2], "1" = colors[1])) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    facet_wrap(~ facet_var, labeller = as_labeller(setNames(grid_data$label, grid_data$facet_var))) 
  
  if(label) {
    p <- p + theme(strip.text = element_text(size = 14, face = "bold"))
  } else {
    p <- p + theme(strip.text = element_blank())
  }
  
  if(!is.null(facet_var) && length(facet_levels) > 1) {
    p <- p + theme(panel.border = element_rect(colour = "gray", fill = NA, linewidth = 1))
  }
  
  return(p)
}