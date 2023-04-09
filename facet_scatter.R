#' Facet Scatter Plots
#' 
#' This function creates facetted scatter plots using ggplot2. Each panel shows the scatter plot and a line of
#' best fit for a subset of the data. The line of best fit is based on the specified family of models (default is 
#' gaussian), and is computed using the lm() function. For the linear model, red indicates slopw is negative, blue is positive.
#' 
#' @param data A data frame containing the data to be plotted.
#' @param x_var A character string indicating the name of the variable to be plotted on the x-axis.
#' @param y_var A character string indicating the name of the variable to be plotted on the y-axis.
#' @param group_var A character string indicating the name of the variable to be used for grouping the data.
#' @param plots_per_panel An integer indicating the maximum number of plots to display in each panel. Default is 25.
#' @param save_to_pdf A logical indicating whether to save the plots as a PDF in the working directory. Default is FALSE.
#' @param family A character string indicating the type of model to use for computing the line of best fit. 
#' Valid values are "gaussian" (default), "poisson", and "binomial".
#' 
#' @return A facetted plot of scatter plots.
#' 
#' @import ggplot2
#' @import gridExtra
#' @import rlang
#' @import dplyr
#' 
#' @examples 
#' # Example data set
#' set.seed(1234)
#' data <- data.frame(x = rnorm(1000), y = rnorm(1000), group = factor(rep(1:100, each = 10)))
#' 
#' # Run the function with the example data set
#' facet_scatter(data, "x", "y", "group")
#' 
#' # Save plots as PDF in working directory
#' facet_scatter(data, "x", "y", "group", save_to_pdf = TRUE)
#' 
#' # Get working directory
#' getwd()
#' 
#' @export

# Load libraries
library(ggplot2)
library(gridExtra)
library(rlang)
library(dplyr)

# Save function
facet_scatter <- function(data, x_var, y_var, group_var, plots_per_panel = 25, save_to_pdf = FALSE, family = "gaussian") {
  # Error checking
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.character(x_var)) stop("x_var must be a character string")
  if (!is.character(y_var)) stop("y_var must be a character string")
  if (!is.character(group_var)) stop("group_var must be a character string")
  if (!is.numeric(plots_per_panel)) stop("plots_per_panel must be a numeric value")
  if (!is.logical(save_to_pdf)) stop("save_to_pdf must be a logical value")
  if (!is.character(family)) stop("family must be a character string")
  
  data <- data.frame(data)
  x_var_sym <- sym(x_var)
  y_var_sym <- sym(y_var)
  group_var_sym <- sym(group_var)
  
  levels_count <- length(unique(data[[group_var]]))
  panel_count <- ceiling(levels_count / plots_per_panel)
  
  lm_data <- data %>%
    group_by(!!group_var_sym) %>%
    summarize(slope = coef(glm(!!y_var_sym ~ !!x_var_sym, family = family))[2],
              intercept = coef(glm(!!y_var_sym ~ !!x_var_sym, family = family))[1]) %>%
    ungroup()
  
  if (save_to_pdf) {
    pdf("facet_scatter_plots.pdf", width = 14, height = 10)
  }
  
  for (panel_idx in 1:panel_count) {
    start_level <- (panel_idx - 1) * plots_per_panel + 1
    end_level <- min(panel_idx * plots_per_panel, levels_count)
    
    subset_data <- data[data[[group_var]] %in% unique(data[[group_var]])[start_level:end_level], ]
    subset_lm_data <- lm_data[lm_data[[group_var]] %in% unique(data[[group_var]])[start_level:end_level], ]
    
    line_data <- data.frame()
    for (i in seq_along(subset_lm_data[[group_var]])) {
      
      if (family == "poisson") {
        y_values <- exp(subset_lm_data$intercept[i] + subset_lm_data$slope[i] * range(subset_data[[x_var]]))
      } else if (family == "binomial") {
        y_values <- 1 / (1 + exp(-(subset_lm_data$intercept[i] + subset_lm_data$slope[i] * range(subset_data[[x_var]]))))
      } else {
        y_values <- subset_lm_data$intercept[i] + subset_lm_data$slope[i] * range(subset_data[[x_var]])
      }
      
      tmp_df <- data.frame(
        x = range(subset_data[[x_var]]),
        y = y_values,
        group = subset_lm_data[[group_var]][i],
        slope = subset_lm_data$slope[i],
        color = ifelse(subset_lm_data$slope[i] > 0, "upward", "downward"),
        linetype = "dashed",
        row.names = NULL
      )
      line_data <- rbind(line_data, tmp_df)
    }
    
    plot <- ggplot(subset_data, aes(!!x_var_sym, !!y_var_sym, group = !!group_var_sym)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", color = "black", se = FALSE, linetype = "solid") +
      geom_line(data = line_data, aes(x = x, y = y, group = group, color = color, linetype = linetype)) +
      scale_color_manual(values = c("upward" = "blue", "downward" = "red")) +
      scale_linetype_manual(values = c("dashed" = "dashed")) +
      facet_wrap(vars(!!group_var_sym), scales = "free", ncol = 5) +
      theme_bw() +
      labs(title = paste0("Scatter plots for levels ", start_level, " to ", end_level, " of ", group_var),
           x = x_var,
           y = y_var) +
      theme(axis.text = element_text(size = 8),
            axis.title = element_text(size = 10, face = "bold"),
            strip.background = element_rect(fill = "grey85", color = "black", size = 0.5),
            strip.text = element_text(size = 8, face = "bold", color = "black"),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 10)),
            legend.position = "none")
    
    print(plot)
  }
  
  if (save_to_pdf) {
    dev.off()
  }
}