library(tidyverse)
library(viridis)

#' Plot Summary Statistics
#'
#' This function takes a data frame, a vector of column names, a facet variable (optional), 
#' and a summary function (default is mean), then creates a bar plot of the summary statistics
#' for each variable. When a summary function of `mean` is used, error bars representing 
#' standard deviation are included in the plot.
#'
#' @param df A data frame containing the data to be visualized.
#' @param cols A character vector of column names in the `df` data frame to be used in the plot.
#' @param facet_var An optional character string specifying a column in `df` to be used for faceting. 
#' If NULL (default), no faceting is done.
#' @param summary_func A function to summarize the data. Defaults to `mean`. If `mean` is used, 
#' standard deviation error bars are added to the plot.
#'
#' @return A ggplot object of the bar plot of the summary statistics for each variable in `cols`.
#'
#' @importFrom tidyverse ggplot aes geom_bar labs theme_minimal theme element_text select 
#' all_of gather group_by summarise
#' @importFrom viridis scale_fill_viridis
#'
#' @examples 
#' \dontrun{
#' set.seed(42)
#' df <- data.frame(
#'  Var1 = rnorm(300, mean = rep(c(5, 10, 15), each = 100), sd = 2),
#'  Var2 = rnorm(300, mean = rep(c(10, 15, 20), each = 100), sd = 3),
#'  Group = rep(c("Group1", "Group2", "Group3"), each = 100)
#' )
#' cols <- c("Var1", "Var2")
#' # With facets, using sum as the summary function
#' plot_stats(df, cols, "Group", sum)
#' # Without facets, using median as the summary function
#' plot_stats(df, cols, NULL, median)
#' # Without facets, using min as the summary function
#' plot_stats(df, cols, NULL, min)
#' # Means with facets
#' plot_stats(df, cols, "Group")
#' # Max with facets
#' plot_stats(df, cols, "Group", max)
#' }
#' @export
plot_stats <- function(df, cols, facet_var = NULL, summary_func = mean) {
  # Gather the data into long format
  if (!is.null(facet_var)) {
    long_df <- df %>%
      select(all_of(c(cols, facet_var))) %>%
      gather(key = "variable", value = "value", -!!sym(facet_var))
  } else {
    long_df <- df %>%
      select(all_of(cols)) %>%
      gather(key = "variable", value = "value")
  }
  
  # Calculate summaries
  if (!is.null(facet_var)) {
    summary_df <- long_df %>%
      group_by(variable, !!sym(facet_var)) %>%
      summarise(summary = summary_func(value, na.rm = TRUE), 
                .groups = "drop")
  } else {
    summary_df <- long_df %>%
      group_by(variable) %>%
      summarise(summary = summary_func(value, na.rm = TRUE), 
                .groups = "drop")
  }
  
  # Create plot
  plot <- ggplot(summary_df, aes(x = variable, y = summary, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
    scale_fill_viridis_d(option = "viridis") +
    labs(x = "Variable", y = "Summary Value") +
    theme_bw() +
    theme(text = element_text(size = 14), 
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # Add labels if the summary function is mean
  if (is.function(mean) && identical(summary_func, mean)) {
    plot <- plot +
      geom_text(aes(y = summary, label = round(summary, 2)), position = position_dodge(0.9), vjust = -1.5, color = "black")
  } else {
    plot <- plot +
      geom_text(aes(label = round(summary, 2)), position = position_dodge(0.9), vjust = -1.5, color = "black")
  }
  
  # Add facets if facet_var is specified
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(as.formula(paste("~", facet_var)), scales = "free")
  }
  
  # Adjust y limits to ensure text is visible
  plot <- plot + coord_cartesian(ylim = c(NA, max(summary_df$summary) * 1.2))
  
  return(plot)
}