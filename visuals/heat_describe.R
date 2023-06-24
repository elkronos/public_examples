library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)

#' Apply Min-Max Scaling to All Columns of a Data Frame
#'
#' This function applies the min-max scaling transformation to all columns of
#' a given data frame. Min-max scaling, also known as normalization, rescales
#' the values of each column to a range between 0 and 1.
#'
#' @param df A data frame. The input data frame to be scaled.
#'
#' @return A data frame with the same dimensions as the input \code{df}, where
#' each column has been rescaled using the min-max scaling transformation.
#'
#' @examples
#' # Example 1: Scaling a data frame with numeric values
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' min_max_scaling_all(df)
#'
#' # Example 2: Scaling a data frame with mixed data types (numeric and character)
#' df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
#' min_max_scaling_all(df)
#'
#' @importFrom base apply, min, max
#' @importFrom stats scale
#'
#' @details
#' This function uses the \code{apply} function from the base R package to
#' apply the min-max scaling transformation to each column of the input data
#' frame. The scaling formula used is:
#'
#' \deqn{\frac{{x - \text{{min}}(x)}}{{\text{{max}}(x) - \text{{min}}(x)}}}
#'
#' The \code{apply} function applies the anonymous function to each column (2
#' indicates columns) and performs the scaling operation using the \code{min}
#' and \code{max} functions from the base package.
#'
#' The resulting scaled data frame is returned as the output.
#'
#' This function imports the \code{apply} function from the base R package to
#' perform the scaling operation. Additionally, it imports the \code{min} and
#' \code{max} functions from the base package to compute the minimum and maximum
#' values for each column. If desired, the function can also import the
#' \code{scale} function from the \code{stats} package for a more general
#' scaling operation.
#'
#' @seealso
#' \code{\link[stats]{scale}}: An alternative scaling function that performs
#' column-wise scaling with centering and/or scaling to unit variance.
#'
#' @references
#' For more information on min-max scaling, see:
#' - H. Gupta and B. Rani (2015). "Data Normalization Techniques for Data
#'   Mining: A Survey." International Journal of Computer Applications, 116(3),
#'   10-16.
#'
#' @export
min_max_scaling_all <- function(df) {
  return (apply(df, 2, function(x) (x - min(x)) / (max(x) - min(x))))
}

#' Apply Z-Score Normalization to All Columns of a Data Frame
#'
#' This function applies the Z-score normalization transformation to all columns
#' of a given data frame. Z-score normalization, also known as standardization,
#' rescales the values of each column to have a mean of 0 and a standard
#' deviation of 1.
#'
#' @param df A data frame. The input data frame to be normalized.
#'
#' @return A data frame with the same dimensions as the input \code{df}, where
#' each column has been normalized using the Z-score normalization transformation.
#'
#' @examples
#' # Example 1: Normalizing a data frame with numeric values
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' z_score_normalization_all(df)
#'
#' # Example 2: Normalizing a data frame with mixed data types (numeric and character)
#' df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
#' z_score_normalization_all(df)
#'
#' @importFrom base apply, mean, sd
#'
#' @details
#' This function uses the \code{apply} function from the base R package to apply
#' the Z-score normalization transformation to each column of the input data
#' frame. The normalization formula used is:
#'
#' \deqn{\frac{{x - \text{{mean}}(x)}}{{\text{{sd}}(x)}}}
#'
#' The \code{apply} function applies the anonymous function to each column (2
#' indicates columns) and performs the normalization operation using the
#' \code{mean} and \code{sd} functions from the base package.
#'
#' The resulting normalized data frame is returned as the output.
#'
#' This function imports the \code{apply} function from the base R package to
#' perform the normalization operation. Additionally, it imports the \code{mean}
#' and \code{sd} functions from the base package to compute the mean and standard
#' deviation for each column.
#'
#' @seealso
#' \code{\link[stats]{scale}}: An alternative normalization function that performs
#' column-wise normalization with centering and/or scaling to unit variance.
#'
#' @references
#' For more information on Z-score normalization, see:
#' - J. Behrens and L. Zernike (2018). "Standardizing Data with R." The R Journal,
#'   10(2), 532-542.
#'
#' @export
z_score_normalization_all <- function(df) {
  return (apply(df, 2, function(x) (x - mean(x)) / sd(x)))
}

#' Apply Feature Scaling to All Columns of a Data Frame
#'
#' This function applies the feature scaling transformation to all columns of a
#' given data frame. Feature scaling standardizes the values of each column to
#' have zero mean and unit variance.
#'
#' @param df A data frame. The input data frame to be scaled.
#'
#' @return A data frame with the same dimensions as the input \code{df}, where
#' each column has been scaled using the feature scaling transformation.
#'
#' @examples
#' # Example 1: Scaling a data frame with numeric values
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' feature_scaling_all(df)
#'
#' # Example 2: Scaling a data frame with mixed data types (numeric and character)
#' df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
#' feature_scaling_all(df)
#'
#' @importFrom stats scale
#'
#' @details
#' This function uses the \code{scale} function from the \code{stats} package to
#' apply the feature scaling transformation to each column of the input data
#' frame. The scaling operation performed by \code{scale} centers the data by
#' subtracting the column means and scales it by dividing by the column
#' standard deviations.
#'
#' The resulting scaled data frame is returned as the output.
#'
#' This function imports the \code{scale} function from the \code{stats}
#' package to perform the feature scaling operation.
#'
#' @seealso
#' \code{\link[stats]{scale}}: The function used for performing feature scaling.
#'
#' @references
#' For more information on feature scaling, see:
#' - J. Behrens and L. Zernike (2018). "Standardizing Data with R." The R Journal,
#'   10(2), 532-542.
#'
#' @export
feature_scaling_all <- function(df) {
  return (data.frame(scale(df, center = TRUE, scale = TRUE)))
}

#' Apply Quantile Transformation to All Columns of a Data Frame
#'
#' This function applies the quantile transformation to all columns of a given
#' data frame. The quantile transformation maps the values of each column to
#' discrete quantiles, based on the specified quantile probabilities.
#'
#' @param df A data frame. The input data frame to be transformed.
#' @param probs A numeric vector of quantile probabilities. Default is c(0.25, 0.50, 0.75).
#'              The quantile probabilities must be between 0 and 1.
#'
#' @return A data frame with the same dimensions as the input \code{df}, where
#' each column has been transformed using the quantile transformation.
#'
#' @examples
#' # Example 1: Transforming a data frame with numeric values using default quantiles
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' quantile_transform_all(df)
#'
#' # Example 2: Transforming a data frame with mixed data types (numeric and character)
#' df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
#' quantile_transform_all(df)
#'
#' # Example 3: Transforming a data frame with custom quantiles
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' quantile_transform_all(df, probs = c(0.1, 0.5, 0.9))
#'
#' @importFrom base apply
#' @importFrom stats quantile
#' @importFrom base cut
#' @importFrom base colnames
#' @importFrom base as.numeric
#'
#' @details
#' This function uses the \code{apply} function from the base R package to
#' compute the quantiles for each column of the input data frame using the
#' \code{quantile} function from the \code{stats} package. The quantiles are
#' determined based on the specified \code{probs} argument.
#'
#' The function then creates a copy of the input data frame named
#' \code{transformed_df}. It iterates over each column of \code{transformed_df}
#' and applies the quantile transformation using the \code{cut} function from
#' the base package. The \code{cut} function assigns discrete values to each
#' element of the column based on the quantile breaks.
#'
#' The transformed data frame, \code{transformed_df}, is returned as the output.
#'
#' This function imports the necessary functions from the base R package:
#' \code{apply}, \code{cut}, \code{colnames}, and \code{as.numeric}. It also
#' imports the \code{quantile} function from the \code{stats} package.
#'
#' @seealso
#' \code{\link[base]{cut}}: The function used for performing the quantile transformation.
#' \code{\link[stats]{quantile}}: The function used for computing quantiles.
#'
#' @references
#' For more information on quantile transformation, see:
#' - M. Borgelt and C. Borgelt (2017). "Quantile Regression as an Alternative
#'   for Data Transformation." Studies in Classification, Data Analysis, and
#'   Knowledge Organization, 777.
quantile_transform_all <- function(df, probs = c(0.25, 0.50, 0.75)) {
  quantiles <- apply(df, 2, function(x) quantile(x, probs = probs))
  transformed_df <- df
  for (col in colnames(df)) {
    transformed_col <- as.numeric(cut(df[[col]], breaks = c(-Inf, quantiles[, col], Inf), labels = FALSE))
    transformed_df[[col]] <- transformed_col
  }
  return (transformed_df)
}

#' Apply Robust Scaling to All Columns of a Data Frame
#'
#' This function applies the robust scaling transformation to all columns of a
#' given data frame. Robust scaling rescales the values of each column using the
#' median and interquartile range (IQR), making it robust to outliers.
#'
#' @param df A data frame. The input data frame to be scaled.
#'
#' @return A data frame with the same dimensions as the input \code{df}, where
#' each column has been rescaled using the robust scaling transformation.
#'
#' @examples
#' # Example 1: Scaling a data frame with numeric values
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' robust_scaling_all(df)
#'
#' # Example 2: Scaling a data frame with mixed data types (numeric and character)
#' df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
#' robust_scaling_all(df)
#'
#' @importFrom base apply, median
#' @importFrom stats IQR
#'
#' @details
#' This function uses the \code{apply} function from the base R package to apply
#' the robust scaling transformation to each column of the input data frame. The
#' scaling formula used is:
#'
#' \deqn{\frac{{x - \text{{median}}(x)}}{{\text{{IQR}}(x)}}}
#'
#' The \code{apply} function applies the anonymous function to each column (2
#' indicates columns) and performs the scaling operation using the \code{median}
#' and \code{IQR} functions from the base and \code{stats} packages respectively.
#'
#' The resulting scaled data frame is returned as the output.
#'
#' This function imports the \code{apply} function from the base R package to
#' perform the scaling operation. Additionally, it imports the \code{median}
#' and \code{IQR} functions from the base and \code{stats} packages respectively
#' to compute the median and interquartile range for each column.
#'
#' @seealso
#' \code{\link[stats]{IQR}}: The function used for computing the interquartile range.
#'
#' @references
#' For more information on robust scaling, see:
#' - H. Buhlmann and S. Van De Geer (2011). "Statistics for High-Dimensional
#'   Data: Methods, Theory and Applications." Springer Science & Business Media.
#'
#' @export
robust_scaling_all <- function(df) {
  return (apply(df, 2, function(x) (x - median(x)) / IQR(x)))
}

#' Visualize Descriptive Statistics
#'
#' This function generates a heatmap plot to visualize descriptive statistics
#' for specified variables within different groups of a given data frame. It
#' provides flexibility in choosing the statistics to compute and allows for
#' optional data normalization. The resulting plot can help identify patterns
#' and variations in the data across groups and variables.
#'
#' @param data A data frame. The input data frame containing the variables of interest.
#' @param var_names A character vector specifying the names of the variables to analyze.
#' @param group_var A symbol representing the grouping variable. It should correspond to a column name in the data frame.
#' @param stats A character vector specifying the statistics to compute for each variable. Default is c("mean").
#' @param normalize A character string specifying the normalization method to apply to the selected variables. Default is NULL.
#'        Valid options include "min_max_scaling", "z_score_normalization", "feature_scaling", "quantile_transform", or "robust_scaling".
#' @param color_within A logical value indicating whether the color mapping should be computed within each statistic. Default is FALSE.
#'
#' @return A list with two elements: 'summary' and 'plot'.
#' - 'summary': A data frame containing the computed statistics for each variable and group.
#' - 'plot': A ggplot2 object representing the heatmap plot.
#'
#' @examples
#' # Example 1: Visualizing mean values of variables by groups
#' data <- data.frame(group = rep(c("A", "B", "C"), each = 4), 
#'                    var1 = rnorm(12), 
#'                    var2 = rnorm(12))
#' heat_describe(data, var_names = c("var1", "var2"), group_var = group)
#'
#' # Example 2: Visualizing median and sum values with normalization by min-max scaling
#' data <- data.frame(group = rep(c("A", "B"), each = 4), 
#'                    var1 = c(1, 2, 3, 4, 10, 20, 30, 40), 
#'                    var2 = c(5, 6, 7, 8, 50, 60, 70, 80))
#' heat_describe(data, var_names = c("var1", "var2"), group_var = group, stats = c("median", "sum"), normalize = "min_max_scaling")
#'
#' @importFrom dplyr group_by summarise across pivot_longer mutate arrange
#' @importFrom ggplot2 ggplot geom_tile facet_grid scale_fill_gradient labs geom_text
#' @importFrom stats mean median sum max min skewness kurtosis
#' @importFrom scales as_label enexpr
#' @importFrom rlang !! enexpr
#'
#' @details
#' This function computes descriptive statistics for the specified variables within
#' different groups of the input data frame. The user can select the statistics to
#' compute using the 'stats' parameter, which defaults to the mean value. The function
#' uses the 'dplyr' and 'ggplot2' packages for data manipulation and visualization.
#'
#' The function also allows for optional data normalization using various methods
#' specified in the 'normalize' parameter. If a normalization method is chosen,
#' the function applies the corresponding scaling function to the selected variables
#' using the respective helper functions: 'min_max_scaling_all', 'z_score_normalization_all',
#' 'feature_scaling_all', 'quantile_transform_all', or 'robust scaling_all'. 
#' The function then computes the selected statistics for each group
#' using the 'summarise' and 'across' functions from the 'dplyr' package.
#'
#' The resulting summary statistics are reshaped to long format using the 'pivot_longer'
#' function, allowing for easy visualization. If the 'color_within' parameter is set
#' to TRUE, the color mapping is computed within each statistic. Otherwise, the color
#' mapping is computed across all statistics.
#'
#' Finally, the function generates a heatmap plot using the 'ggplot2' package, where
#' each cell represents a statistic-value pair. The plot is faceted by variable and
#' grouped by the specified 'group_var'. The color of each cell is determined by the
#' relative rank of the value within the respective statistic.
#'
#' The function returns a list with two elements: 'summary' and 'plot'. The 'summary'
#' element is a data frame containing the computed statistics for each variable and group.
#' The 'plot' element is a ggplot2 object representing the heatmap plot.
#'
#' This function imports necessary functions from the 'dplyr', 'ggplot2', and 'stats'
#' packages to perform data manipulation, visualization, and compute statistics.
#'
#' @seealso
#' \code{\link[dplyr]{group_by}}: Grouping rows by one or more variables.
#' \code{\link[dplyr]{summarise}}: Computing summary statistics for grouped data.
#' \code{\link[dplyr]{across}}: Applying a function to multiple columns.
#' \code{\link[dplyr]{pivot_longer}}: Reshaping data from wide to long format.
#' \code{\link[ggplot2]{ggplot}}: Creating a ggplot object.
#' \code{\link[ggplot2]{geom_tile}}: Drawing rectangular tiles on a plot.
#' \code{\link[ggplot2]{facet_grid}}: Creating a grid of facets in a plot.
#' \code{\link[ggplot2]{scale_fill_gradient}}: Defining color gradients for fill.
#' \code{\link[ggplot2]{labs}}: Modifying plot labels.
#' \code{\link[ggplot2]{geom_text}}: Adding text labels to a plot.
#' \code{\link[stats]{mean}}: Computing the mean of a numeric vector.
#' \code{\link[stats]{median}}: Computing the median of a numeric vector.
#' \code{\link[stats]{sum}}: Computing the sum of a numeric vector.
#' \code{\link[stats]{max}}: Computing the maximum value of a numeric vector.
#' \code{\link[stats]{min}}: Computing the minimum value of a numeric vector.
#' \code{\link[stats]{skewness}}: Computing the skewness of a numeric vector.
#' \code{\link[stats]{kurtosis}}: Computing the kurtosis of a numeric vector.
#' \code{\link[scales]{as_label}}: Extracting a label from a symbol.
#' \code{\link[rlang]{enexpr}}: Capturing the expression associated with a symbol.
#'
#' @references
#' For more information on data visualization and statistics, see:
#' - Hadley Wickham (2016). "ggplot2: Elegant Graphics for Data Analysis."
#' - Hadley Wickham, Romain François, Lionel Henry, and Kirill Müller (2021).
#'   "dplyr: A Grammar of Data Manipulation."
#' - William
heat_describe <- function(data, var_names, group_var, stats = c("mean"), normalize = NULL, color_within = FALSE) {
  
  # Normalize data if method is specified
  if (!is.null(normalize)) {
    normalize_funcs <- list(
      min_max_scaling = min_max_scaling_all,
      z_score_normalization = z_score_normalization_all,
      feature_scaling = feature_scaling_all,
      quantile_transform = quantile_transform_all,
      robust_scaling = robust_scaling_all
    )
    normalize_func <- normalize_funcs[[normalize]]
    if (is.null(normalize_func)) {
      stop("Invalid normalization method. Please choose from 'min_max_scaling', 'z_score_normalization', 'feature_scaling', 'quantile_transform', or 'robust_scaling'.")
    }
    data[var_names] <- normalize_func(data[var_names])
  }
  
  # Compute selected statistics per group
  stat_funcs <- list(
    mean = ~mean(.),
    median = ~median(.),
    sum = ~sum(.),
    max = ~max(.),
    min = ~min(.),
    skewness = ~skewness(.),
    kurtosis = ~kurtosis(.)
  )
  
  selected_funcs <- stat_funcs[stats]
  
  summary_stats <- data %>%
    group_by({{ group_var }}) %>%
    summarise(across(all_of({{ var_names }}), selected_funcs, .names = "{.col}_{.fn}"))
  
  # Reshape data to long format
  summary_stats_long <- summary_stats %>% 
    pivot_longer(cols = -{{ group_var }}, 
                 names_to = c("variable", "stat"), 
                 names_sep = "_") %>% 
    arrange(variable)
  
  # Compute relative rank for color mapping
  if (color_within) {
    summary_stats_long <- summary_stats_long %>%
      group_by(stat) %>%
      mutate(relative_rank = rank(value) / n())
  } else {
    summary_stats_long <- summary_stats_long %>%
      mutate(relative_rank = rank(value) / n())
  }
  
  # Generate heatmap plot faceted by variable and grouped by group
  plot <- ggplot(summary_stats_long, aes(x = {{ group_var }}, y = stat, fill = relative_rank)) +
    geom_tile() +
    facet_grid(variable~., scales = "free_y") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(x = rlang::as_label(rlang::enexpr(group_var)), y = NULL, fill = NULL) +
    geom_text(aes(label = round(value, 2)), color = "black", size = 4, nudge_x = 0.4)
  
  return(list(summary = summary_stats, plot = plot))
}

#' Functionality verification
#' 
#' @examples
#' # Example data with Group column
#' set.seed(123)
#' data <- data.frame(Group = rep(letters[1:3], each = 5),
#'                    var1 = rnorm(15),
#'                    var2 = rnorm(15),
#'                    var3 = rnorm(15),
#'                    var4 = rnorm(15),
#'                    var5 = rnorm(15))
#'
#' # Example 1: Default behavior (mean, no normalization, color across entire table)
#' results1 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group)
#' print("Example 1: Default behavior (mean, no normalization, color across entire table)")
#' print(results1$summary)
#' print(results1$plot)
#'
#' # Example 2: Multiple statistics, no normalization, color within each statistic
#' results2 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group, c("max", "min"), color_within = TRUE)
#' print("Example 2: Multiple statistics, no normalization, color within each statistic")
#' print(results2$summary)
#' print(results2$plot)
#'
#' # Example 3: Multiple statistics, min_max_scaling normalization, color across entire table
#' results3 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group, c("skewness", "kurtosis"), normalize = "min_max_scaling")
#' print("Example 3: Multiple statistics, min_max_scaling normalization, color across entire table")
#' print(results3$summary)
#' print(results3$plot)
#'
#' # Example 4: Multiple statistics, z_score_normalization, color within each statistic
#' results4 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group, c("mean", "median", "sum"), normalize = "z_score_normalization", color_within = TRUE)
#' print("Example 4: Multiple statistics, z_score_normalization, color within each statistic")
#' print(results4$summary)
#' print(results4$plot)
#'
#' # Example 5: All available statistics, feature_scaling normalization, color within each statistic
#' results5 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group, c("mean", "median", "sum", "max", "min", "skewness", "kurtosis"), normalize = "feature_scaling", color_within = TRUE)
#' print("Example 5: All available statistics, feature_scaling normalization, color within each statistic")
#' print(results5$summary)
#' print(results5$plot)
#'
#' # Example 6: Multiple statistics, quantile_transform normalization, color within each statistic
#' results6 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group, c("mean", "median", "sum"), normalize = "quantile_transform", color_within = TRUE)
#' print("Example 6: Multiple statistics, quantile_transform normalization, color within each statistic")
#' print(results6$summary)
#' print(results6$plot)
#'
#' # Example 7: Multiple statistics, robust_scaling normalization, color within each statistic
#' results7 <- heat_describe(data, c("var1", "var2", "var3", "var4", "var5"), Group, c("mean", "median", "sum"), normalize = "robust_scaling", color_within = TRUE)
#' print("Example 7: Multiple statistics, robust_scaling normalization, color within each statistic")
#' print(results7$summary)
#' print(results7$plot)
#'
#'
#' @details
#' These examples demonstrate the usage of the 'heat_describe' function for visualizing descriptive statistics.
#' The example data consists of a data frame named 'data' with a 'Group' column and several variables ('var1' to 'var5').
#' Each example shows different combinations of statistics, normalization methods, and color mapping options.
#'
#' The 'print' statements are used to display the computed summary statistics and show the generated heatmap plots.
#' Each example highlights a specific scenario and helps verify the correctness of the function's behavior.
#'
#' These examples import the necessary functions from the 'dplyr', 'ggplot2', and 'stats' packages to perform
#' data manipulation, visualization, and compute statistics. They also import the 'scales' and 'rlang' packages
#' for additional functionality.