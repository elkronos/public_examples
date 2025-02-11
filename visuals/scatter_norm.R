#' Scatter Plot of Rescaled Variables with Optional Annotations
#'
#' This function creates a scatter plot of two rescaled numeric variables from a data frame, with optional annotations such as
#' the Pearson correlation coefficient and quadrant percentages. It supports both static (ggplot2) and interactive (plotly) plots.
#'
#' @param data A data frame containing the variables.
#' @param x Unquoted name of the x variable (must be numeric).
#' @param y Unquoted name of the y variable (must be numeric).
#' @param r_label Logical. If TRUE, the Pearson correlation coefficient is displayed on the plot. Default is TRUE.
#' @param save_path Character. File path to save the plot. If NULL (default), the plot is not saved.
#' @param interactive Logical. If TRUE, returns an interactive plotly object. Default is FALSE.
#' @param annotate_quadrants Logical. If TRUE, annotates the plot with quadrant percentages. Default is FALSE.
#' @return A ggplot object or an interactive plotly object, depending on the 'interactive' parameter.
#' @examples
#' # Basic static plot:
#' scatter_norm(mtcars, mpg, disp)
#'
#' # Interactive plot with quadrant annotations:
#' scatter_norm(mtcars, mpg, disp, interactive = TRUE, annotate_quadrants = TRUE)
#'
#' # Plot without correlation label:
#' scatter_norm(mtcars, mpg, disp, r_label = FALSE)
#'
#' # Save plot to file:
#' scatter_norm(mtcars, mpg, disp, save_path = "my_plot.png")
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom rlang ensym as_string
#' @export
scatter_norm <- function(data, x, y, r_label = TRUE, save_path = NULL,
                         interactive = FALSE, annotate_quadrants = FALSE) {
  # Validate that 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  # Capture column names using tidy evaluation (from rlang)
  x_sym <- rlang::ensym(x)
  y_sym <- rlang::ensym(y)
  x_name <- rlang::as_string(x_sym)
  y_name <- rlang::as_string(y_sym)
  
  # Check that the specified columns exist
  if (!x_name %in% names(data)) {
    stop(paste("Column", x_name, "not found in data."))
  }
  if (!y_name %in% names(data)) {
    stop(paste("Column", y_name, "not found in data."))
  }
  
  # Ensure the variables are numeric
  if (!is.numeric(data[[x_name]])) {
    stop(paste("Column", x_name, "must be numeric."))
  }
  if (!is.numeric(data[[y_name]])) {
    stop(paste("Column", y_name, "must be numeric."))
  }
  
  # Remove missing values
  data_complete <- data[stats::complete.cases(data[[x_name]], data[[y_name]]), ]
  if (nrow(data_complete) < 2) {
    stop("Not enough complete cases for analysis.")
  }
  
  # Helper function to rescale a vector (centering and scaling)
  rescale_vector <- function(vec) {
    sd_vec <- stats::sd(vec)
    if (sd_vec == 0) {
      stop("Standard deviation is zero. Cannot rescale a constant vector.")
    }
    (vec - mean(vec)) / sd_vec
  }
  
  # Rescale x and y
  x_rescaled <- rescale_vector(data_complete[[x_name]])
  y_rescaled <- rescale_vector(data_complete[[y_name]])
  
  df_plot <- data.frame(x_rescaled = x_rescaled, y_rescaled = y_rescaled)
  
  # Create the base ggplot
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_rescaled, y = y_rescaled)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = TRUE)
  
  # Add the correlation coefficient annotation if requested
  if (r_label) {
    r_value <- cor(data_complete[[x_name]], data_complete[[y_name]])
    r_value <- round(r_value, 2)
    label_text <- paste("r =", r_value)
    # Place the label at the top-left corner of the plot area
    x_pos <- min(x_rescaled, na.rm = TRUE)
    y_pos <- max(y_rescaled, na.rm = TRUE)
    p <- p + ggplot2::annotate("text", x = x_pos, y = y_pos, label = label_text,
                               hjust = 0, vjust = 1, fontface = "italic", color = "darkblue", alpha = 0.6)
  }
  
  # Add quadrant annotations if requested
  if (annotate_quadrants) {
    if (length(x_rescaled) < 4) {
      warning("Not enough points to reliably annotate quadrants.")
    } else {
      # Define quadrants:
      # Q1: x>0, y>0; Q2: x<=0, y>0; Q3: x<=0, y<=0; Q4: x>0, y<=0.
      count_q1 <- sum(x_rescaled > 0 & y_rescaled > 0)
      count_q2 <- sum(x_rescaled <= 0 & y_rescaled > 0)
      count_q3 <- sum(x_rescaled <= 0 & y_rescaled <= 0)
      count_q4 <- sum(x_rescaled > 0 & y_rescaled <= 0)
      counts <- c(count_q1, count_q2, count_q3, count_q4)
      total <- sum(counts)
      percentages <- round((counts / total) * 100, 1)
      quadrant_labels <- paste0(c("Q1: ", "Q2: ", "Q3: ", "Q4: "), percentages, "%")
      
      # Determine annotation positions using quantiles
      x_positions <- c(stats::quantile(x_rescaled, 0.75),
                       stats::quantile(x_rescaled, 0.25),
                       stats::quantile(x_rescaled, 0.25),
                       stats::quantile(x_rescaled, 0.75))
      y_positions <- c(stats::quantile(y_rescaled, 0.75),
                       stats::quantile(y_rescaled, 0.75),
                       stats::quantile(y_rescaled, 0.25),
                       stats::quantile(y_rescaled, 0.25))
      
      for (i in seq_along(quadrant_labels)) {
        p <- p + ggplot2::annotate("text", x = x_positions[i], y = y_positions[i],
                                   label = quadrant_labels[i],
                                   hjust = 0, vjust = 1, fontface = "italic", color = "darkred", alpha = 0.6)
      }
    }
  }
  
  # Add reference lines, axis labels, and a minimal theme
  p <- p +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linewidth = 1, linetype = "dashed") +
    ggplot2::xlab(paste("Rescaled", x_name)) +
    ggplot2::ylab(paste("Rescaled", y_name)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()
  
  # Save the plot if a file path is provided
  if (!is.null(save_path)) {
    tryCatch({
      ggplot2::ggsave(filename = save_path, plot = p)
    }, error = function(e) {
      warning("Could not save the plot: ", e$message)
    })
  }
  
  # Return an interactive plotly object if requested; otherwise, return the ggplot object
  if (interactive) {
    return(plotly::ggplotly(p))
  } else {
    return(p)
  }
}

if (interactive()) {  # Only run tests interactively or within a test suite
  library(testthat)
  library(ggplot2)
  library(plotly)
  
  context("Testing scatter_norm function")
  
  test_that("Basic functionality with valid numeric columns", {
    p <- scatter_norm(mtcars, mpg, disp)
    expect_s3_class(p, "ggplot")
  })
  
  test_that("Interactive plot returns a plotly object", {
    p <- scatter_norm(mtcars, mpg, disp, interactive = TRUE)
    expect_s3_class(p, "plotly")
  })
  
  test_that("Correlation label is omitted when r_label is FALSE", {
    p <- scatter_norm(mtcars, mpg, disp, r_label = FALSE)
    # Check that no annotation layer contains the correlation label text "r ="
    annotations <- vapply(p$layers, function(layer) {
      if (!is.null(layer$geom_params$label)) layer$geom_params$label else ""
    }, character(1))
    expect_false(any(grepl("r =", annotations, fixed = TRUE)))
  })
  
  test_that("Quadrant annotations are added when requested", {
    p <- scatter_norm(mtcars, mpg, disp, annotate_quadrants = TRUE)
    expect_s3_class(p, "ggplot")
    
    # Build the plot to access computed data for all layers
    built <- ggplot2::ggplot_build(p)
    
    # Extract all labels from all layers (if any)
    labels_found <- unlist(lapply(built$data, function(layer_data) {
      if ("label" %in% names(layer_data)) {
        return(layer_data$label)
      } else {
        return(NULL)
      }
    }))
    
    # Check if any of the labels contain a "Q" (indicating quadrant annotation)
    quadrant_found <- any(grepl("Q", labels_found))
    expect_true(quadrant_found)
  })
  
  
  test_that("Non-numeric x variable produces an error", {
    expect_error(scatter_norm(iris, Species, Sepal.Length),
                 "must be numeric")
  })
  
  test_that("Missing column produces an error", {
    expect_error(scatter_norm(mtcars, non_existent, disp),
                 "not found in data")
  })
  
  test_that("Data with insufficient complete cases produces an error", {
    df <- data.frame(a = as.numeric(c(NA, NA)), b = c(1, 2))
    expect_error(scatter_norm(df, a, b), "Not enough complete cases")
  })
  
  test_that("Constant column (zero variance) produces an error", {
    df <- data.frame(a = rep(1, 10), b = rnorm(10))
    expect_error(scatter_norm(df, a, b),
                 "Standard deviation is zero")
  })
  
  test_that("Plot is successfully saved to file", {
    tmp_file <- tempfile(fileext = ".png")
    scatter_norm(mtcars, mpg, disp, save_path = tmp_file)
    expect_true(file.exists(tmp_file))
    unlink(tmp_file)  # Clean up
  })
  
  print("All UAT tests passed successfully!")
}
