# THIS FUNCTION WAS MADE USING CHAT GPT AS AN EXAMPLE FOR PROMPT ENGINEERING

library(ggplot2)
library(assertthat)
library(mgcv)

smooth_scatter <- function(data, x, y, color = NULL, method = "loess", method.args = NULL, rug = FALSE, show_r2 = FALSE) {
  # Convert input to strings
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  if (!is.null(substitute(color))) {
    color <- as.character(substitute(color))
  }
  
  # Input validation
  assert_that(is.data.frame(data))
  assert_that(is.character(x), is.character(y))
  assert_that(is.null(color) || is.character(color))
  assert_that(is.character(method))
  assert_that(is.null(method.args) || is.list(method.args))
  assert_that(is.logical(rug), is.logical(show_r2))
  
  plot_variables <- c(x, y, color)
  plot_variables <- plot_variables[!is.na(plot_variables)]
  
  # Check if variables exist in the data
  assert_that(all(plot_variables %in% colnames(data)))
  
  # If color variable has <= 12 levels, convert to factor
  if (!is.null(color) && nlevels(as.factor(data[[color]])) <= 12) {
    data[[color]] <- as.factor(data[[color]])
  }
  
  # Base plot
  plot <- ggplot(data, aes_string(x = x, y = y))
  
  # Add color grouping if specified
  if (!is.null(color)) {
    plot <- plot + aes_string(color = color)
  }
  
  # Add points
  plot <- plot + geom_point()
  
  # Add smooth curves
  plot <- plot + geom_smooth(method = method, method.args = method.args, se = TRUE)
  
  # Add density rugs if specified
  if (rug) {
    plot <- plot + geom_rug(sides = "b", size = 0.5, alpha = 0.5)
  }
  
  # Add R-squared or deviance explained if specified
  if (show_r2 && method %in% c("lm", "glm", "gam")) {
    model <- do.call(method, c(list(formula = as.formula(paste(y, "~", x)), data = data), method.args))
    
    if (method == "gam") {
      r2 <- summary(model)$dev.expl * 100
      label <- "Deviance Explained"
    } else {
      r2 <- summary(model)$r.squared * 100
      label <- "R-squared"
    }
  }
  
  # Improve plot appearance
  plot <- plot + theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "lightgrey", color = "black")
    )
  
  return(plot)
}


# Create a fake data set
set.seed(123)
x <- rnorm(100)
y <- x^2 + rnorm(100)
color <- sample(c("red", "blue"), 100, replace = TRUE)
facet <- sample(c("A", "B"), 100, replace = TRUE)
data <- data.frame(x, y, color, facet)

# Example 1: plot a scatterplot with a smooth curve of the relationship between x and y
smooth_scatter(data, x, y)

# Example 2: plot a scatterplot with a smooth curve of the relationship between x and y, colored by the "color" variable
smooth_scatter(data, x, y, color = "color")

# Example 3: plot a scatterplot with a smooth curve of the relationship between x and y, with density rugs for both variables
smooth_scatter(data, x, y, rug = TRUE)

# Example 4: plot a scatterplot with a smooth curve of the relationship between x and y, with density rugs for both variables, and show R-squared
smooth_scatter(data, x, y, rug = TRUE, show_r2 = TRUE)

# Example 5: plot a scatterplot with a smooth curve of the relationship between x and y, colored by the "color" variable, with a different smoothing method
smooth_scatter(data, x, y, color = "color", method = "lm")

# Example 6: plot a scatterplot with a smooth curve of the relationship between x and y, faceted by the "facet" variable, with a different smoothing method and different method arguments
smooth_scatter(data, x, y, facet = "facet", method = "gam")

# Example 7: plot a scatterplot with a smooth curve of the relationship between x and y, faceted by the "facet" variable, with a different plot title and subtitle
smooth_scatter(data, x, y, facet = "facet", method = "gam", rug = TRUE, show_r2 = TRUE) +
  ggtitle("My plot title") +
  labs(subtitle = "My plot subtitle")

