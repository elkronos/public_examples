# Load packages
library(data.table)
library(ggplot2)
library(nortest)
library(rlang)
library(dunn.test)

anova_kw <- function(data, response_var, group_vars_vec, plot_qq = FALSE) {
  
  # Check if response_var exists in the data
  if (!(response_var %in% names(data))) stop("response_var not found in data")
  
  # Check if response_var is numeric
  if (!is.numeric(data[[response_var]])) stop("response_var must be numeric")
  
  # Check if all group_vars_vec elements exist in the data
  not_found_vars <- setdiff(group_vars_vec, names(data))
  if (length(not_found_vars) > 0) stop(paste("The following group_vars_vec elements are not found in data:", paste(not_found_vars, collapse = ", ")))
  
  # Convert data to data.table
  data <- data.table(data)
  
  # Create interaction term based on group variables
  interaction_vars <- data[, lapply(.SD, as.character), .SDcols = group_vars_vec]
  data[, 'interaction_term' := do.call(interaction, interaction_vars)]
  
  # Build a model formula and calculate residuals
  model_formula <- as.formula(paste(response_var, "~", "interaction_term"))
  residuals <- resid(aov(model_formula, data = data))
  
  # Perform Anderson-Darling test for normality
  ad_test <- ad.test(residuals)
  
  # Generate QQ plot if requested
  if (plot_qq) {
    qqnorm(residuals)
    qqline(residuals, col = "steelblue", lwd = 2)
  }
  
  # Conduct Kruskal-Wallis test
  kruskal_test <- kruskal.test(formula = model_formula, data = data)
  
  # Dunn's Test for post-hoc analysis if necessary
  posthoc <- NULL
  if (length(unique(data[[group_vars_vec[1]]])) > 2 || length(group_vars_vec) > 1) {
    posthoc_res <- dunn.test(data[[response_var]], g = data$interaction_term, method = "bh")
    posthoc <- posthoc_res  # Extract the result dataframe from the output
  }
  
  # Create a boxplot for each interaction term group, color-coded by the group
  plot <- ggplot(data, aes(x = !!rlang::sym(group_vars_vec[1]), y = !!rlang::sym(response_var))) +
    geom_boxplot(aes(fill = !!rlang::sym(group_vars_vec[1])), show.legend = FALSE) +
    scale_fill_brewer(palette = "Dark2") +
    labs(
      title = "Boxplot of Response by Group", 
      subtitle = paste("Response Variable:", response_var, " Group Variables:", paste(group_vars_vec, collapse = ", ")),
      x = "Interaction Group",
      y = "Response Variable Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      plot.subtitle = element_text(hjust = 0.5, size = 15),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  # Add facetting for each additional grouping variable
  if (length(group_vars_vec) > 1) {
    for (var in group_vars_vec[-1]) {
      plot <- plot + facet_grid(paste(var, "~ ."))
    }
  }
  
  # Print the plot
  print(plot)
  
  # Return a list of results including the plot, test results, and posthoc analysis (if applicable)
  results <- list(
    assumptions = list(
      qqplot = residuals,  # Note: this is not a plot but the residuals from the model
      ad_test = ad_test
    ),
    kruskal_test = kruskal_test,
    posthoc = posthoc
  )
  
  return(results)
}

# Synthetic data generation and example usage
set.seed(123)  # For reproducibility

# Single factor example
group <- rep(c("Group A", "Group B", "Group C"), each = 50)
value <- c(rnorm(50, mean = 5, sd = 1),
           rnorm(50, mean = 7, sd = 1.5),
           rnorm(50, mean = 4, sd = 0.8))

data_single <- data.frame(group, value)
data_single$group <- as.factor(data_single$group)  # Convert to factor

# Two-factor example
group1 <- rep(c("Group A", "Group B"), each = 50)
group2 <- rep(c("Group X", "Group Y"), times = 50)
value <- c(rnorm(50, mean = 5, sd = 1),
           rnorm(50, mean = 7, sd = 1.5),
           rnorm(50, mean = 4, sd = 0.8),
           rnorm(50, mean = 6, sd = 1.2))

data_double <- data.frame(group1, group2, value)
data_double$group1 <- as.factor(data_double$group1)  # Convert group1 to factor
data_double$group2 <- as.factor(data_double$group2)  # Convert group2 to factor

# Single factor test
results_single <- anova_kw(data_single, "value", c("group"), plot_qq = TRUE)

# Two-factor test
results_double <- anova_kw(data_double, "value", c("group1", "group2"), plot_qq = TRUE)
