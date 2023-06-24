library(ggplot2)
library(dplyr)
library(scales)

anova_bin <- function(data, response_var, group_var) {
  # Load required packages
  require(ggplot2)
  
  # Check if group_var is a character vector, if not convert it to a character vector
  if (!is.character(group_var)) {
    group_var <- as.character(group_var)
  }
  
  # Convert each grouping variable to a factor if not already
  for (var in group_var) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- as.factor(data[[var]])
    }
  }
  
  # Perform logistic regression
  formula <- as.formula(paste(response_var, "~", paste(group_var, collapse = "+")))
  logistic_regression <- glm(formula, data = data, family = binomial())
  
  # Perform deviance ANOVA
  deviance_anova <- stats::anova(logistic_regression, test = "Chisq")
  
  # Compute effect size (odds ratios) and confidence intervals
  effect_size <- broom::tidy(logistic_regression, exponentiate = TRUE, conf.int = TRUE)
  
  # Compute proportion by group
  prop_data <- data %>%
    group_by(across(all_of(group_var)), across(all_of(response_var))) %>%
    summarize(count = n()) %>%
    group_by(across(all_of(group_var))) %>%
    mutate(prop = count / sum(count))
  
  # Generate a bar plot of the response by group
  count_plot <- ggplot(data = prop_data, aes(x = !!rlang::sym(group_var), y = prop, fill = !!rlang::sym(response_var), group = interaction(!!rlang::sym(group_var), !!rlang::sym(response_var)))) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(aes(label = scales::percent(prop), group = interaction(!!rlang::sym(group_var), !!rlang::sym(response_var))), position = position_fill(vjust = 0.5), size = 4) +
    labs(x = group_var, y = "Proportion", fill = response_var, 
         title = paste("Proportion of", response_var, "by", group_var)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
  
  print(count_plot)
  
  # Return results
  results <- list(
    deviance_anova = deviance_anova,
    effect_size = effect_size,
    count_plot = count_plot
  )
  
  return(results)
}

# One-way "ANOVA"
result1 <- anova_bin(data, "response_var", "group_var1")
