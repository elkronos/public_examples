# Import necessary libraries
library(coin)
library(ggplot2)
library(data.table)
library(nortest)
library(effsize)

# Define the function
anova_welch <- function(data, response_var, group_vars_list) {
  data <- data.table(data)
  
  # Create interaction term
  interaction_vars <- data[, lapply(.SD, as.character), .SDcols = group_vars_list]
  data[, 'interaction_term' := do.call(interaction, interaction_vars)]
  
  # Create a formula for the model and calculate residuals
  model_formula <- as.formula(paste(response_var, "~", "interaction_term"))
  residuals <- resid(aov(model_formula, data = data))
  
  # Perform Anderson-Darling normality test and plot QQ-plot for residuals
  qqplot_res <- qqnorm(residuals)
  ad_test <- ad.test(residuals)
  
  # Perform Welch's ANOVA
  welch_anova <- oneway_test(model_formula, data = data, distribution = "asymptotic")
  
  # Perform post-hoc tests with Bonferroni correction if there are more than 2 groups or more than 1 group variable
  posthoc <- NULL
  if (length(unique(data[[group_vars_list[1]]])) > 2 || length(group_vars_list) > 1) {
    posthoc <- pairwise.t.test(data[[response_var]], data$interaction_term, p.adjust.method = "bonferroni",
                               pool.sd = FALSE, alternative = "two.sided")
  }
  
  # Calculate means and standard deviations by group
  means <- data[, .(mean = mean(get(response_var)), sd = sd(get(response_var))), by = interaction_term]
  
  # Calculate number of samples in each group
  n <- data[, .N, by = interaction_term]
  
  # Calculate confidence intervals for each group
  ci <- data[, .(mean = mean(get(response_var)), 
                 sd = sd(get(response_var)), 
                 ci_low = mean(get(response_var)) - qt(0.975, .N - 1) * (sd(get(response_var)) / sqrt(.N)),
                 ci_high = mean(get(response_var)) + qt(0.975, .N - 1) * (sd(get(response_var)) / sqrt(.N))), by = interaction_term]
  
  # Calculate effect sizes (Cohen's d)
  groups <- unique(data$interaction_term)
  cohen_d_list <- list()
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      group1 <- as.numeric(data[interaction_term == groups[i], get(response_var)])
      group2 <- as.numeric(data[interaction_term == groups[j], get(response_var)])
      cohen_d_value <- effsize::cohen.d(group1, group2)$estimate
      cohen_d_list[[paste0(groups[i], "_vs_", groups[j])]] <- cohen_d_value
    }
  }
  
  # Plot barplot
  means_plot <- ggplot(means, aes(x = interaction_term, y = mean, fill = interaction_term)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    geom_errorbar(aes(ymin = mean - sd/2, ymax = mean + sd/2), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label = round(mean, 2)), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
    labs(x = "Interaction Term", y = "Mean", title = "Group Means and Standard Deviations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Return list of results
  results <- list(
    assumptions = list(
      qqplot = qqplot_res,
      ad_test = ad_test
    ),
    welch_anova = welch_anova,
    posthoc = posthoc,
    barplot_data = means,
    confidence_intervals = ci,
    effect_sizes = cohen_d_list,
    means_plot = means_plot
  )
  
  return(results)
}

# Create example dataset
set.seed(1)  # For reproducibility

group <- rep(c("Group A", "Group B", "Group C"), each = 50)
value <- c(rnorm(50, mean = 5, sd = 1),
           rnorm(50, mean = 7, sd = 1.5),
           rnorm(50, mean = 4, sd = 0.8))

data <- data.frame(group = factor(group), value = value)

# Run Welch's ANOVA and post-hoc tests
results <- anova_welch(data, "value", c("group"))

# Print the barplot with means and standard deviations
print(results$means_plot)
