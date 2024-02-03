# Load libraries
library(lme4)
library(car)
library(dplyr)
library(scales)
library(sjPlot)
library(ggplot2)
library(knitr)
library(psych)
library(apaTables)
library(car)
library(coefplot)

########################################################################### Make fake data

# Set seed for reproducibility
set.seed(123)

# Create a sample data frame with 50k rows
sample_data <- data.frame(
  individual_identifier = sample(paste0(rep(LETTERS[1:20], each = 2500), 1:2500), 400000, replace = TRUE),
  survey_date = sample(seq(as.Date('2000/01/01'), as.Date('2023/01/01'), by="day"), 400000, replace = TRUE),
  mental_health = sample(1:100, 400000, replace = TRUE),
  age = sample(18:100, 400000, replace = TRUE),
  sex = sample(c("M", "F"), 400000, replace = TRUE),
  move_date = sample(c(NA, seq(as.Date('2000/01/01'), as.Date('2023/01/01'), by="day")), 400000, replace = TRUE),
  move_reason = sample(c(NA, 1:7), 400000, replace = TRUE)
)

# Compute descriptive statistics
descriptives <- describe(sample_data)

# Generate APA style table
apa_table <- apaTables::apa.descriptives(descriptives, filename = "descriptives_table.doc", table.number = 1)

########################################################################### Pre processing

# Clean up data: replace move_date NAs with the survey_date, assuming no move if not specified
sample_data <- sample_data %>% mutate(move_date = ifelse(is.na(move_date), survey_date, move_date))

# Convert dates to numeric (days since a fixed date) to use in the model
sample_data$survey_date <- as.numeric(difftime(sample_data$survey_date, min(sample_data$survey_date), units = "days"))
sample_data$move_date <- as.numeric(difftime(sample_data$move_date, min(sample_data$move_date), units = "days"))

########################################################################### Pre processing (Modified)

# Identify actual moves (excluding instances where move_date is same as survey_date)
sample_data <- sample_data %>%
  mutate(actual_move = ifelse(move_date != survey_date, 1, 0))

# Count the actual number of moves for each individual
move_counts <- sample_data %>%
  group_by(individual_identifier) %>%
  summarise(number_of_moves = sum(actual_move))

# Merge this count back into the sample_data data frame
sample_data <- merge(sample_data, move_counts, by = "individual_identifier")

# Rescale continuous variables
sample_data <- sample_data %>%
  mutate(
    age_scaled = scale(age),
    mental_health_scaled = scale(mental_health),
    number_of_moves_scaled = scale(number_of_moves)
  )


########################################################################### Make models (Modified)

# Model 1: Null model with random intercept for individual identifier
model_null <- lmer(mental_health ~ 1 + (1|individual_identifier), data = sample_data, REML = FALSE)

# Model 2: Add number of moves as a fixed effect
model_num_moves <- lmer(mental_health ~ number_of_moves + (1|individual_identifier), data = sample_data, REML = FALSE)

# Model 3: Add random slope for number of moves
model_num_moves_slope <- lmer(mental_health ~ number_of_moves + (number_of_moves|individual_identifier), data = sample_data, REML = FALSE)

# Model 4: Add age and sex (or reason or anything else - this is WITHOUT interactions)
model_age_sex_moves <- lmer(mental_health ~ number_of_moves + age + sex + (1|individual_identifier), data = sample_data, REML = FALSE)

# Model 5: Add age and sex with number of moves as a random slope (still without interactions)
model_age_sex_moves_slope <- lmer(mental_health ~ number_of_moves + age + sex + (number_of_moves|individual_identifier), data = sample_data, REML = FALSE)

########################################################################### Compare models (Modified)

# Compare models using ANOVA
anova(model_null, model_num_moves)
anova(model_num_moves, model_num_moves_slope)
anova(model_num_moves, model_age_sex_moves)
anova(model_num_moves_slope, model_age_sex_moves_slope)


########################################################################### Model summary

# Call summary
summary(model_age_sex_moves_slope)

# Model Summary Interpretation

# Model: mental_health ~ number_of_moves + age + sex + (number_of_moves | individual_identifier)
# Data: sample_data

# Model Fit Statistics
# - AIC: 3825240, BIC: 3825327, Log-Likelihood: -1912612, Deviance: 3825224
# - Lower AIC and BIC values typically indicate a better fit.

# Scaled Residuals
# - Range from -1.73426 to 1.73354, suggesting symmetric distribution around the median (0.01002).

# Random Effects
# - Variance in Intercept (6.97920) and Number of Moves (0.04876) with perfect negative correlation (-1.00)
# - Indicates potential overparameterization or insufficient data for estimating random slopes.

# Fixed Effects
# - (Intercept): 50.561697, suggesting average mental health score when other predictors are zero.
# - number_of_moves: Coefficient 0.001452, very small positive effect, not statistically significant (t = 0.090).
# - age: Coefficient -0.001895, small negative effect, not statistically significant (t = -0.996).
# - sexM: Coefficient 0.075608, slight positive effect for being male, not statistically significant (t = 0.828).

# Convergence
# - Model converged successfully, but has a boundary (singular) fit indicating potential issues.

# Interpretation Notes
# - Weak or non-significant associations of number_of_moves, age, and sex with mental health.
# - Perfect negative correlation in random effects and singular fit suggest overcomplex model for the data.
# - Results should be interpreted with caution; consider simplifying the model or reassessing data.


########################################################################### Make table

tab_model(model_num_moves, 
          show.se = TRUE, show.std = TRUE, show.stat = TRUE)

# Interpretation of the Model Output Table

# Fixed Effects:
# - (Intercept): The estimated average mental health score is 50.49 when all other predictors are held constant.
# - Number of Moves: The estimate of 0.00 with a standard error of 0.02 suggests that the number of moves has an insignificant effect on mental health (p-value = 0.930). The confidence interval (-0.03 to 0.03) includes zero, further indicating a lack of significant effect.

# Random Effects:
# - σ2 (Residual Variance): The variance in mental health scores not explained by the model is 832.96.
# - τ00 individual_identifier (Variance of Random Intercepts): The variance attributed to the individual differences (individual_identifier) is 0.16, which is relatively small.
# - ICC (Intraclass Correlation Coefficient): An ICC of 0.00 suggests that there is negligible clustering within the individual_identifier grouping.
# - N individual_identifier: The model includes random intercepts for 49,991 individual identifiers.
# - Observations: The model is based on 400,000 observations.

# Model Fit:
# - Marginal R2 / Conditional R2: Both the marginal and conditional R-squared values are 0.000, indicating that the model explains none of the variability in the mental health scores. This might suggest that the predictors used in the model are not suitable for explaining the variation in mental health.

# Overall Interpretation:
# This model suggests that the number of moves does not significantly predict mental health scores, as indicated by the non-significant p-value and the confidence interval of the estimate. The random effects indicate minimal variability attributed to individual differences. The low R-squared values suggest that the model has limited explanatory power for the variability in mental health scores, implying the need to explore additional or alternative predictors.

########################################################################### Plots

# Look at the random effects
random_effects <- ranef(model_age_sex_moves_slope)$individual_identifier %>%
  rownames_to_column("individual_identifier")

# Display random effects as a table
random_effects %>%
  knitr::kable()

## Plot random effects using dotplot
dotplot(ranef(model_age_sex_moves_slope))

# Plot for intercepts
intercept_sd <- sd(random_effects$`(Intercept)`)
number_of_moves_sd <- sd(random_effects$number_of_moves)

# Plot for intercepts
int_plt <- random_effects %>%
  ggplot(aes(x = `(Intercept)`, y = reorder(individual_identifier, `(Intercept)`))) +
  geom_errorbar(aes(xmin = `(Intercept)` - intercept_sd, xmax = `(Intercept)` + intercept_sd), width = 0, size = 1) +
  geom_point(size = 3, shape = 21, color = "black", fill = "white") +
  geom_vline(xintercept = 0, color = "red", size = 1, linetype = "dashed") +
  labs(x = "Intercept", y = "Individual Identifier", title = "Random Intercepts")

# Display the plot for intercepts
print(int_plt)

# Plot for residuals
plot(model_age_sex_moves_slope)
hist(resid(model_age_sex_moves_slope))

# Plotting Residuals vs Fitted values
fitted_values <- predict(model_age_sex_moves_slope, re.form = NULL)
residuals <- resid(model_age_sex_moves_slope)

plot(x = fitted_values, y = residuals, pch = 19, main = "Resid ~ Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 3, lty = 2)


# Extract the fixed effect coefficients
model_coefficients <- coef(model_age_sex_moves_slope)

# Plot the coefficients
coefplot(model_age_sex_moves_slope, sort = "magnitude", intercept = TRUE)

# Coefficients Plot Interpretation Notes

# - Direction of Effects:
#   Positive Coefficients indicate a direct relationship with the dependent variable.
#   Negative Coefficients indicate an inverse relationship with the dependent variable.

# - Magnitude of Effects:
#   Larger absolute values of coefficients suggest stronger effects.
#   Smaller absolute values indicate weaker effects.

# - Statistical Significance:
#   If confidence intervals (horizontal lines or bars) do not cross zero, the effect is likely significant.
#   If they cross zero, the effect is not statistically significant at the chosen confidence level (typically 95%).

# - Interpretation of Intercept:
#   Represents the expected value of the dependent variable when all predictors are zero. 
#   Context-dependent and may be less interpretable in some models.

# - Relative Influence:
#   Compare the magnitude and direction of coefficients to understand the relative influence of predictors.
#   Ensure comparability (e.g., via standardization) for meaningful comparisons.


################################################################## Adding reasons

vif_model <- lm(mental_health ~ number_of_moves + age + sex + reason_1 + reason_2 + reason_3 + reason_4 + reason_5 + reason_6 + reason_7, data = sample_data)
vif(vif_model)

# Update the model to include reason variables
model_age_sex_moves_reasons_slope <- lmer(mental_health ~ number_of_moves + age + sex + reason_1 + reason_2 + reason_3 + reason_4 + reason_5 + reason_6 + reason_7 + (number_of_moves|individual_identifier), data = sample_data, REML = FALSE)

# Descriptively review reaons (assuming each is named 'reason_1', 'reason_2', etc)

# Frequency Distribution of each reason
for(i in 1:7) {
  cat("Frequency Distribution for Reason", i, ":\n")
  print(table(sample_data[[paste0("reason_", i)]]))
  cat("\n")
}

# Cross-tabulations with a categorical variable (e.g., sex)
for(i in 1:7) {
  cat("Cross-tabulation with Sex for Reason", i, ":\n")
  print(table(sample_data$sex, sample_data[[paste0("reason_", i)]]))
  cat("\n")
}

# Summary statistics for mental health scores for each reason
for(i in 1:7) {
  cat("Summary Statistics for Mental Health for Reason", i, ":\n")
  summary_stats <- sample_data %>%
    group_by(sample_data[[paste0("reason_", i)]]) %>%
    summarise(Mean = mean(mental_health, na.rm = TRUE), 
              SD = sd(mental_health, na.rm = TRUE),
              Count = n())
  print(summary_stats)
  cat("\n")
}

# Optionally, visualize the distribution
for(i in 1:7) {
  ggplot(sample_data, aes_string(x = paste0("reason_", i), y = "mental_health")) +
    geom_boxplot() +
    labs(title = paste("Mental Health Distribution for Reason", i))
}


