# Load libraries
library(lme4)
library(car)
library(dplyr)

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

########################################################################### Pre processing

# Clean up data: replace move_date NAs with the survey_date, assuming no move if not specified
sample_data <- sample_data %>% mutate(move_date = ifelse(is.na(move_date), survey_date, move_date))

# Convert dates to numeric (days since a fixed date) to use in the model
sample_data$survey_date <- as.numeric(difftime(sample_data$survey_date, min(sample_data$survey_date), units = "days"))
sample_data$move_date <- as.numeric(difftime(sample_data$move_date, min(sample_data$move_date), units = "days"))

########################################################################### Make models

# Model 1: Null model with random intercept for individual identifier
model_null <- lmer(mental_health ~ 1 + (1|individual_identifier), data = sample_data, REML = FALSE)

# Model 2: Add move date as a fixed effect
model_move_date <- lmer(mental_health ~ move_date + (1|individual_identifier), data = sample_data, REML = FALSE)

# Model 3: Add random slope for move date
model_move_date_slope <- lmer(mental_health ~ move_date + (move_date|individual_identifier), data = sample_data, REML = FALSE)

# Model 4: Add age and sex (or reason or anything else - this is WITHOUT interactions)
model_age_sex <- lmer(mental_health ~ move_date + age + sex + (1|individual_identifier), data = sample_data, REML = FALSE)

# Model 5: Add age and sex with move date as a random slope (still without interactions)
model_age_sex_slope <- lmer(mental_health ~ move_date + age + sex + (move_date|individual_identifier), data = sample_data, REML = FALSE)

########################################################################### Compare models

# Compare models using ANOVA
anova(model_null, model_move_date)
anova(model_move_date, model_move_date_slope)
anova(model_move_date, model_age_sex)
anova(model_move_date_slope, model_age_sex_slope)

# The anova function in R, when used with linear mixed-effects models 
# performs a comparison of the models to evaluate whether
# the addition of certain terms (like fixed effects or random effects) 
# significantly improves the model's fit to the data.
# The p-value tells you whether the differences in model fit between the models are 
# statistically significant. A low p-value (usually < 0.05) suggests that the more 
# complex model provides a significantly better fit to the data.
# This approach is used for model selection, where you start with a simpler model and 
# progressively add more terms. At each step, you use anova to test if the new model 
# (with more terms) is significantly better than the previous model. 
# This helps in deciding whether additional complexity (like adding random slopes or 
# interaction terms) is warranted.

###########################################################################  Code to visualize

# For example, to visualize the fixed effects of Model 4:
library(coefplot)
coefplot(model_age_sex, intercept = FALSE)
# generates a plot that visually represents the estimated coefficients (effect sizes) 
# of the fixed effects (excluding the intercept) in the model_age_sex. 
# This plot typically displays the point estimates of the coefficients for move_date,
# age, and sex, along with their confidence intervals. 
# The interpretation involves examining the direction (positive or negative), 
# magnitude, and statistical significance of these effects on the dependent variable (mental_health).


###########################################################################  

# Visualize random effects
library(lattice)
dotplot(ranef(model_move_date_slope, condVar = TRUE))

# Random Effects Display: The plot shows the estimated random effects for each level of the
# grouping factor (in your case, individual_identifier) in the model. 
# These random effects are the deviations of each group's intercept and/or slope (for move_date) 
# from the overall fixed effect estimate.
# The spread of these dots and their intervals provides insight into the variability of 
# the effect across individuals. A wide spread suggests substantial variability, 
# while a narrow spread indicates that the effect is more consistent across individuals.
# If your model includes random slopes (as model_move_date_slope does), 
# the plot can show how the relationship between the predictor (move_date) 
# and the outcome (mental_health) varies across individuals.
# The plot typically includes a reference line at zero. 
# Random effects whose confidence intervals do not overlap with this line are 
# those where the group's deviation from the overall estimate is statistically significant.


###########################################################################  Assumptions/diagnostics

# Model diagnostics
par(mfrow = c(2, 2))
plot(model_age_sex)

# Residuals vs Fitted: This plot shows the residuals (differences between observed and predicted values) against the fitted values (predicted values). It's used to check for non-linearity, unequal error variances, and outliers. Ideally, the points should be randomly scattered around the horizontal line at zero, indicating no pattern.
# Normal Q-Q Plot: This quantile-quantile plot compares the distribution of the residuals to a normal distribution. If the residuals are normally distributed, the points will closely follow the reference line. Deviations from this line suggest departures from normality.
# Scale-Location Plot (Spread vs Level): This plot shows the square root of the absolute residuals against the fitted values. It’s useful for checking the homoscedasticity of residuals (constant variance). Points spread evenly across the range of predictors indicate constant variance.
# Residuals vs Leverage: This plot helps to identify influential observations. Points that stand out far from the rest of the data, especially in the extremes of the x-axis (high leverage), can have a significant impact on the model fit. The Cook’s distance contours help to identify observations that might unduly influence the model.


###########################################################################  Predictions

# Predicted vs. observed
predicted_values <- predict(model_age_sex, re.form = NA)  # re.form = NA for fixed effects only
observed_values <- sample_data$mental_health

plot(observed_values, predicted_values, xlab = "Observed Mental Health", ylab = "Predicted Mental Health")
abline(0, 1)

# This plot is used to visually assess the accuracy of the model's predictions. By plotting observed values against predicted values, you can see how well the model is capturing the data.


########################################################################### Make tables

# Tables
library(xtable)

# Extract model summary
model_summary <- summary(model_age_sex)

# Convert to xtable
model_table <- xtable(model_summary$coefficients, caption = "Fixed Effects of Model Age and Sex")

# Print the table as HTML
print(model_table, type = "html", include.rownames = FALSE)


########################################################################### Model selection

AIC(model_null, model_move_date, model_move_date_slope, model_age_sex, model_age_sex_slope)
BIC(model_null, model_move_date, model_move_date_slope, model_age_sex, model_age_sex_slope)

# AIC is a measure of the relative quality of a statistical model for a given set of data. It balances model complexity against the model's ability to fit the data. A lower AIC value indicates a better model.
# Similar to AIC, BIC is used to select the best model and is based on the likelihood of the model. However, it introduces a stronger penalty for the number of parameters in the model compared to AIC.


########################################################################### Adding interactions

model_interaction <- lmer(mental_health ~ move_date * age + sex + (1|individual_identifier), data = sample_data, REML = FALSE)

# This is how to include interaction terms you might want to test



########################################################################### Singularity troubleshooting

# When you encounter a singularity issue in a mixed-effects model, 
# it usually indicates that there is a problem with the random effects structure of the model. 
# This could be due to overparameterization or because the data do not support the complexity 
# of the model.

# Try simpler models
# Model with only random intercepts
model_simplified <- lmer(mental_health ~ move_date + age + sex + (1|individual_identifier), data = sample_data, REML = FALSE)

# Or, Model with only random slopes for move_date
model_simplified_slope <- lmer(mental_health ~ move_date + age + sex + (0 + move_date|individual_identifier), data = sample_data, REML = FALSE)


# Check for multicollinearity
cor(sample_data[c("move_date", "age", "sex")])

# Examine distributions
aggregate(sample_data$mental_health, by = list(sample_data$individual_identifier), FUN = function(x) c(mean = mean(x), sd = sd(x)))
