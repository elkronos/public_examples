# Load required libraries
library(stacks)
library(mlbench)
library(tidymodels)
library(dplyr)
library(ggplot2)

# 1. Create a synthetic dataset using mlbench
set.seed(123)
data <- mlbench.2dnormals(n = 1000, cl = 2)
data <- as_tibble(data) %>%
  mutate(classes = as.factor(classes))

# 2. Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# 3. Define models and their workflows
# Logistic Regression
logistic_spec <- logistic_reg() %>% 
  set_engine("glm")
logistic_wf <- workflow() %>%
  add_model(logistic_spec) %>%
  add_formula(classes ~ .)

# Random Forest
rf_spec <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_formula(classes ~ .)

# K-Nearest Neighbors
knn_spec <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")
knn_wf <- workflow() %>%
  add_model(knn_spec) %>%
  add_formula(classes ~ .)

# 4. Create a resampling object for cross-validation
cv_folds <- vfold_cv(train_data, v = 5, strata = classes)

# 5. Fit the models with updated control settings
control_settings <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

logistic_res <- fit_resamples(
  logistic_wf, 
  resamples = cv_folds, 
  control = control_settings
)

rf_res <- fit_resamples(
  rf_wf, 
  resamples = cv_folds, 
  control = control_settings
)

knn_res <- fit_resamples(
  knn_wf, 
  resamples = cv_folds, 
  control = control_settings
)

# 6. Create and train the model stack
stack <- stacks() %>%
  add_candidates(logistic_res) %>%
  add_candidates(rf_res) %>%
  add_candidates(knn_res) %>%
  blend_predictions() %>%
  fit_members()

# 7. Evaluate the model on test data
predictions <- predict(stack, new_data = test_data)
combined_data <- bind_cols(test_data, predictions)
metrics <- yardstick::metrics(combined_data, truth = classes, estimate = .pred_class)

# 8. Visualization - ROC Curve
# Generate predictions with probabilities
predictions <- predict(stack, new_data = test_data, type = "prob")

# Combine the test data with the predictions
combined_data <- bind_cols(test_data, predictions)

# Check the structure of the new predictions to find the correct column names
str(predictions)

# Assuming the column name for the predicted probability of class 1 is '.pred_1' (replace with the actual column name)
stack_plot <- roc_curve(combined_data, truth = classes, .pred_1) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line() +
  coord_equal() +
  theme_minimal() +
  ggtitle("ROC Curve for Stacked Model")

# Print the ROC curve plot
print(stack_plot)