# Load necessary libraries
library(tidymodels)
library(dplyr)
library(purrr)
library(yardstick)

# Step 1: Create a synthetic dataset
set.seed(123)
n <- 100
data <- tibble(
  x1 = rnorm(n),
  x2 = rnorm(n),
  y = 1.5 * x1 - 2 * x2 + rnorm(n)
)

# Split the data
set.seed(123)
data_split <- initial_split(data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 2: Build individual models with specified mode
models <- list(
  linear_reg = linear_reg() %>% set_engine("lm") %>% set_mode("regression"),
  tree_reg = decision_tree() %>% set_engine("rpart") %>% set_mode("regression")
)

fitted_models <- list()
for (model in names(models)) {
  fitted_models[[model]] <- models[[model]] %>%
    fit(y ~ ., data = train_data)
}

# Step 3: Implement Voting Methods
# Function to make predictions from each model
predict_models <- function(models, new_data) {
  predictions <- lapply(models, predict, new_data = new_data)
  predictions <- do.call(cbind, predictions)
  colnames(predictions) <- names(models)
  return(predictions)
}

# Average Voting
average_voting <- function(predictions) {
  rowMeans(predictions)
}

# Weighted Voting
weighted_voting <- function(predictions, weights) {
  apply(predictions * weights, 1, sum) / sum(weights)
}

# Soft Voting (using model variances as a proxy for confidence)
soft_voting <- function(predictions) {
  # This is a simplistic approach and may need refinement
  variances <- apply(predictions, 2, var)
  weights <- 1 / variances
  weighted_voting(predictions, weights)
}

# Step 4: Evaluate the Ensemble
predictions <- predict_models(fitted_models, test_data)
weights <- c(0.5, 0.5) # Equal weights for simplicity

# Applying different methods
avg_predictions <- average_voting(predictions)
weighted_predictions <- weighted_voting(predictions, weights)
soft_predictions <- soft_voting(predictions)

# Evaluate models
results <- tibble(
  actual = test_data$y,
  avg = avg_predictions,
  weighted = weighted_predictions,
  soft = soft_predictions
)

# Calculate RMSE for each method
rmse_results <- tibble(
  avg_rmse = rmse_vec(truth = results$actual, estimate = results$avg),
  weighted_rmse = rmse_vec(truth = results$actual, estimate = results$weighted),
  soft_rmse = rmse_vec(truth = results$actual, estimate = results$soft)
)

rmse_results
