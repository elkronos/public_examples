library(recipes)
library(modeldata)
library(rsample)

# Load example data
data(ames)

# Split into training and testing
split <- initial_split(ames, prop = .75) # 75% train, 25% test
train_data <- training(split)
test_data <- testing(split)

# Create recipe and train
rec <- recipe(Sale_Price ~ ., data = train_data) %>%
  # Remove # predictors with R > .89
  step_corr(all_numeric_predictors(), threshold = .89) %>%
  # Remove predictors with 0 variance
  step_nzv(all_numeric_predictors()) %>%
  # Impute missing values using the median
  step_impute_median(all_numeric_predictors()) %>%
  # Re-scale all data using mean centering
  step_scale(all_numeric_predictors()) %>%
  # Convert nominal variables to 1/0 dummy codes
  step_dummy(all_nominal_predictors()) %>%
  prep()

# Apply recipe to the test data
prep_data <- bake(rec, new_data = test_data)

# Review
dim(prep_data)

################## Now you can do your models like elastic net and MARs
