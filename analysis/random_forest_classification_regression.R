#' Random Forest Model Selection
#'
#' This function builds a random forest model with optional feature selection, preprocessing, train-test split, stratification, and parallel processing.
#'
#' @param data Data frame containing the features and the target variable.
#' @param target Character specifying the name of the target variable.
#' @param type Character specifying the type of random forest model to build: "classification" or "regression".
#' @param ntree Integer specifying the number of trees to grow (default is 500).
#' @param importance Logical indicating whether variable importance should be computed (default is TRUE).
#' @param sample_size Integer specifying the sample size to use, if desired (default is NULL).
#' @param feature_select Function for feature selection, if desired (default is NULL).
#' @param preprocess Function for preprocessing, if desired (default is NULL).
#' @param n_cores Integer specifying the number of cores to use for parallel processing, if desired (default is NULL).
#' @param grid_search Logical indicating whether to use grid search for hyperparameter tuning (default is FALSE).
#' @param tune_grid Data frame specifying the hyperparameter grid for grid search, if desired (default is NULL).
#' @param train_prop Numeric between 0 and 1 specifying the proportion of data to use for training, if desired (default is NULL).
#' @param stratify Character specifying the name of the variable to use for stratified sampling, if desired (default is NULL).
#' @return A vector of predictions for the test data.
#' @importFrom randomForest randomForest
#' @importFrom caret train createDataPartition trainControl
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach %dopar% foreach
#' @examples
#' # Create a fake dataset with classification and regression targets
#' set.seed(42)
#' data <- data.frame(feature1 = rnorm(100),
#'                    feature2 = rnorm(100),
#'                    feature3 = rnorm(100),
#'                    class_target = factor(sample(c("Class1", "Class2"), 100, replace = TRUE)),
#'                    reg_target = rnorm(100))
#'
#' # Example use of rf_select for classification without additional features
#' result_class_simple <- rf_select(
#'   data = data, 
#'   target = "class_target", 
#'   type = "classification", 
#'   ntree = 500
#' )
#'
#' # Example use of rf_select for regression without additional features
#' result_reg_simple <- rf_select(
#'   data = data, 
#'   target = "reg_target", 
#'   type = "regression", 
#'   ntree = 500
#' )
#'
#' # Example use of rf_select for classification with train-test split, stratification, and parallel processing
#' result_class_complex <- rf_select(
#'   data = data, 
#'   target = "class_target", 
#'   type = "classification", 
#'   ntree = 500, 
#'   train_prop = 0.8,
#'   stratify = "class_target",
#'   n_cores = 4
#' )
#'
#' # Example use of rf_select for regression with train-test split and parallel processing
#' result_reg_complex <- rf_select(
#'   data = data, 
#'   target = "reg_target", 
#'   type = "regression", 
#'   ntree = 500, 
#'   train_prop = 0.8,
#'   n_cores = 4
#' )
#'
#' # Print results
#' print(head(result_class_simple))
#' print(head(result_reg_simple))
#' print(head(result_class_complex))
#' print(head(result_reg_complex))

# Load packages
library(caret)
library(doParallel)
library(randomForest)

# Save random forest function
rf_select <- function(data, target, type, ntree = 500, importance = TRUE, 
                      sample_size = NULL, feature_select = NULL, 
                      preprocess = NULL, n_cores = NULL, grid_search = FALSE,
                      tune_grid = NULL, train_prop = NULL, stratify = NULL) {
  
  # Get the index of the target variable in the data
  target_idx <- match(target, names(data))
  
  # Convert the target variable to a factor if classification is selected
  if(type == "classification") {
    data[[target]] <- factor(data[[target]])
  }
  
  # Apply preprocessing and feature selection, if specified
  if (!is.null(preprocess) || !is.null(feature_select)) {
    data <- preprocess(data) %>% feature_select()
  }
  
  # Sample data, if specified
  if(!is.null(sample_size)) {
    data <- data %>% sample_n(sample_size)
  }
  
  # Train-test split, if specified
  if(!is.null(train_prop)) {
    if (type == "classification" && !is.null(stratify)) {
      train_idx <- createDataPartition(data[[stratify]], p = train_prop, list = FALSE)
    } else {
      train_idx <- sample(1:nrow(data), floor(train_prop * nrow(data)))
    }
    train_data <- data[train_idx, ]
    test_data <- data[-train_idx, ]
  } else {
    train_data <- data
    test_data <- data
  }
  
  # Grid search mode
  if(grid_search) {
    if(is.null(tune_grid)) {
      stop("Provide a tune_grid dataframe for grid search")
    }
    
    # Define train control for grid search
    train_control <- trainControl(method = "cv", number = 10,
                                  allowParallel = !is.null(n_cores))
    
    # Register parallel backend, if specified
    if (!is.null(n_cores)) {
      registerDoParallel(cores = n_cores)
    }
    
    # Train the model using grid search
    rf <- train(x = train_data[, -target_idx],
                y = train_data[[target]],
                method = "rf",
                metric = ifelse(type == "classification", "Accuracy", "RMSE"),
                tuneGrid = tune_grid,
                trControl = train_control,
                importance = importance,
                ntree = ntree)
    
    # Stop the parallel backend, if specified
    if (!is.null(n_cores)) {
      stopImplicitCluster()
    }
    
    # Default mode
  } else {
    # Parallel mode, if specified
    if(!is.null(n_cores)) {
      registerDoParallel(cores = n_cores)
      rf <- foreach(ntree = rep(floor(ntree/n_cores), n_cores), .combine = combine) %dopar% {
        randomForest::randomForest(x = train_data[, -target_idx],
                                   y = train_data[[target]],
                                   ntree = ntree, importance = importance)
      }
      stopImplicitCluster()
      # Single-core mode
    } else {
      rf <- randomForest::randomForest(x = train_data[, -target_idx],
                                       y = train_data[[target]],
                                       ntree = ntree, importance = importance)
    }
  }
  
  # Make predictions and return results
  if (type == "classification") {
    return(predict(rf, newdata = test_data, type = "class"))
  } else if (type == "regression") {
    return(predict(rf, newdata = test_data))
  } else {
    stop("Invalid type parameter. Choose 'classification' or 'regression'")
  }
}