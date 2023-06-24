# Load packages
library(dplyr)
library(caret)
library(mice)
library(lubridate)
library(ranger)

#' Preprocess Data for Machine Learning
#'
#' This function performs various data pre-processing steps to prepare the data for machine learning tasks.
#' This function is suitable for various machine learning models, including linear regression, logistic regression,
#' decision trees, random forests, gradient boosting, support vector machines, and neural networks.
#'
#' @param data The input data frame.
#' @param outcome_var The name of the outcome variable.
#' @param partition_ratio The ratio to partition the data into training and testing sets (default is 0.7).
#' @param date_vars Vector of date variable names in the data frame (default is NULL).
#' @param outlier_multiplier The multiplier to define outliers (default is 1.5).
#' @param interaction_degree The degree of interaction terms to create (default is 2).
#' @param custom_transform A custom transformation function to apply to the data (default is NULL).
#' @param feature_selection Logical indicating whether to perform feature selection (default is TRUE).
#' @param num_selected_features The number of top features to select (default is 3).
#' @param ordinal_encoding Logical indicating whether to perform ordinal encoding (default is FALSE).
#' @param scale_data_flag Logical indicating whether to scale data (default is TRUE).
#' @param impute_method Method to be used for data imputation (default is 'pmm').
#'
#' @return A list containing the preprocessed training dataset, the testing dataset, and a table of feature importances.
#'
#' @details This function performs the following preprocessing steps:
#' \itemize{
#'   \item Partition the data into training and testing sets.
#'   \item Convert date variables to POSIXct format.
#'   \item Impute missing data using the MICE package.
#'   \item Scale numeric variables to have mean 0 and standard deviation 1.
#'   \item One-hot encode categorical variables.
#'   \item Remove outliers from numeric variables using the IQR method.
#'   \item Create interaction terms for numeric variables.
#'   \item Apply a custom transformation function, if provided.
#'   \item Perform feature selection using the Random Forest importance measure.
#' }
#'
#' @importFrom caret createDataPartition dummyVars
#' @importFrom dplyr select all_of
#' @importFrom lubridate as.POSIXct
#' @importFrom mice mice complete
#' @importFrom ranger ranger importance
#' @importFrom stats IQR scale
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' library(caret)
#' library(mice)
#' library(lubridate)
#' library(randomForest)
#' library(ranger)
#' 
#' # Set seed for reproducibility
#' set.seed(123)
#' 
#' # Generate example data
#' data <- data.frame(
#'   outcome_var = sample(0:1, 1000, replace = TRUE),
#'   num_var1 = rnorm(1000),
#'   num_var2 = rnorm(1000, mean = 2),
#'   cat_var1 = sample(letters[1:3], 1000, replace = TRUE),
#'   cat_var2 = sample(LETTERS[1:3], 1000, replace = TRUE),
#'   date_var = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 1000, replace = TRUE)
#' )
#' 
#' # Introduce some missing values
#' data[sample(1:nrow(data), 50), "num_var1"] <- NA
#' data[sample(1:nrow(data), 50), "num_var2"] <- NA
#' data[sample(1:nrow(data), 50), "cat_var1"] <- NA
#' data[sample(1:nrow(data), 50), "cat_var2"] <- NA
#' 
#' # Call preprocess_data function
#' preprocessed_data <- preprocess_data(data, "outcome_var", date_vars = "date_var")
#' }
#' 
#' @export
preprocess_data <- function(data, outcome_var, partition_ratio = 0.7, date_vars = NULL,
                            outlier_multiplier = 1.5, interaction_degree = 2,
                            custom_transform = NULL, feature_selection = TRUE, num_selected_features = 3,
                            ordinal_encoding = FALSE, scale_data_flag = TRUE, impute_method = 'pmm'){
  
  # Check if data is a dataframe
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }
  
  # Check if outcome_var exists in data
  if (!outcome_var %in% names(data)) {
    stop("outcome_var not found in data")
  }
  
  # Check if date_vars exist in data
  if (!is.null(date_vars) && !all(date_vars %in% names(data))) {
    stop("Some date_vars not found in data")
  }
  
  if (num_selected_features < 1 || num_selected_features > (ncol(data)-1)) {
    stop("num_selected_features must be between 1 and the number of variables in data excluding the outcome variable")
  }
  
  if (partition_ratio <= 0 || partition_ratio >= 1) {
    stop("partition_ratio must be between 0 and 1")
  }
  
  if (!is.null(custom_transform) && !is.function(custom_transform)) {
    stop("custom_transform must be a function")
  }
  
  if (ordinal_encoding && !is.factor(data[[outcome_var]])) {
    stop("outcome_var must be a factor for ordinal encoding")
  }
  
  # Apply ordinal encoding if necessary
  if (ordinal_encoding) {
    levels <- unique(data[[outcome_var]])
    ordinal_levels <- 1:length(levels)
    names(ordinal_levels) <- levels
    data[[outcome_var]] <- ordinal_levels[data[[outcome_var]]]
  }
  
  # Partition data
  trainIndex <- caret::createDataPartition(data[[outcome_var]], p = partition_ratio, list = FALSE, times = 1)
  train <- data[trainIndex, ]
  test <- data[-trainIndex, ]
  
  # Handle datetime variables
  if (!is.null(date_vars)) {
    for (var in date_vars) {
      train[[var]] <- as.POSIXct(train[[var]], format = "%Y-%m-%d")
      test[[var]] <- as.POSIXct(test[[var]], format = "%Y-%m-%d")
    }
  }
  
  # Impute missing data
  impute_missing_data <- function(data, method){
    imputed_data <- mice::mice(data, m=5, maxit = 50, method = method, seed = 500)
    completed_data <- mice::complete(imputed_data,1)
    return(completed_data)
  }
  
  train <- impute_missing_data(train, method = impute_method)
  
  # Scale data
  scale_data <- function(data){
    numeric_columns <- sapply(data, is.numeric)
    data[numeric_columns] <- lapply(data[numeric_columns], scale)
    return(data)
  }
  
  if(scale_data_flag){
    train <- scale_data(train)
  }
  
  # One-hot encode
  one_hot_encode <- function(data){
    data <- caret::dummyVars("~.", data = data, fullRank = TRUE) %>% predict(data)
    return(data)
  }
  
  train <- one_hot_encode(train)
  
  # Remove outliers
  remove_outliers <- function(data, multiplier = 1.5){
    for (col in names(data)){
      if(is.numeric(data[[col]])){
        x <- data[[col]]
        qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
        H <- multiplier * IQR(x, na.rm = T)
        x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
        data[[col]] <- x
      }
    }
    data <- impute_missing_data(data, method = impute_method)
    return(data)
  }
  
  train <- remove_outliers(train, multiplier = outlier_multiplier)
  
  # Interaction terms
  create_interaction_terms <- function(data, degree = 2){
    num_vars <- sapply(data, is.numeric)
    data[num_vars] <- lapply(data[num_vars], function(x) {
      if(is.numeric(x)) return(poly(x, degree = degree, interactions = TRUE))
      return(x)
    })
    return(data)
  }
  
  train <- create_interaction_terms(train, degree = interaction_degree)
  
  # Custom transformations
  if (!is.null(custom_transform) && is.function(custom_transform)) {
    train <- custom_transform(train)
  }
  
  importance_table <- NULL
  if (feature_selection) {
    set.seed(123)
    
    # Check if outcome_var is numeric or factor
    if (is.numeric(train[[outcome_var]])) {
      rf_model <- ranger::ranger(as.formula(paste(outcome_var, "~ .")), data = train, importance = 'impurity')
    } else {
      rf_model <- ranger::ranger(as.formula(paste(outcome_var, "~ .")), data = train, importance = 'permutation')
    }
    
    
    importance_vector <- ranger::importance(rf_model)
    importance_table <- data.frame(Variable = names(importance_vector), Importance = importance_vector)
    top_features <- importance_table$Variable[order(importance_table$Importance, decreasing = TRUE)][1:num_selected_features]
    train <- dplyr::select(train, dplyr::all_of(c(outcome_var, top_features)))
  }
  
  return(list(train = train, test = test, importance_table = importance_table))
}

#' Apply Preprocessing Transformations to Test Data
#'
#' This function applies a range of preprocessing transformations to a testing data set.
#' The transformations are defined by the parameters that are generated from a previous preprocessing of the training data.
#' This ensures that the same transformations are applied to both training and testing data.
#'
#' @param data The testing data frame on which the transformations are to be applied.
#'
#' @param preproc_params A list containing all the preprocessing parameters used on the training set. This includes:
#'   \itemize{
#'     \item \strong{scaler_params}: A list that includes 'center' and 'scale' parameters from the previous feature scaling step. These parameters are used to standardize numeric variables in the testing set.
#'     \item \strong{imputer_params}: A MICE 'mids' object that contains the imputation model fitted on the training data. This is used to fill missing values in the testing set.
#'     \item \strong{one_hot_encoder_params}: An object from caret's 'dummyVars' function or similar, which defines how to perform one-hot encoding on categorical variables in the testing set.
#'     \item \strong{outlier_params}: A list with a named list for each numeric variable, where each named list contains the 'quantiles' and 'H' (IQR multiplier) used to identify and remove outliers in the testing data.
#'     \item \strong{interaction_params}: A list with the 'degree' of the polynomial for creating interaction terms.
#'     \item \strong{custom_transform}: A custom function for data transformation if any was applied to the training data.
#'   }
#'
#' @return A data frame that has had all the specified transformations applied.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(testdata)
#' preproc_params <- list(scaler_params = list(center = TRUE, scale = TRUE),
#'                        imputer_params = mice::mice(testdata, m = 5, method = 'pmm', seed = 500),
#'                        one_hot_encoder_params = caret::dummyVars(~., data = testdata),
#'                        outlier_params = lapply(testdata, function(x) list(quantiles = quantile(x, c(0.25, 0.75)), H = 1.5 * IQR(x))),
#'                        interaction_params = list(degree = 2),
#'                        custom_transform = NULL)
#' transformed_data <- apply_transformations(testdata, preproc_params)
#' }
#' @export
apply_transformations <- function(data, preproc_params){
  
  # Extract parameters
  scaler_params <- preproc_params$scaler_params
  imputer_params <- preproc_params$imputer_params
  one_hot_encoder_params <- preproc_params$one_hot_encoder_params
  outlier_params <- preproc_params$outlier_params
  interaction_params <- preproc_params$interaction_params
  custom_transform <- preproc_params$custom_transform
  
  # Check if data is a dataframe
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }
  
  # Scale data
  if (!is.null(scaler_params)) {
    data[sapply(data, is.numeric)] <- Map(scale, data[sapply(data, is.numeric)], center = scaler_params$center, scale = scaler_params$scale)
  }
  
  # Impute missing data
  if (!is.null(imputer_params)) {
    data <- mice::complete(imputer_params, newdata = data)
  }
  
  # Apply one-hot encoding
  if (!is.null(one_hot_encoder_params)) {
    data <- predict(one_hot_encoder_params, newdata = data)
  }
  
  # Remove outliers
  if (!is.null(outlier_params)) {
    for (col in names(data)){
      if(is.numeric(data[[col]])){
        x <- data[[col]]
        qnt <- outlier_params[[col]]$quantiles
        H <- outlier_params[[col]]$H
        x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
        data[[col]] <- x
      }
    }
    data <- impute_missing_data(data, method = impute_method) # impute method should be passed or defined
  }
  
  # Interaction terms
  if (!is.null(interaction_params)) {
    num_vars <- sapply(data, is.numeric)
    data[num_vars] <- lapply(data[num_vars], function(x) {
      if(is.numeric(x)) return(poly(x, degree = interaction_params$degree, interactions = TRUE))
      return(x)
    })
  }
  
  # Custom transformations
  if (!is.null(custom_transform) && is.function(custom_transform)) {
    data <- custom_transform(data)
  }
  
  return(data)
}