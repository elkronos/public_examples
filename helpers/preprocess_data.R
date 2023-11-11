library(caret)   # For data partitioning and dummy variables
library(mice)    # For missing data imputation
library(dplyr)   # For data manipulation
library(ranger)  # For Random Forest models in feature selection
library(magrittr) # For the pipe operator `%>%`

#' Impute Missing Data in a Dataset
#'
#' This function imputes missing data in a given dataset using the MICE (Multiple Imputation by Chained Equations) package. It automatically selects the imputation method for each variable in the dataset based on its data type: 'mean' for numeric variables and 'logreg' (logistic regression) for factor variables. 
#'
#' @param data A data frame or matrix containing the data with missing values.
#' @param method The imputation method to be applied. Default is 'pmm' (predictive mean matching). This parameter is passed to the `mice` function from the MICE package. Optional.
#'
#' @importFrom mice mice
#' @importFrom mice complete
#'
#' @return A data frame or matrix with missing values imputed.
#'
#' @examples
#' \dontrun{
#'   data_with_missing <- data.frame(
#'     a = c(1, NA, 3, 4, 5),
#'     b = factor(c("yes", "no", NA, "yes", "no"))
#'   )
#'   imputed_data <- impute_missing_data(data_with_missing)
#' }
#'
#' @export
impute_missing_data <- function(data, method = 'pmm') {
  methods <- sapply(data, function(x) {
    if (is.numeric(x)) {
      return('mean')
    } else if (is.factor(x)) {
      return('logreg')
    } else {
      return('')
    }
  })
  
  imputed_data <- mice::mice(data, m=5, maxit = 50, method = methods, seed = 500)
  completed_data <- mice::complete(imputed_data, 1)
  return(completed_data)
}


#' Scale Numeric Columns of a Data Frame
#'
#' This function scales numeric columns in a given data frame using a specified method.
#' The default scaling method is standardization (z-score scaling). It is designed to work
#' with data frames where some columns are numeric and others may not be. Only numeric
#' columns are scaled.
#'
#' @param data A data frame containing the data to be scaled. This parameter is required.
#' @param method A character string specifying the scaling method. Currently supports
#'   only 'standard' for z-score standardization. Default is 'standard'.
#'
#' @return A data frame with numeric columns scaled according to the specified method.
#'
#' @importFrom stats scale
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example dataset
#'   data <- data.frame(a = 1:10, b = rnorm(10))
#'   # Scale the data
#'   scaled_data <- scale_data(data)
#' }
#'
#' @references
#' The scaling is performed using the `scale` function from the 'stats' package.
#'
scale_data <- function(data, method = 'standard') {
  numeric_columns <- sapply(data, is.numeric)
  if (method == 'standard') {
    data[numeric_columns] <- lapply(data[numeric_columns], scale)
  }
  return(data)
}


#' One-Hot Encode Data
#'
#' This function performs one-hot encoding on a given dataset. One-hot encoding 
#' is a process of converting categorical variables into a form that could be 
#' provided to machine learning algorithms to improve predictions. It uses the 
#' `dummyVars` function from the `caret` package to create dummy variables for 
#' categorical features in the dataset and then converts the result to a dataframe.
#'
#' @param data A dataframe containing the data to be one-hot encoded. The function 
#' expects this parameter to be provided, and there is no default value.
#'
#' @return A dataframe with one-hot encoded variables.
#'
#' @importFrom caret dummyVars
#' @importFrom stats predict
#' 
#' @examples
#' \dontrun{
#'   # Assuming 'my_data' is your dataframe with categorical variables
#'   encoded_data <- one_hot_encode(my_data)
#' }
#'
#' @export
one_hot_encode <- function(data) {
  dummy_model <- caret::dummyVars("~ .", data = data)
  data_encoded <- predict(dummy_model, newdata = data)
  data_encoded <- as.data.frame(data_encoded)  # Convert to dataframe
  return(data_encoded)
}


#' Remove Outliers from Numeric Columns in a Data Frame
#'
#' This function iterates over each numeric column in a given data frame and replaces outliers 
#' with NA. An outlier is defined as a value that lies beyond the interquartile range (IQR) 
#' multiplied by a specified multiplier. By default, the multiplier is set to 1.5.
#' 
#' @param data A data frame from which outliers will be removed. Required.
#' @param multiplier A numeric value used to scale the IQR to determine outliers. 
#'   Default is 1.5. Optional.
#' 
#' @return A data frame with outliers in numeric columns replaced by NA.
#' 
#' @importFrom stats quantile IQR
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(a = rnorm(100), b = rnorm(100))
#'   cleaned_data <- remove_outliers(data)
#' }
#'
#' @export
remove_outliers <- function(data, multiplier = 1.5) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      x <- data[[col]]
      qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
      H <- multiplier * IQR(x, na.rm = T)
      x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
      data[[col]] <- x
    }
  }
  return(data)
}


#' Generate Interaction Terms
#'
#' This function generates interaction terms for numeric columns in a given dataset.
#' It creates pairwise interaction terms (product of pairs of columns) for all numeric columns.
#' The degree of interaction is currently set to 2 (pairwise), and this function only
#' works for numeric columns.
#'
#' @param data A dataframe containing the data for which interaction terms are to be generated.
#' @param degree An integer specifying the degree of interaction, with a default value of 2.
#'              Currently, only a degree of 2 is implemented, implying pairwise interactions.
#'              This parameter is optional.
#' @return A dataframe with the original data and added interaction terms.
#'
#' @importFrom stats setNames
#' @importFrom base cbind
#' @importFrom utils as.data.frame
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(a = 1:3, b = 2:4, c = 3:5)
#'   result <- generate_interaction_terms(data)
#'   print(result)
#' }
generate_interaction_terms <- function(data, degree = 2) {
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  interactions <- list()
  
  if (degree >= 2) {
    for (i in 1:(length(numeric_cols) - 1)) {
      for (j in (i + 1):length(numeric_cols)) {
        new_col_name <- paste(numeric_cols[i], numeric_cols[j], sep = "_x_")
        interactions[[new_col_name]] <- data[[numeric_cols[i]]] * data[[numeric_cols[j]]]
      }
    }
  }
  
  interactions_df <- as.data.frame(interactions)
  return(cbind(data, interactions_df))
}


#' Preprocess Data for Machine Learning
#'
#' This function preprocesses a dataset for machine learning purposes. It includes partitioning the data into training and test sets, handling date variables, applying ordinal encoding, scaling, one-hot encoding, outlier removal, imputation of missing data, generation of interaction terms, and applying custom transformations.
#'
#' @param data A dataframe that contains the dataset to be preprocessed.
#' @param outcome_var A string representing the name of the outcome variable in the dataset.
#' @param partition_ratio A numeric value representing the proportion of the dataset to be used for training. Default is 0.7.
#' @param date_vars A vector of strings representing the names of date variables in the dataset. If NULL (default), no date handling is performed.
#' @param outlier_multiplier A numeric value used in outlier detection. Default is 1.5.
#' @param interaction_degree An integer indicating the degree of interaction terms to generate. Default is 2.
#' @param custom_transform An optional custom function for data transformation. If NULL (default), no custom transformation is applied.
#' @param feature_selection A logical indicating whether feature selection should be performed. Default is TRUE.
#' @param num_selected_features An integer specifying the number of features to select if feature selection is enabled. Default is 3.
#' @param ordinal_encoding A logical indicating whether to apply ordinal encoding to the outcome variable. Default is FALSE.
#' @param scale_data_flag A logical indicating whether to scale the data. Default is TRUE.
#' @param impute_method A string specifying the method for imputing missing data. Default is 'pmm'.
#' 
#' @importFrom caret createDataPartition
#' @importFrom somePackage scale_data, one_hot_encode, remove_outliers, impute_missing_data, generate_interaction_terms
#' 
#' @return A list containing the preprocessed training and test datasets.
#' 
#' @examples
#' \dontrun{
#'   # Example usage of preprocess_data
#'   processed_data <- preprocess_data(my_data, "target_variable")
#' }
#'
#' @export
preprocess_data <- function(data, outcome_var, partition_ratio = 0.7, date_vars = NULL,
                            outlier_multiplier = 1.5, interaction_degree = 2,
                            custom_transform = NULL, feature_selection = TRUE, num_selected_features = 3,
                            ordinal_encoding = FALSE, scale_data_flag = TRUE, impute_method = 'pmm') {
  
  # Error Handling: Check if data is a dataframe
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }
  
  # Error Handling: Check if outcome_var exists in data
  if (!outcome_var %in% names(data)) {
    stop("outcome_var not found in data")
  }
  
  # Error Handling: Check if date_vars exist in data
  if (!is.null(date_vars) && !all(date_vars %in% names(data))) {
    stop("Some date_vars not found in data")
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
  
  # Scale data
  if (scale_data_flag) {
    train <- scale_data(train)
  }
  
  # One-hot encode
  train <- one_hot_encode(train)
  
  # Remove outliers
  train <- remove_outliers(train, multiplier = outlier_multiplier)
  
  # Impute missing data (after outlier removal)
  train <- impute_missing_data(train, method = impute_method)
  
  # Generate Interaction Terms
  if (interaction_degree > 1) {
    train <- generate_interaction_terms(train, degree = interaction_degree)
  }
  
  # Custom transformations
  if (!is.null(custom_transform) && is.function(custom_transform)) {
    train <- custom_transform(train)
  }
  
  return(list(train = train, test = test))
}


#' Apply Transformations to Data
#'
#' This function applies various preprocessing transformations to a given dataframe. 
#' It includes scaling, imputation, outlier removal, one-hot encoding, interaction terms, 
#' and custom transformations.
#'
#' @param data A dataframe to which transformations are to be applied. 
#'             This is a required parameter.
#' @param preproc_params A list containing parameters for different transformations:
#'        - scaler_params: A list with 'center' and 'scale' for scaling numeric columns. Optional.
#'        - imputer_params: A list with 'method' for imputing missing data. Optional.
#'        - one_hot_encoder_params: An object (like a model) for one-hot encoding. Optional.
#'        - outlier_params: A list with 'multiplier' for outlier removal. Optional.
#'        - interaction_params: A list with 'degree' for generating interaction terms. Optional.
#'        - custom_transform: A custom function to be applied to the data. Optional.
#' @return A dataframe with the applied transformations.
#' @importFrom stats scale poly
#' @importFrom base lapply is.function as.vector
#' @import custom_package remove_outliers impute_missing_data predict
#' @examples
#' \dontrun{
#'   data <- data.frame(a = 1:10, b = rnorm(10))
#'   preproc_params <- list(
#'     scaler_params = list(center = TRUE, scale = TRUE),
#'     imputer_params = list(method = "median"),
#'     custom_transform = function(x) {return(x * 2)}
#'   )
#'   transformed_data <- apply_transformations(data, preproc_params)
#' }
#' @export
apply_transformations <- function(data, preproc_params) {
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
    data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], function(x) {
      as.vector(scale(x, center = scaler_params$center, scale = scaler_params$scale))
    })
  }
  
  # Remove outliers and then impute missing data
  if (!is.null(outlier_params)) {
    data <- remove_outliers(data, multiplier = outlier_params$multiplier)
    if (!is.null(imputer_params)) {
      data <- impute_missing_data(data, method = imputer_params$method)
    }
  }
  
  # Apply one-hot encoding
  if (!is.null(one_hot_encoder_params)) {
    data_encoded <- predict(one_hot_encoder_params, newdata = data)
    data_encoded <- as.data.frame(data_encoded)  # Ensure data remains a dataframe
    data <- cbind(data[!names(data) %in% names(data_encoded)], data_encoded)
  }
  
  # Interaction terms
  if (!is.null(interaction_params)) {
    num_vars <- sapply(data, is.numeric)
    data[num_vars] <- lapply(data[num_vars], function(x) {
      if (is.numeric(x)) {
        interaction_term <- poly(x, degree = interaction_params$degree, interactions = TRUE)
        return(as.vector(interaction_term))
      }
      return(x)
    })
  }
  
  # Custom transformations
  if (!is.null(custom_transform) && is.function(custom_transform)) {
    data <- custom_transform(data)
  }
  
  return(as.data.frame(data))
}


#' @examples
#' \dontrun{
#'   # Generating a Sample Dataset
#'   set.seed(0)
#'   A <- runif(100) * 10
#'   B <- sample(1:10, 100, replace = TRUE)
#'   C <- sample(c('Cat1', 'Cat2', 'Cat3', 'Cat4', 'Cat5'), 100, replace = TRUE)
#'   D <- seq(as.Date("2023-01-01"), by="day", length.out=100)
#'   E <- rnorm(100) * 50
#'   F <- sample(c(NA, 1:10), 100, replace = TRUE)
#'   Outcome <- sample(0:1, 100, replace = TRUE)
#' 
#'   df <- data.frame(A, B, C, D, E, F, Outcome)
#'   df$E[1:6] <- df$E[1:6] * 4
#'   df$A[7:11] <- NA
#' 
#'   # Checking for constant columns
#'   constant_columns <- sapply(df, function(x) length(unique(x)) == 1)
#'   if (any(constant_columns)) {
#'     warning("The dataset contains constant columns.")
#'   }
#' 
#'   # Applying Preprocessing (Assuming 'preprocess_data' function exists)
#'   processed_data <- preprocess_data(df, outcome_var = "Outcome", date_vars = c("D"))
#' 
#'   # Explore the processed data
#'   head(processed_data$train)
#'   head(processed_data$test)
#' 
#'   # Create a list of preprocessing parameters
#'   preproc_params <- list(
#'     scaler_params = list(center = TRUE, scale = TRUE),
#'     imputer_params = list(method = 'pmm'),
#'     one_hot_encoder_params = caret::dummyVars("~.", data = df),
#'     outlier_params = list(multiplier = 1.5),
#'     interaction_params = list(degree = 2),
#'     custom_transform = NULL
#'   )
#' 
#'   # Apply the transformations to a new dataset (or the test set)
#'   transformed_data <- apply_transformations(df, preproc_params)
#' 
#'   # View the transformed data
#'   head(transformed_data)
#' }