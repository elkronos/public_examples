# SMOTE
# -----------------------------------------------------
# This implementation requires the 'FNN' package. If not installed, please install it using:
# install.packages("FNN")
if (!requireNamespace("FNN", quietly = TRUE)) {
  stop("Please install the 'FNN' package: install.packages('FNN')")
}

# Validate Input and Identify Minority Class
validate_smote_input <- function(data, target) {
  if (!target %in% names(data)) {
    stop("Target variable not found in the data.")
  }
  # Identify the minority class based on frequency.
  target_counts <- table(data[[target]])
  minority_class <- names(which.min(target_counts))
  return(minority_class)
}

# Compute k–Nearest Neighbors for Minority Samples
compute_knn <- function(minority_features, k) {
  # Uses FNN::get.knn for efficient neighbor search.
  knn_result <- FNN::get.knn(minority_features, k = k)
  return(knn_result$nn.index)
}

# Generate a Single Synthetic Sample
generate_synthetic_sample <- function(sample_instance, neighbor_instance) {
  # Interpolate between the sample and a neighbor using a random gap.
  gap <- runif(1)
  synthetic_feature <- sample_instance + gap * (neighbor_instance - sample_instance)
  return(synthetic_feature)
}

# Generate Synthetic Samples for the Entire Minority Set
generate_synthetic_samples <- function(minority_features, neighbor_indices, perc_over, minority_class) {
  N <- nrow(minority_features)
  # Determine the number of synthetic samples per instance.
  rep_count <- perc_over %/% 100          # Whole number of replications per instance.
  remainder <- (perc_over / 100) - rep_count  # Fractional extra samples per instance.
  total_samples <- rep_count * N + round(remainder * N)
  
  synthetic_samples <- vector("list", total_samples)
  count <- 1
  
  # Generate rep_count synthetic samples for each instance.
  for (i in 1:N) {
    for (j in 1:rep_count) {
      neighbor_index <- sample(neighbor_indices[i, ], 1)
      synthetic_features <- generate_synthetic_sample(minority_features[i, ], 
                                                      minority_features[neighbor_index, ])
      synthetic_samples[[count]] <- c(synthetic_features, minority_class)
      count <- count + 1
    }
  }
  
  # Handle any extra fractional samples.
  extra_samples_needed <- round(remainder * N)
  if (extra_samples_needed > 0) {
    extra_indices <- sample(1:N, extra_samples_needed)
    for (i in extra_indices) {
      neighbor_index <- sample(neighbor_indices[i, ], 1)
      synthetic_features <- generate_synthetic_sample(minority_features[i, ], 
                                                      minority_features[neighbor_index, ])
      synthetic_samples[[count]] <- c(synthetic_features, minority_class)
      count <- count + 1
    }
  }
  
  return(synthetic_samples)
}

# Main
smote <- function(data, target, perc_over = 100, k = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Validate input and determine the minority class.
  minority_class <- validate_smote_input(data, target)
  
  # Extract minority class data and the feature columns.
  minority_data <- data[data[[target]] == minority_class, ]
  feature_cols <- setdiff(names(minority_data), target)
  
  # Ensure features are numeric.
  minority_features <- minority_data[, feature_cols, drop = FALSE]
  if (!all(sapply(minority_features, is.numeric))) {
    stop("All features must be numeric. Preprocess non-numeric variables (e.g., one-hot encoding) before applying SMOTE.")
  }
  minority_features <- as.matrix(minority_features)
  
  # Compute the k–nearest neighbors for minority instances.
  neighbor_indices <- compute_knn(minority_features, k)
  
  # Generate synthetic samples.
  synthetic_list <- generate_synthetic_samples(minority_features, neighbor_indices, perc_over, minority_class)
  synthetic_matrix <- do.call(rbind, synthetic_list)
  synthetic_df <- as.data.frame(synthetic_matrix, stringsAsFactors = FALSE)
  
  # Assign proper column names.
  colnames(synthetic_df) <- c(feature_cols, target)
  
  # Convert feature columns to numeric.
  for (col in feature_cols) {
    synthetic_df[[col]] <- as.numeric(synthetic_df[[col]])
  }
  
  # Convert the target column to factor.
  synthetic_df[[target]] <- as.factor(synthetic_df[[target]])
  
  # Combine the original data with the synthetic samples.
  new_data <- rbind(data, synthetic_df)
  
  return(new_data)
}

# -----------------------------------------------------
# Example Usage:
# Assuming your data frame 'df' has a target column named "Class",
# the following command oversamples the minority class by 200%:
#
# new_df <- smote(df, target = "Class", perc_over = 200, k = 5, seed = 123)
