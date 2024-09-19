# Load necessary packages
library(microbenchmark)
library(moments)  # For skewness and kurtosis calculation

# Create a large matrix of random numbers (1000 columns, 1 million rows)
set.seed(123)
large_matrix <- matrix(rnorm(1e7), ncol = 1000)  # 1 million rows, 1,000 columns

# Complex operation: normalize, transform, and compute statistics
complex_operation <- function(x) {
  # Step 1: Normalize the data (mean = 0, sd = 1)
  normalized_x <- scale(x)
  
  # Step 2: Apply an exponential transformation
  transformed_x <- exp(normalized_x)
  
  # Step 3: Compute various statistics
  c(sum = sum(transformed_x), 
    mean = mean(transformed_x), 
    sd = sd(transformed_x), 
    skewness = skewness(transformed_x), 
    kurtosis = kurtosis(transformed_x))
}

# Using apply to perform the complex operation
apply_benchmark <- microbenchmark({
  result_apply <- t(apply(large_matrix, 2, complex_operation))  # Transpose to match matrix structure
}, times = 10)

# Using a for loop to perform the complex operation
loop_benchmark <- microbenchmark({
  result_for <- matrix(NA, nrow = 5, ncol = ncol(large_matrix))  # Initialize a matrix with 5 rows for 5 statistics
  for (i in 1:ncol(large_matrix)) {
    result_for[, i] <- complex_operation(large_matrix[, i])
  }
  result_for <- t(result_for)  # Transpose to match structure of apply output
}, times = 10)

# Strip attributes (such as names or dimnames) to ensure a fair comparison
attributes(result_apply) <- NULL
attributes(result_for) <- NULL

# Display benchmark results
print(apply_benchmark)
print(loop_benchmark)

# Ensure the results are the same
identical_result <- all.equal(result_apply, result_for)
print(identical_result)  # Should return TRUE
