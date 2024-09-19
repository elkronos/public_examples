# Create a large matrix of random numbers
set.seed(123)
large_matrix <- matrix(rnorm(1e7), ncol = 1000)

# Using apply to sum columns
system.time({
  result_apply <- apply(large_matrix, 2, sum)
})

# Using a for loop to sum columns
system.time({
  result_for <- numeric(ncol(large_matrix)) # Initialize a vector to store results
  for (i in 1:ncol(large_matrix)) {
    result_for[i] <- sum(large_matrix[, i])
  }
})

# Ensure the results are the same
all.equal(result_apply, result_for)
