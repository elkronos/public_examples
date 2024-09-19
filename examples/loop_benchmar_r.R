# Generate a large vector of random numbers
set.seed(123)
large_vector <- rnorm(as.integer(1e7))  # 10 million random numbers

# Start the timer
start_time <- Sys.time()

# Sum using R's vectorized sum function
total <- sum(large_vector)

# End the timer
end_time <- Sys.time()

# Calculate and print the elapsed time
elapsed_time <- end_time - start_time
cat(sprintf("R vectorized summation time: %.5f seconds\n", as.numeric(elapsed_time, units = "secs")))
