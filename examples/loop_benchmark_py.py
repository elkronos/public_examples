import time
import numpy as np

# Generate a large array of random numbers using NumPy
large_array = np.random.randn(int(1e7))  # 10 million random numbers

# Start the timer
start_time = time.time()

# Sum using a vectorized operation in NumPy
total = np.sum(large_array)

# End the timer
end_time = time.time()

# Calculate and print the elapsed time
elapsed_time = end_time - start_time
print(f"Python (NumPy) summation time: {elapsed_time:.5f} seconds")
