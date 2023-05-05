set.seed(3141) # for reproducibility

# Q-learning update function
update <- function(i, r) {
  Q[i] <<- Q[i] + 1/(k[i]+1) * (r-Q[i]) # Q-learning function
  k[i] <<- k[i] + 1 # one more game played on the i'th bandit
}

# Simulate game on one-armed bandit i
ret <- function(i) {
  round(rnorm(1, mean = rets[i]))
}

# Choose which bandit to play
which.bandit <- function() {
  p <- runif(1)
  ifelse(p >= epsilon, which.max(Q), sample(1:n, 1))
}

epsilon <- 0.1 # switch in epsilon percent of cases
rets <- c(4, 5, 4, 4, 4) # average returns of bandits
n <- length(rets)
Q <- rep(0, n) # initialize return vector
k <- rep(0, n) # initialize vector for games played on each bandit
N <- 1000 # number of runs
R <- 0 # sum of returns

for (j in 1:N) {
  i <- which.bandit() # choose bandit
  r <- ret(i) # simulate bandit
  R <- R + r # add return of bandit to overall sum of returns
  update(i, r) # calling Q-learning update function
}

set.seed(1234) # for reproducibility, changed to 1234

# Load packages
library(neuralnet)
library(NeuralNetTools)

# Normalize the input data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalized_i <- normalize(1:n)
normalized_Q <- normalize(Q)

# Train the neural network with the normalized data
nn <- neuralnet(Q ~ i, data = data.frame(i = normalized_i, Q = normalized_Q), hidden = c(25, 25, 25), linear.output = TRUE, lifesign = "none", algorithm = "rprop+")

# Compute predictions
predictions <- compute(nn, data.frame(i = normalized_i))

predicted_Q <- predictions$net.result

# Denormalize the predicted Q values
denormalize <- function(x, original_data) {
  return(x * (max(original_data) - min(original_data)) + min(original_data))
}

denormalized_predicted_Q <- denormalize(predicted_Q, Q)

# Calculate evaluation metrics
MAE <- mean(abs(denormalized_predicted_Q - Q))
MSE <- mean((denormalized_predicted_Q - Q)^2)
RMSE <- sqrt(MSE)

cat("MAE:", MAE, "\n")
cat("MSE:", MSE, "\n")
cat("RMSE:", RMSE, "\n")
