set.seed(3141) # for reproducibility

# Q-learning update function
update <- function(i, r) {
  Q[i] <<- Q[i] + 1/(k[i]+1) * (r-Q[i]) # Q-learning function
  k[i] <<- k[i] + 1 # one more game played on i'th bandit
}

# simulate game on one-armed bandit i
ret <- function(i) {
  round(rnorm(1, mean = rets[i]))
}

# chose which bandit to play
which.bandit <- function() {
  p <- runif(1)
  Q <- predict.Q(1:n)
  if (p >= epsilon) {
    return(which.max(Q))
  } else {
    return(sample(1:n, 1))
  }
}

epsilon <- 0.1 # switch in epsilon percent of cases
rets <- c(4, 5, 4, 4, 4) # average returns of bandits
n <- length(rets)
k <- rep(0, n) # initialize vector for games played on each bandit
N <- 1000 # no. of runs
R <- 0 # sum of returns

library(neuralnet)
set.seed(271) # for reproducibility

epsilon <- 0.1 # switch in epsilon percent of cases
rets <- c(4, 5, 4, 4, 4) # average returns of bandits
n <- length(rets)
k <- rep(0, n) # initialize vector for games played on each bandit
N <- 10000 # no. of runs
R <- 0 # sum of returns

# Neural network model
nn <- neuralnet(Q ~ i, data = data.frame(i = 1:n, Q = rep(0, n)), hidden = c(100), linear.output = TRUE, lifesign = "none", algorithm = "rprop+")

# Predict Q-values using the neural network
predict.Q <- function(i) {
  predict(nn, data.frame(i = i))
}
Q <- predict.Q(1:n) # initialize return vector

# Update the neural network using backpropagation
update <- function(i, r) {
  y <- Q[i] + 1/(k[i]+1) * (r-Q[i]) # Q-learning function
  k[i] <<- k[i] + 1 # one more game played on the i'th bandit
  
  # Update the neural network weights less frequently
  if (j %% 10 == 0) {
    nn <<- neuralnet(Q ~ i, data = data.frame(i = 1:n, Q = c(Q[-i], y)), hidden = c(50), linear.output = TRUE, startweights = nn$weights, lifesign = "none", algorithm = "rprop+")
  }
}

for (j in 1:N) {
  i <- which.bandit() # chose bandit
  r <- ret(i) # simulate bandit
  R <- R + r # add return of bandit to overall sum of returns
  update(i, r) # calling Q-learning update function
}

which.max(Q)

k

N * max(rets)

R

R / (N * max(rets))

N * mean(rets) 

(R - N * mean(rets)) / (N * mean(rets))
