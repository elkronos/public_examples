# Load necessary libraries
library(tweedie)
library(glm2)
library(DHARMa)
library(car)
library(boot)
library(ggplot2)
library(cvTools)
library(statmod)

# Simulating synthetic data
set.seed(123)
n <- 200
x1 <- runif(n, 0, 10)
x2 <- rnorm(n, 5, 2)
mu <- exp(1 + 0.5 * x1 - 0.3 * x2)
y <- rtweedie(n, mu=mu, phi=2, power=1.5)
data <- data.frame(y, x1, x2)

# Display data summaries and plots
hist(y, main="Synthetic Tweedie Distributed Data", col="skyblue", border="black")
par(mfrow=c(1,2))
plot(x1, y, main="X1 vs Y")
plot(x2, y, main="X2 vs Y")

#' Grid Search Function for var.power and link.power
#'
#' This function performs grid search to find the optimal parameters for the tweedie regression model.
#' It returns the set of parameters which minimize the AIC.
#'
#' @param data A data frame containing the predictors and response variable.
#' @return A row from the grid with the optimal var.power and link.power values.
#' @examples
#' optimal_parameters(data)
optimal_parameters <- function(data) {
  grid <- expand.grid(var.power = seq(1.05, 1.95, by=0.05), link.power = c(-1, -0.5, 0, 0.5, 1))
  grid$AIC <- sapply(1:nrow(grid), function(i) {
    tryCatch({
      model <- glm2(y ~ x1 + x2, 
                    family = tweedie(var.power = grid$var.power[i], link.power = grid$link.power[i]), 
                    data = data,
                    start = coef(glm(y ~ x1 + x2, data = data, family = gaussian())))
      AIC(model)
    }, error=function(e) return(Inf))
  })
  grid[which.min(grid$AIC), ]
}

#' Cross-validation for Tweedie Regression Model
#'
#' This function performs K-fold cross-validation to estimate the mean squared error of the tweedie regression model.
#'
#' @param data A data frame containing the predictors and response variable.
#' @param K The number of folds for cross-validation. Default is 5.
#' @return The mean squared error for the K-fold cross-validation.
#' @examples
#' cv_tweedie(data)
cv_tweedie <- function(data, K=5) {
  folds <- cvFolds(nrow(data), K=K)
  MSE_list <- sapply(1:K, function(i) {
    train <- data[-folds$subsets[[i]],]
    test <- data[folds$subsets[[i]],]
    params <- optimal_parameters(train)
    model <- glm2(y ~ x1 + x2, family=tweedie(var.power=params$var.power, link.power=params$link.power), data=train)
    preds <- predict(model, newdata=test, type="response")
    mean((preds - test$y)^2)
  })
  mean(MSE_list)
}

# Fit the model
opt_params <- optimal_parameters(data)
model <- glm2(y ~ x1 + x2, family=tweedie(var.power=opt_params$var.power, link.power=opt_params$link.power), data=data)

# Diagnostics
residuals <- residuals(model, type="deviance")
par(mfrow=c(1,2))
plot(predict(model), residuals, main="Residuals vs Fitted", ylab="Residuals")
abline(h=0, col="red")
hist(residuals, breaks=30, main="Histogram of Residuals", col="skyblue")
qqPlot(residuals, main="QQ Plot of Residuals")

# Define the coefficients from the model trained on the full dataset
starting_values <- coef(model)

# Bootstrap Resampling with starting values
boot_data <- boot(data=data, statistic=function(data, indices) {
  sample_data <- data[indices, ]
  model_bootstrap <- tryCatch({
    glm2(y ~ x1 + x2, 
         family=tweedie(var.power=opt_params$var.power, link.power=opt_params$link.power), 
         data=sample_data,
         start=starting_values)
  }, error=function(e) return(NULL))
  
  # If the model failed, return NA values for each coefficient
  if (is.null(model_bootstrap)) {
    return(rep(NA, length(starting_values)))
  } else {
    return(coef(model_bootstrap))
  }
}, R=1000)

# Display results
print(boot_data)

# Check linearity
scatterplotMatrix(~y + x1 + x2, data=data, main="Scatterplot Matrix")
