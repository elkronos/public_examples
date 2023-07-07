#' Calculate Model Statistics
#'
#' The function calculates and returns a variety of statistics for each model object (of class 'lm' or 'glm') in a list of models. 
#' These statistics include measures of fit (e.g., MSE, RMSE, MAE, R squared, Adjusted R squared), AIC, BIC, tests for 
#' heteroscedasticity, autocorrelation, normality of residuals, and descriptive statistics for residuals. It also provides 
#' the opportunity to back-transform predictions and actual values based on a specified transformation ("log" or "sqrt").
#'
#' @param models A named list of models, where the name of each model will be used as the model_name in the resulting dataframe. 
#'               Each model should be a linear model object (created using \code{lm()} or \code{glm()} in R).
#' @param transformation A character string specifying the transformation applied to the response variable during model fitting. 
#'                       This is used to back-transform the predicted and actual response variables. Can be "log" or "sqrt". 
#'                       If \code{NULL} (the default), no back-transformation is performed. If an unsupported transformation 
#'                       is provided, the function will stop with an error message.
#'
#' @return A dataframe where each row corresponds to a different model in the input list, and the columns correspond to different 
#'         calculated statistics for each model.
#'
#' @details This function requires the following packages: \code{broom} (for the \code{tidy} function) and \code{car} (for the 
#'           \code{Anova} function). These functions are used to calculate specific model statistics.
#'
#' @examples
#' df1 <- data.frame(y1 = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' df2 <- data.frame(y2 = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' model1 <- lm(y1 ~ x1 + x2, data = df1)
#' model2 <- glm(y2 ~ x1 + x2, data = df2, family = gaussian)
#' models <- list("Model 1" = model1, "Model 2" = model2)
#' stats <- calculate_model_stats(models)
#' print(stats)
#'
#' @seealso
#' \code{\link[stats]{lm}}, \code{\link[lmtest]{bptest}}, \code{\link[lmtest]{durbinWatsonTest}}, \code{\link[broom]{tidy}}, 
#' \code{\link[car]{Anova}}
#'
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom stats AIC BIC
#'
#' @export
calculate_model_stats <- function(models, transformation = NULL) {
  require(broom)
  require(car)
  
  model_stats <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    
    actual <- model$model[, 1]
    predicted <- predict(model)
    
    # If transformation is provided, back-transform actual and predicted
    if (!is.null(transformation)) {
      switch(
        transformation,
        "log" = {
          actual <- exp(actual)
          predicted <- exp(predicted)
        },
        "sqrt" = {
          actual <- actual^2
          predicted <- predicted^2
        },
        stop("Unsupported transformation")
      )
    }
    
    # Get residuals and calculate metrics
    residuals <- resid(model)
    n <- length(actual)
    p <- length(coef(model))
    
    mse <- mean(residuals^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(residuals))
    r_squared <- summary(model)$r.squared
    adjusted_r_squared <- summary(model)$adj.r.squared
    
    # Check for negative values in actual and predicted before calculating MSLE
    msle <- if (any(actual <= -1) | any(predicted <= -1)) {
      warning <- "Warning: Cannot calculate MSLE because the actual or predicted values contain negative numbers."
      NA
    } else {
      warning <- "No warnings."
      mean((log1p(predicted) - log1p(actual))^2)
    }
    
    # Test for heteroscedasticity
    bp <- bptest(model)
    heteroscedasticity_p_value <- bp$p.value
    
    # Test for autocorrelation
    dw <- durbinWatsonTest(model)
    autocorrelation_p_value <- if (is.null(dw$p.value)) NA else dw$p.value
    
    # Test for normality of residuals
    sw <- shapiro.test(residuals)
    normality_p_value <- sw$p.value
    
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Descriptive statistics for residuals
    residuals_mean <- mean(residuals)
    residuals_median <- median(residuals)
    residuals_min <- min(residuals)
    residuals_max <- max(residuals)
    residuals_sd <- sd(residuals)
    
    # Get model formula, coefficients, p-values, and confidence intervals
    model_formula <- deparse(model$call)
    coefficients <- tidy(model)
    coefficients_str <- paste(collapse = " | ", apply(coefficients, 1, function(x) paste(x, collapse = "; ")))
    
    # Combine into a data frame
    model_stat <- data.frame(
      Model_Name = model_name,
      Sample_Size = n,
      Model_Formula = model_formula,
      MSE = mse,
      RMSE = rmse,
      MAE = mae,
      R_squared = r_squared,
      Adjusted_R_squared = adjusted_r_squared,
      MSLE = msle,
      AIC = aic,
      BIC = bic,
      Residuals_Mean = residuals_mean,
      Residuals_Median = residuals_median,
      Residuals_Min = residuals_min,
      Residuals_Max = residuals_max,
      Residuals_SD = residuals_sd,
      Heteroscedasticity_p_value = heteroscedasticity_p_value,
      Autocorrelation_p_value = autocorrelation_p_value,
      Normality_p_value = normality_p_value,
      Warnings = warning,
      Coefficients = coefficients_str
    )
    
    model_stats[[i]] <- model_stat
  }
  
  # Combine all model statistics into a single data frame
  model_stats_df <- do.call(rbind, model_stats)
  
  return(model_stats_df)
}