# Load required packages
library(MASS)
library(ggplot2)
library(fitdistrplus)

#' Density Function for a Given Distribution
#'
#' This function calculates the probability density (or mass) function of a specified distribution
#' at the given data points. Available distributions include "gaussian", "poisson", "binomial",
#' "exponential", "geometric", "gamma", and "log-normal".
#'
#' @param dist A character string specifying the distribution.
#' @param x A numeric vector of data points where the PDF/PMF will be evaluated.
#' @param params A numeric vector containing the distribution parameters.
#'
#' @return A numeric vector containing the evaluated density.
#'
#' @examples
#' # Gaussian example:
#' density_fun("gaussian", seq(-5, 5, length.out = 100), c(0, 1))
#'
#' # Poisson example:
#' density_fun("poisson", 0:10, c(5))
#'
#' @export
density_fun <- function(dist, x, params) {
  if (!is.numeric(x)) stop("x must be a numeric vector.")
  x <- x[!is.na(x)]
  
  switch(tolower(dist),
         gaussian = {
           if (length(params) < 2) stop("Gaussian distribution requires parameters: mean and sd.")
           if (params[2] <= 0) stop("Standard deviation must be positive.")
           dnorm(x, mean = params[1], sd = params[2])
         },
         poisson = {
           if (length(params) < 1) stop("Poisson distribution requires parameter: lambda.")
           if (params[1] <= 0) stop("Lambda must be positive.")
           dpois(x, lambda = params[1])
         },
         binomial = {
           if (length(params) < 2) stop("Binomial distribution requires parameters: size and prob.")
           if (params[1] < 0) stop("Size must be non-negative.")
           if (params[2] < 0 || params[2] > 1) stop("Probability must be between 0 and 1.")
           dbinom(x, size = params[1], prob = params[2])
         },
         exponential = {
           if (length(params) < 1) stop("Exponential distribution requires parameter: rate.")
           if (params[1] <= 0) stop("Rate must be positive.")
           dexp(x, rate = params[1])
         },
         geometric = {
           if (length(params) < 1) stop("Geometric distribution requires parameter: prob.")
           if (params[1] <= 0 || params[1] > 1) stop("Probability must be between 0 and 1.")
           dgeom(x, prob = params[1])
         },
         gamma = {
           if (length(params) < 2) stop("Gamma distribution requires parameters: shape and rate.")
           if (params[1] <= 0 || params[2] <= 0) stop("Shape and rate must be positive.")
           dgamma(x, shape = params[1], rate = params[2])
         },
         `log-normal` = {
           if (length(params) < 2) stop("Log-normal distribution requires parameters: meanlog and sdlog.")
           if (params[2] <= 0) stop("sdlog must be positive.")
           dlnorm(x, meanlog = params[1], sdlog = params[2])
         },
         stop("Unsupported distribution specified.")
  )
}


#' Generate Random Numbers from a Given Distribution
#'
#' This function generates random numbers from a specified distribution. Available distributions include
#' "gaussian", "poisson", "binomial", "exponential", "geometric", "gamma", and "log-normal".
#'
#' @param dist A character string specifying the distribution.
#' @param n A positive integer indicating how many random numbers to generate.
#' @param params A numeric vector containing the distribution parameters.
#'
#' @return A numeric vector of generated random numbers.
#'
#' @examples
#' # Gaussian example:
#' r_fun("gaussian", 100, c(10, 2))
#'
#' # Poisson example:
#' r_fun("poisson", 100, c(4))
#'
#' @export
r_fun <- function(dist, n, params) {
  if (!is.numeric(n) || n <= 0) stop("n must be a positive integer.")
  n <- as.integer(n)
  
  switch(tolower(dist),
         gaussian = {
           if (length(params) < 2) stop("Gaussian distribution requires parameters: mean and sd.")
           if (params[2] <= 0) stop("Standard deviation must be positive.")
           rnorm(n, mean = params[1], sd = params[2])
         },
         poisson = {
           if (length(params) < 1) stop("Poisson distribution requires parameter: lambda.")
           if (params[1] <= 0) stop("Lambda must be positive.")
           rpois(n, lambda = params[1])
         },
         binomial = {
           if (length(params) < 2) stop("Binomial distribution requires parameters: size and prob.")
           if (params[1] < 0) stop("Size must be non-negative.")
           if (params[2] < 0 || params[2] > 1) stop("Probability must be between 0 and 1.")
           rbinom(n, size = params[1], prob = params[2])
         },
         exponential = {
           if (length(params) < 1) stop("Exponential distribution requires parameter: rate.")
           if (params[1] <= 0) stop("Rate must be positive.")
           rexp(n, rate = params[1])
         },
         geometric = {
           if (length(params) < 1) stop("Geometric distribution requires parameter: prob.")
           if (params[1] <= 0 || params[1] > 1) stop("Probability must be between 0 and 1.")
           rgeom(n, prob = params[1])
         },
         gamma = {
           if (length(params) < 2) stop("Gamma distribution requires parameters: shape and rate.")
           if (params[1] <= 0 || params[2] <= 0) stop("Shape and rate must be positive.")
           rgamma(n, shape = params[1], rate = params[2])
         },
         `log-normal` = {
           if (length(params) < 2) stop("Log-normal distribution requires parameters: meanlog and sdlog.")
           if (params[2] <= 0) stop("sdlog must be positive.")
           rlnorm(n, meanlog = params[1], sdlog = params[2])
         },
         stop("Unsupported distribution specified.")
  )
}


#' Plot Observed and Fitted Distributions
#'
#' This function plots overlapping histograms of observed data and data generated from a fitted distribution.
#'
#' @param data A numeric vector of observed data.
#' @param fitted_data A numeric vector of data generated from the fitted distribution.
#' @param dist_name A character string used for the plot title (name of the distribution).
#'
#' @importFrom ggplot2 aes geom_histogram scale_fill_manual theme_bw theme element_text labs
#' @export
plot_overlapping_histograms <- function(data, fitted_data, dist_name) {
  if (!is.numeric(data) || !is.numeric(fitted_data))
    stop("Both data and fitted_data must be numeric vectors.")
  if (length(data) == 0 || length(fitted_data) == 0)
    stop("Data vectors must not be empty.")
  
  hist_df <- data.frame(
    Data = c(data, fitted_data),
    Type = factor(rep(c("Observed", "Fitted"), times = c(length(data), length(fitted_data))))
  )
  
  p <- ggplot(hist_df, aes(x = Data, fill = Type)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, position = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("Observed" = "lightblue", "Fitted" = "red")) +
    theme_bw() +
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 20, hjust = 0.5)
    ) +
    labs(title = paste("Best Distribution:", dist_name))
  
  print(p)  # Explicitly print the plot
}


#' Find Best Distribution for a Given Dataset
#'
#' This function takes a dataset and finds the best-fitting distribution by comparing goodness-of-fit measures
#' (AIC or BIC) for a set of candidate distributions. The candidates are chosen automatically based on the
#' properties of the data (e.g. non-negativity, discreteness). It uses the fitdistrplus package for fitting.
#'
#' @param data_frame A data frame containing the dataset.
#' @param column A character string specifying the column name to analyze.
#' @param plot Logical. If TRUE, a plot comparing the observed data and the fitted distribution is shown.
#' @param use_bic Logical. If TRUE, the BIC is used for model comparison (default is AIC).
#' @param return_all_info Logical. If TRUE, returns a list with the best distribution name, all fitted models,
#'   and the goodness-of-fit table.
#'
#' @return Either a character string (best distribution) or a list with detailed information.
#'
#' @importFrom MASS fitdistr
#' @import ggplot2
#' @importFrom fitdistrplus fitdist
#'
#' @examples
#' # Gaussian data example:
#' gaussian_data <- rnorm(1000, mean = 10, sd = 2)
#' gaussian_df <- data.frame(gaussian_data)
#' best_dist <- find_distribution(gaussian_df, "gaussian_data", plot = TRUE)
#' print(paste("Best distribution for Gaussian data:", best_dist))
#'
#' @export
find_distribution <- function(data_frame, column, plot = FALSE, use_bic = FALSE, return_all_info = FALSE) {
  # Validate inputs
  if (!is.data.frame(data_frame))
    stop("data_frame must be a data.frame.")
  if (!column %in% names(data_frame))
    stop("Specified column not found in data_frame.")
  
  data <- data_frame[[column]]
  if (!is.numeric(data))
    stop("Data in the specified column must be numeric.")
  if (length(data) == 0)
    stop("Data column is empty.")
  
  # Choose candidate distributions based on data properties
  if (any(data < 0)) {
    candidate_dists <- c("gaussian")
  } else {
    candidate_dists <- c("gaussian", "poisson", "exponential", "gamma", "log-normal")
  }
  
  is_discrete <- all(data == floor(data))
  # For gamma and log-normal, data must be strictly positive
  can_fit_positive <- min(data) > 0
  
  fits <- list()
  gof <- data.frame(dist = character(),
                    loglik = numeric(),
                    aic = numeric(),
                    bic = numeric(),
                    stringsAsFactors = FALSE)
  
  for (dist in candidate_dists) {
    fit <- NULL
    if (dist == "gaussian") {
      fit <- tryCatch(fitdist(data, "norm"), error = function(e) NULL)
    } else if (dist == "poisson") {
      if (is_discrete) {
        fit <- tryCatch(fitdist(data, "pois"), error = function(e) NULL)
      }
    } else if (dist == "exponential") {
      fit <- tryCatch(fitdist(data, "exp"), error = function(e) NULL)
    } else if (dist == "gamma") {
      if (can_fit_positive) {
        fit <- tryCatch(fitdist(data, "gamma"), error = function(e) NULL)
      }
    } else if (dist == "log-normal") {
      if (can_fit_positive) {
        fit <- tryCatch(fitdist(data, "lnorm"), error = function(e) NULL)
      }
    }
    
    if (!is.null(fit)) {
      fits[[dist]] <- fit
      gof <- rbind(gof, data.frame(dist = dist,
                                   loglik = fit$loglik,
                                   aic = fit$aic,
                                   bic = fit$bic,
                                   stringsAsFactors = FALSE))
    }
  }
  
  if (nrow(gof) == 0) {
    stop("No candidate distributions could be successfully fit to the data.")
  }
  
  best_dist <- if (use_bic) {
    gof$dist[which.min(gof$bic)]
  } else {
    gof$dist[which.min(gof$aic)]
  }
  
  if (plot) {
    best_fit <- fits[[best_dist]]
    # Generate fitted data using the estimated parameters.
    fitted_data <- r_fun(best_dist, length(data), as.numeric(coef(best_fit)))
    plot_overlapping_histograms(data, fitted_data, best_dist)
  }
  
  if (return_all_info) {
    return(list(best_distribution = best_dist, fits = fits, gof = gof))
  } else {
    return(best_dist)
  }
}
