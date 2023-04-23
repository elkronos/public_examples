#' Density Function for a Given Distribution
#'
#' This function calculates the probability density function (PDF) of a specified distribution at the given data points. The \code{dist} argument is a character string that specifies the distribution. The available distributions are "gaussian", "poisson", "binomial", "exponential", "geometric", "gamma", and "log-normal". The \code{x} argument is a numeric vector containing the data points at which to evaluate the PDF. The \code{params} argument is a numeric vector containing the distribution parameters.
#'
#' @param dist A character string specifying the distribution to calculate the PDF for.
#' @param x A numeric vector containing the data points at which to evaluate the PDF.
#' @param params A numeric vector containing the distribution parameters.
#'
#' @return A numeric vector containing the PDF evaluated at the given data points.
#'
#' @examples
#' # calculate the PDF of a gaussian distribution
#' density_fun("gaussian", seq(-5, 5, length.out = 100), c(0, 1))
#'
#' # calculate the PDF of a poisson distribution
#' density_fun("poisson", 0:10, 5)
#'
#' @export

library(MASS)
library(ggplot2)
library(fitdistrplus)

set.seed(123)

# Density function
density_fun <- function(dist, x, params) {
  x <- x[!is.na(x)]
  if (dist == "gaussian") {
    return(dnorm(x, mean = params[1], sd = params[2]))
  } else if (dist == "poisson") {
    return(dpois(x, lambda = params[1]))
  } else if (dist == "binomial") {
    return(dbinom(x, size = params[1], prob = params[2]))
  } else if (dist == "exponential") {
    return(dexp(x, rate = params[1]))
  } else if (dist == "gamma") {
    return(dgamma(x, shape = params[1], rate = params[2]))
  } else if (dist == "log-normal") {
    return(dlnorm(x, meanlog = params[1], sdlog = params[2]))
  }
}

#' Generate Random Numbers from a Given Distribution
#'
#' This function generates random numbers from a specified distribution. The \code{dist} argument is a character string that specifies the distribution. The available distributions are "gaussian", "poisson", "binomial", "exponential", "geometric", "gamma", and "log-normal". The \code{n} argument specifies the number of random numbers to generate, and the \code{params} argument is a numeric vector containing the distribution parameters.
#'
#' @param dist A character string specifying the distribution to generate random numbers from.
#' @param n An integer specifying the number of random numbers to generate.
#' @param params A numeric vector containing the distribution parameters.
#'
#' @return A numeric vector containing the generated random numbers.
#'
#' @examples
#' # generate random numbers from a gaussian distribution
#' r_fun("gaussian", 100, c(10, 2))
#'
#' # generate random numbers from a poisson distribution
#' r_fun("poisson", 100, 4)
#'
#' @export

# Distribution function
r_fun <- function(dist, n, params) {
  if (dist == "gaussian") {
    return(rnorm(n, mean = params[1], sd = params[2]))
  } else if (dist == "poisson") {
    if (is.na(params[1]) || params[1] <= 0) return(rep(NA, n))  # Return NAs if the lambda parameter is invalid
    return(rpois(n, lambda = params[1]))
  } else if (dist == "binomial") {
    return(rbinom(n, size = params[1], prob = params[2]))
  } else if (dist == "exponential") {
    return(rexp(n, rate = params[1]))
  } else if (dist == "geometric") {
    return(rgeom(n, prob = params[1]))
  } else if (dist == "gamma") {
    return(rgamma(n, shape = params[1], rate = params[2]))
  } else if (dist == "log-normal") {
    return(rlnorm(n, meanlog = params[1], sdlog = params[2]))
  }
}

#' Plot Observed and Fitted Distributions
#'
#' This function takes two vectors, \code{data} and \code{fitted_data}, and the name of the distribution, and plots the overlapping histograms of the two datasets. The \code{data} vector contains the observed data, and the \code{fitted_data} vector contains the data generated from the fitted distribution. The \code{dist_name} argument is a character string that specifies the name of the distribution for the plot title.
#'
#' @param data A numeric vector containing the observed data.
#' @param fitted_data A numeric vector containing the data generated from the fitted distribution.
#' @param dist_name A character string specifying the name of the distribution for the plot title.
#'
#' @importFrom ggplot2 aes geom_histogram scale_fill_manual theme_bw theme element_text labs

# Plot function
plot_overlapping_histograms <- function(data, fitted_data, dist_name) {
  hist_df <- data.frame(Data = c(data, fitted_data),
                        Type = factor(rep(c("Observed", "Fitted"), c(length(data), length(fitted_data)))))
  
  plot <- ggplot(hist_df, aes(x = Data, fill = Type)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, position = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("Observed" = "lightblue", "Fitted" = "red")) +
    theme_bw() +
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 20, hjust = 0.5)
    ) +
    labs(title = paste("Best Distribution: ", dist_name))
  
  print(plot)  # Explicitly print the plot
}

#' Find Best Distribution for a Given Dataset
#'
#' This function takes a dataset and finds the best-fitting distribution by comparing the Akaike Information Criterion (AIC) or Bayesian Information Criterion (BIC) for a set of candidate distributions. The function uses the fitdistrplus package to fit distributions to the data and returns the name of the best distribution. Additionally, it can plot the observed data and the fitted distribution if the \code{plot} argument is set to TRUE. The function can handle six types of distributions: Gaussian, Poisson, Exponential, Gamma, and Log-normal.
#'
#' @param data_frame A data frame containing the dataset.
#' @param column A character string containing the column name of the dataset to analyze.
#' @param plot A logical value indicating whether to plot the observed data and the fitted distribution. Default is FALSE.
#' @param use_bic A logical value indicating whether to use the BIC instead of the AIC for the model comparison. Default is FALSE.
#' @param return_all_info A logical value indicating whether to return all the fitted distributions, their parameters, and the goodness-of-fit measures. Default is FALSE.
#'
#' @return If \code{return_all_info} is FALSE (default), the function returns the name of the best-fitting distribution as a character string. If \code{return_all_info} is TRUE, the function returns a list with three elements: \code{best_distribution} (name of the best-fitting distribution), \code{fits} (list of all fitted distributions and their parameters), and \code{gof} (data frame with goodness-of-fit measures for all fitted distributions).
#'
#' @importFrom MASS fitdistr
#' @import ggplot2
#' @importFrom fitdistrplus descdist
#'
#' @examples
#' # Gaussian dataset
#' gaussian_data <- rnorm(1000, mean = 10, sd = 2)
#' gaussian_df <- data.frame(gaussian_data)
#' gaussian_best_dist <- find_distribution(gaussian_df, "gaussian_data", plot = TRUE)
#' print(paste("Best distribution for Gaussian data:", gaussian_best_dist))
#'
#' # Poisson dataset
#' poisson_data <- rpois(1000, lambda = 4)
#' poisson_df <- data.frame(poisson_data)
#' poisson_best_dist <- find_distribution(poisson_df, "poisson_data", plot = TRUE)
#' print(paste("Best distribution for Poisson data:", poisson_best_dist))
#'
#' # Exponential dataset
#' exponential_data <- rexp(1000, rate = 0.5)
#' exponential_df <- data.frame(exponential_data)
#' exponential_best_dist <- find_distribution(exponential_df, "exponential_data", plot = TRUE)
#' print(paste("Best distribution for Exponential data:", exponential_best_dist))
#'
#' # Gamma dataset
#' gamma_data <- rgamma(1000, shape = 2, rate = 0.5)
#' gamma_df <- data.frame(gamma_data)
#' gamma_best_dist <- find_distribution(gamma_df, "gamma_data", plot = TRUE)
#' print(paste("Best distribution for Gamma data:", gamma_best_dist))
#'
#' # Log-normal dataset
#' log_normal_data <- rlnorm(1000, meanlog = 1, sdlog = 0.5)
#' log_normal_df <- data.frame(log_normal_data)
#' log_normal_best_dist <- find_distribution(log_normal_df, "log_normal_data", plot = TRUE)
#' print(paste("Best distribution for Log-normal data:", log_normal_best_dist))
#'
#' @export

# Find best distribution
find_distribution <- function(data_frame, column, plot = FALSE, use_bic = FALSE, return_all_info = FALSE) {
  data <- data_frame[[column]]
  distributions <- c("gaussian", "poisson", "exponential", "gamma", "log-normal")
  
  fits <- list()
  gof <- data.frame(dist = character(),
                    loglik = numeric(),
                    aic = numeric(),
                    bic = numeric(),
                    stringsAsFactors = FALSE)
  
  if (all(data >= 0)) {
    is_discrete <- all(data == round(data))
    has_zeros <- any(data == 0)
    
    for (dist in distributions) {
      if (dist == "gaussian") {
        fits[[dist]] <- fitdist(data, "norm")
      } else if (dist == "poisson") {
        if (is_discrete) {
          fits[[dist]] <- fitdist(data, "pois")
        }
      } else if (dist == "exponential") {
        fits[[dist]] <- fitdist(data, "exp")
      } else if (dist == "gamma") {
        if (!is_discrete) {
          fits[[dist]] <- fitdist(data, "gamma")
        }
      } else if (dist == "log-normal") {
        if (!has_zeros) {
          fits[[dist]] <- fitdist(data, "lnorm")
        }
      }
      
      if (!is.null(fits[[dist]])) {
        gof <- rbind(gof, data.frame(dist = dist,
                                     loglik = fits[[dist]]$loglik,
                                     aic = fits[[dist]]$aic,
                                     bic = fits[[dist]]$bic))
      }
    }
    
    if (nrow(gof) > 0) {
      if (use_bic) {
        best_dist <- gof[which.min(gof$bic), "dist"]
      } else {
        best_dist <- gof[which.min(gof$aic), "dist"]
      }
      
      if (plot) {
        fitted_data <- r_fun(best_dist, length(data), coef(fits[[best_dist]]))
        plot_overlapping_histograms(data, fitted_data, best_dist)
      }
      
      if (return_all_info) {
        return(list(best_distribution = best_dist, fits = fits, gof = gof))
      } else {
        return(best_dist)
      }
    }
  } else {
    stop("Data must be non-negative.")
  }
}