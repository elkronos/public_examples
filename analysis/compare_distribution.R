#' Compare the distribution of observed data with a specified distribution type
#'
#' @param observed_data A numeric vector containing the observed data
#' @param distribution_type A string specifying the distribution to compare with (default: "gaussian")
#'                          Accepted values: "gaussian", "poisson", "binomial", "exponential", "geometric", "negative_binomial"
#' @return A chi-squared test result object
#' 
#' @examples
#' # Example observed data
#' observed_data <- c(2, 3, 4, 1, 3, 0, 2, 4, 5, 1, 2, 2, 3, 4, 3, 1, 2, 5, 2, 3)
#' # Gaussian distribution example
#' gaussian_result <- compare_distribution(observed_data, distribution_type = "gaussian")
#' print("Gaussian result:")
#' print(gaussian_result)
#' 
#' # Poisson distribution example
#' poisson_result <- compare_distribution(observed_data, distribution_type = "poisson")
#' print("Poisson result:")
#' print(poisson_result)
#' 
#' # Binomial distribution example
#' binomial_result <- compare_distribution(observed_data, distribution_type = "binomial")
#' print("Binomial result:")
#' print(binomial_result)
#' 
#' # Exponential distribution example
#' exponential_result <- compare_distribution(observed_data, distribution_type = "exponential")
#' print("Exponential result:")
#' print(exponential_result)
#' 
#' # Geometric distribution example
#' geometric_result <- compare_distribution(observed_data, distribution_type = "geometric")
#' print("Geometric result:")
#' print(geometric_result)
#' 
#' # Negative binomial distribution example
#' negative_binomial_result <- compare_distribution(observed_data, distribution_type = "negative_binomial")
#' print("Negative binomial result:")
#' print(negative_binomial_result)
#' 

# Save function
compare_distribution <- function(observed_data, distribution_type = "gaussian") {
  # Calculate lambda and create frequency table of observed data
  lambda <- mean(observed_data)
  observed_freq <- table(observed_data)
  
  # Define categories, calculate expected probabilities and frequencies
  categories <- as.numeric(names(observed_freq))
  
  if (distribution_type == "gaussian") {
    expected_prob <- dnorm(categories, mean = lambda, sd = sd(observed_data))
  } else if (distribution_type == "poisson") {
    expected_prob <- dpois(categories, lambda)
  } else if (distribution_type == "binomial") {
    n <- max(observed_data)
    p <- lambda / n
    expected_prob <- dbinom(categories, size = n, prob = p)
  } else if (distribution_type == "exponential") {
    rate <- 1 / lambda
    expected_prob <- dexp(categories, rate = rate)
  } else if (distribution_type == "geometric") {
    p <- 1 / (lambda + 1)
    expected_prob <- dgeom(categories, prob = p)
  } else if (distribution_type == "negative_binomial") {
    size <- round(lambda^2 / (lambda - 1))
    prob <- lambda / (lambda + size)
    expected_prob <- dnbinom(categories, size = size, prob = prob)
  } else {
    stop("Invalid distribution type. Please use 'gaussian', 'poisson', 'binomial', 'exponential', 'geometric', or 'negative_binomial'.")
  }
  
  # Ensure that expected_prob sums to 1
  expected_prob <- expected_prob / sum(expected_prob)
  
  expected_freq <- round(expected_prob * length(observed_data))
  
  # Perform the chi-square test with Yates' continuity correction
  chisq_result <- chisq.test(observed_freq, p = expected_prob, correct = TRUE)
  
  # Check the result
  return(chisq_result)
}