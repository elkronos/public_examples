#' Find redundant variables in a data frame
#'
#' This function computes the correlation between specified variables in a data frame and returns the pairs with correlation
#' coefficients greater than or equal to the given inf_limit.
#'
#' @param data A data frame containing the variables to be analyzed.
#' @param var_names A character vector of variable names to be analyzed for redundancy.
#' @param inf_limit A numeric value indicating the absolute correlation coefficient threshold (default: 1).
#' @param cor_method A character string indicating which correlation method to use, "pearson", "kendall", or "spearman" (default: "pearson").
#'
#' @return A data frame with columns 'Var1', 'Var2', and 'Correlation' containing pairs of redundant variables and their correlation coefficients. If no pairs meet the inf_limit criterion, NULL is returned.
#'
#' @importFrom dplyr select arrange na.omit
#' @importFrom magrittr %>%
#'
#' @examples
#' find_redundant(mtcars, c("disp", "hp", "drat", "wt", "qsec"))
#' find_redundant(mtcars, c("disp", "hp", "drat", "wt", "qsec"), inf_limit = 0.75)

# Load packages
library(dplyr)
library(magrittr)

# Save function
find_redundant <- function(data, var_names, inf_limit = 1, cor_method = "pearson") {
  redundancy <- function(x, inf_limit) {
    x_upper_tri <- upper.tri(x, diag = TRUE)
    x[x_upper_tri] <- NA
    
    x_lower_tri_NA <- x %>%
      as.table() %>%
      as.data.frame() %>%
      na.omit() %>%
      setNames(c('Var1', 'Var2', 'Correlation'))
    
    founded_lines <- which(abs(x_lower_tri_NA[, 3]) >= inf_limit)
    
    if (length(founded_lines) > 0) {
      out <- x_lower_tri_NA[founded_lines,] %>%
        arrange(desc(abs(Correlation)))
    } else {
      out <- NULL
    }
    
    out
  }
  
  data %>%
    select(all_of(var_names)) %>%
    cor(method = cor_method) -> correlation_matrix
  
  correlation_matrix |> redundancy(inf_limit)
}