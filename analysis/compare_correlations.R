# Load necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(MASS)
library(polycor)

#' Compare Correlations
#'
#' This function calculates correlation matrices for each level of a grouping variable within a given dataset.
#' The function can compute either Pearson or polychoric correlations. The differences between the correlation
#' matrices of different groups are then calculated and visualized as heatmaps.
#'
#' @param data A data frame containing the data to be analyzed. It should include the grouping variable and the numeric variables to be correlated.
#' @param grouping_var A character string specifying the column name of the grouping variable in the data.
#' @param polychoric A logical. If FALSE (default), Pearson correlations are computed. If TRUE, polychoric correlations are computed.
#'
#' @return This function does not return a value. It prints a series of heatmaps representing the absolute differences between the correlation matrices of different groups.
#'
#' @importFrom dplyr filter select_if
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c theme_minimal theme labs coord_fixed geom_text
#' @importFrom reshape2 melt
#' @importFrom MASS rnorm
#' @importFrom polycor hetcor
#'
#' @examples
#' # Set a seed for reproducibility
#' set.seed(123)
#'
#' # Create synthetic numeric variables
#' x1 <- rnorm(300)
#' x2 <- rnorm(300)
#' x3 <- x1 + rnorm(300, sd = 0.5)
#' x4 <- x2 + rnorm(300, sd = 0.5)
#'
#' # Create a synthetic categorical variable with 3 levels
#' grouping_var <- sample(c("Group1", "Group2", "Group3"), 300, replace = TRUE)
#'
#' # Combine into a data frame
#' data <- data.frame(grouping_var, x1, x2, x3, x4)
#'
#' # Call the function
#' compare_correlations(data, "grouping_var")
#'
#' # Set a seed for reproducibility
#' set.seed(123)
#'
#' # Create synthetic ordinal variables
#' # Consider these as Likert scale responses (from 1 to 5) for three different questions (x1, x2, x3)
#' x1 <- sample(1:5, 300, replace = TRUE)
#' x2 <- sample(1:5, 300, replace = TRUE)
#' x3 <- sample(1:5, 300, replace = TRUE)
#'
#' # Create a synthetic categorical variable with 3 levels
#' grouping_var <- sample(c("Group1", "Group2", "Group3"), 300, replace = TRUE)
#'
#' # Combine into a data frame
#' ordinal_data <- data.frame(grouping_var, x1, x2, x3)
#'
#' # Use function on ordinal data
#' compare_correlations(ordinal_data, "grouping_var", polychoric = TRUE)
#' 
#' @export
compare_correlations <- function(data, grouping_var, polychoric = FALSE) {
  
  # Check if grouping variable exists in the data
  if (!(grouping_var %in% colnames(data))) {
    stop(paste0("Error: Grouping variable ", grouping_var, " does not exist in the dataset."))
  }
  
  # Create a list to store the correlation matrices
  cor_list <- list()
  
  # Get unique values of the grouping variable
  group_values <- unique(data[[grouping_var]])
  
  # Calculate correlation matrix for each group
  for(i in seq_along(group_values)) {
    group_data <- data %>% 
      filter((!!sym(grouping_var)) == group_values[i]) %>%
      select_if(is.numeric)
    
    if (polychoric) {
      library(polycor)
      cor_list[[i]] <- hetcor(group_data, ML = TRUE)$correlations
    } else {
      cor_list[[i]] <- cor(group_data, use = "pairwise.complete.obs")
    }
  }
  
  # Get all permutations of two different groups
  permutations <- combn(length(cor_list), 2, simplify = FALSE)
  
  # For each permutation, calculate the absolute difference between correlation matrices
  for (i in seq_along(permutations)) {
    # Get the two correlation matrices
    cor1 <- cor_list[[permutations[[i]][1]]]
    cor2 <- cor_list[[permutations[[i]][2]]]
    
    # Calculate the absolute difference
    difference <- abs(cor1 - cor2)
    
    # Melt the difference matrix to long format for ggplot2
    melted <- melt(difference)
    
    # Only keep lower triangle of the matrix for the plot
    melted$Var1 <- as.numeric(as.factor(melted$Var1))
    melted$Var2 <- as.numeric(as.factor(melted$Var2))
    melted <- melted[melted$Var1 > melted$Var2,]
    
    # Create a heatmap of the absolute differences
    print(
      ggplot(data = melted, aes(x = factor(Var1), y = factor(Var2), fill = value)) +
        geom_tile() +
        scale_fill_viridis_c(option = "C", direction = -1, aesthetics = "fill", limits = c(0,1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        labs(title = paste("Difference between group", permutations[[i]][1], 
                           "and group", permutations[[i]][2]),
             fill = "Absolute\nDifference") +
        coord_fixed() +
        geom_text(aes(label = round(value, 2)), size = 3, color = "black")  # Added geom_text to display values on tiles
    )
  }
}