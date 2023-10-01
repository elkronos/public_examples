# Load Packages
library(MASS)
library(ggplot2)
library(caret)
library(heplots)
library(GGally)

#' Plot Histograms of Specified Variables
#'
#' This function iterates through a specified set of variables from a data frame and
#' plots histograms using ggplot2.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param variables A character vector of variable names to be plotted. 
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_minimal
#' @import ggplot2
#'
#' @examples 
#' \dontrun{
#'   data <- data.frame(var1 = rnorm(100), var2 = rnorm(100))
#'   plot_histograms(data, variables = c("var1", "var2"))
#' }
#'
#' @return The function prints a series of ggplot2 histogram plots and doesn't return any value.
#'
#' @seealso \code{\link[ggplot2]{ggplot}} for the base plotting function used within \code{plot_histograms}.
#' 
#' @note Ensure that the variable names provided in the `variables` parameter match the column names in the `data`.
#' 
#' @export
plot_histograms <- function(data, variables) {
  for (var in variables) {
    print(
      ggplot(data, aes_string(x = var)) + 
        geom_histogram(binwidth = 0.5, fill = "lightblue") +
        ggtitle(paste("Histogram of", var)) +
        theme_minimal()
    )
  }
}

#' Check Normality of Variables within Groups
#'
#' This function performs the Shapiro-Wilk test for normality on specified variables within groups of a data frame.
#'
#' @param data A data frame containing the variables to test and a grouping variable.
#' @param variables A character vector specifying the names of the variables to test for normality.
#' @param grouping_var A string specifying the name of the variable used to group the data.
#'
#' @details
#' The function iterates over unique values (species, in a biological context, as an example) 
#' found within the grouping variable (`grouping_var`) and performs the Shapiro-Wilk test on 
#' the specified `variables`. The p-values from the test are printed to the console for each 
#' variable within each group.
#' 
#' Note that small p-values (typically <= 0.05) indicate that the null hypothesis of normality is 
#' rejected. Interpret results with caution and consult statistical advice as needed.
#' 
#' @return
#' This function doesn't return a value but prints the p-values from the Shapiro-Wilk tests to the console.
#'
#' @examples
#' \dontrun{
#' # Assuming `data` is a data frame with numerical variables
#' # "Sepal.Length", "Sepal.Width" and a grouping variable "Species".
#' 
#' check_normality(data = iris, 
#'                 variables = c("Sepal.Length", "Sepal.Width"), 
#'                 grouping_var = "Species")
#' }
#'
#' @seealso
#' \code{\link[stats]{shapiro.test}} for the underlying statistical test used in this function.
#'
#' @author Your Name
#' 
#' @export
check_normality <- function(data, variables, grouping_var){
  species <- unique(data[[grouping_var]])
  for (specie in species) {
    cat("\nShapiro-Wilk Test for species", specie, "\n")
    for (variable in variables) {
      p_val <- shapiro.test(data[data[[grouping_var]] == specie, variable])$p.value
      cat(variable, "p-value:", p_val, "\n")
    }
  }
}

#' Check for Homogeneity of Variance-Covariance
#'
#' This function performs Box's M test for homogeneity of covariance matrices using the `boxM` 
#' function from the `heplots` package. The test is used to determine whether the covariance 
#' matrices are equal across different groups.
#'
#' @param data A data frame or matrix containing the variables to be tested.
#' @param variables A character vector specifying the column names of the variables to 
#'   be included in the test.
#' @param grouping_var A character string specifying the name of the variable used to 
#'   define the groups.
#' 
#' @importFrom heplots boxM
#' 
#' @return The function implicitly prints the results of Box's M test to the console 
#'   via `print()` and also returns the result object from `boxM()` invisibly. It includes 
#'   test statistics, approximate chi-square statistic, degrees of freedom, and p-value.
#' 
#' @examples
#' \dontrun{
#' # Example usage of check_covariance_homogeneity function
#' # Assuming `mydata` is a data frame with numeric variables "var1", "var2", and a 
#' # grouping variable "group".
#' # 
#' # mydata <- data.frame(
#' #   group = rep(c("A", "B"), each = 50),
#' #   var1 = rnorm(100),
#' #   var2 = rnorm(100)
#' # )
#' #
#' # check_covariance_homogeneity(data = mydata, 
#' #                              variables = c("var1", "var2"), 
#' #                              grouping_var = "group")
#' }
#' 
#' @seealso [heplots::boxM()] for the underlying testing function.
#' 
#' @export
check_covariance_homogeneity <- function(data, variables, grouping_var){
  print(heplots::boxM(as.matrix(data[variables]), group = data[[grouping_var]]))
}

#' Plot Pair Plots to Check for Linearity
#'
#' The `plot_pairplots` function provides a quick visual check for linearity
#' between multiple variables using pair plots, enhanced by coloring points
#' with respect to an additional variable. 
#'
#' @importFrom GGally ggpairs
#' @importFrom ggplot2 theme_minimal
#'
#' @param data A data frame containing the variables to be plotted.
#' @param variables A vector of character strings specifying the names of the
#'   variables to include in the pair plot.
#' @param color_var A character string specifying the name of the variable
#'   used for coloring the points in the plots.
#'
#' @return A pair plot of the specified `variables`, colored by `color_var`,
#'   with a minimalistic theme applied.
#' 
#' @examples
#' \dontrun{
#'   # Example usage of plot_pairplots
#'   library(GGally)
#'   data(mtcars)
#'   plot_pairplots(data = mtcars, variables = c("mpg", "disp", "hp"), color_var = "am")
#' }
#'
#' @seealso \code{\link[GGally]{ggpairs}} and \code{\link[ggplot2]{theme_minimal}} for
#'   further modifications and details regarding pair plots and themes respectively.
#'
#' @export
plot_pairplots <- function(data, variables, color_var) {
  print(ggpairs(data, columns = variables, aes_string(color = color_var)) +
          theme_minimal())
}


#' Evaluate Classification Model Performance
#'
#' This function evaluates the performance of a classification model using various metrics
#' such as the confusion matrix, accuracy, precision, recall, and F1 score, and then prints
#' these metrics to the console.
#'
#' @param model A model object for which predictions should be made. This object is assumed
#'        to be compatible with the `predict` method from the `stats` package.
#' @param test_data A data.frame or similar object containing the testing data to be used
#'        for evaluating the model. It should include the true class labels.
#' @param true_col A character string specifying the name of the column in `test_data`
#'        that contains the true class labels.
#'
#' @importFrom stats predict
#' 
#' @return This function invisibly returns a list with the following components:
#'   * confusion_mat: A confusion matrix.
#'   * accuracy: Overall accuracy of the model.
#'   * precision: A vector of precision values, one for each class.
#'   * recall: A vector of recall values, one for each class.
#'   * F1: A vector of F1 scores, one for each class.
#'   These metrics are also printed to the console.
#' @examples
#' \dontrun{
#' # Assuming `my_model` is a fitted model, `my_test_data` is the test dataset, and
#' # "true_label" is the name of the column containing the true labels:
#' evaluate_model(my_model, my_test_data, "true_label")
#' }
#' @export
evaluate_model <- function(model, test_data, true_col){
  predictions <- predict(model, newdata = test_data)
  confusion_mat <- table(Predicted = predictions, Actual = test_data[[true_col]])
  cat("\nConfusion Matrix:\n")
  print(confusion_mat)
  
  accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
  precision <- diag(confusion_mat) / rowSums(confusion_mat)
  recall <- diag(confusion_mat) / colSums(confusion_mat)
  F1 <- 2 * (precision * recall) / (precision + recall)
  
  cat("\nMetrics:\n")
  cat("Overall Accuracy:", accuracy, "\n")
  cat("Precision by class:", precision, "\n")
  cat("Recall by class:", recall, "\n")
  cat("F1 Score by class:", F1, "\n")
  
  invisible(list(confusion_mat = confusion_mat,
                 accuracy = accuracy,
                 precision = precision,
                 recall = recall,
                 F1 = F1))
}

#' Visualize Linear Discriminant Analysis (LDA) Results
#'
#' Creates a density plot visualization of the linear discriminants from an LDA model, potentially 
#' trained using either `lda()` or `train()` (from the caret package).
#'
#' @param model An object of class `lda` or `train`. This is the model fitted to your data.
#' @param test_data A data frame containing the new data to predict.
#' @param lda_col A character string specifying the column name of the linear discriminant in the 
#' posterior probabilities to plot.
#' @param color_var A character string specifying the column name used to color the density plots.
#'
#' @importFrom ggplot2 ggplot aes_string labs theme_minimal guides geom_density guide_legend
#' @importFrom caret train
#' @importFrom MASS lda
#' @importFrom stats predict
#'
#' @return NULL. This function is used for the side effect of plotting.
#'
#' @examples
#' \dontrun{
#' # Assuming `lda_model` is an LDA model object and `new_data` a suitable data frame
#' plot_lda(lda_model, new_data, "LD1", "predicted_class")
#' }
#'
#' @export
plot_lda <- function(model, test_data, lda_col, color_var) {
  # Model predictions and extracting posterior probabilities
  if ("train" %in% class(model)) {
    predictions <- predict(model, newdata = test_data, type = "raw")
    posterior_probs <- as.data.frame(predict(model$finalModel, newdata = test_data[, colnames(model$finalModel$means)]))
  } else if ("lda" %in% class(model)) {
    predictions <- predict(model, newdata = test_data)
    posterior_probs <- as.data.frame(predictions$x)
  } else {
    stop("Model type not supported.")
  }
  
  # Naming the posterior probabilities
  names(posterior_probs) <- paste0("LD", 1:ncol(posterior_probs))
  posterior_probs[[color_var]] <- predictions
  
  # Check if lda_col exists in the data
  if (!(lda_col %in% names(posterior_probs))) {
    stop(paste("Variable", lda_col, "not found in the posterior probabilities data frame."))
  }
  
  # Check if color_var exists in the data
  if (!(color_var %in% names(posterior_probs))) {
    stop(paste("Variable", color_var, "not found in the posterior probabilities data frame."))
  }
  
  # Check for low group count and filter them out, if present
  group_counts <- table(posterior_probs[[color_var]])
  low_groups <- names(group_counts)[group_counts < 2]
  if (length(low_groups) > 0) {
    warning(paste(length(low_groups), "groups have fewer than two data points and will be dropped in the plot. Groups:", paste(low_groups, collapse = ", ")))
    posterior_probs <- posterior_probs[!(posterior_probs[[color_var]] %in% low_groups), ]
  }
  
  # Creating the plot
  p <- ggplot(posterior_probs, aes_string(x = lda_col, fill = color_var)) +
    labs(title = "Density plot of the Linear Discriminants", x = lda_col) +
    theme_minimal() +
    guides(fill = guide_legend(title = color_var))
  
  for (group in unique(posterior_probs[[color_var]])) {
    group_data <- posterior_probs[posterior_probs[[color_var]] == group, ]
    
    if (nrow(group_data) >= 2) {
      p <- p + geom_density(data = group_data, aes_string(x = lda_col, fill = color_var), alpha = 0.5)
    } else {
      message(paste("Omitting group", group, "from plot due to having fewer than two data points."))
    }
  }
  
  print(p)
}

#' @examples
#' \dontrun{
#' # Synthetic data creation
#' set.seed(123)
#' n <- 150
#'
#' # Generate data for three synthetic "species"
#' species_A <- data.frame(
#'   Sepal.Length = rnorm(n/3, mean=5, sd=0.5),
#'   Sepal.Width = rnorm(n/3, mean=3.5, sd=0.3),
#'   Petal.Length = rnorm(n/3, mean=1.5, sd=0.3),
#'   Petal.Width = rnorm(n/3, mean=0.3, sd=0.1),
#'   Species = factor(rep("A", n/3))
#' )
#'
#' species_B <- data.frame(
#'   Sepal.Length = rnorm(n/3, mean=6, sd=0.5),
#'   Sepal.Width = rnorm(n/3, mean=3, sd=0.3),
#'   Petal.Length = rnorm(n/3, mean=4.5, sd=0.4),
#'   Petal.Width = rnorm(n/3, mean=1.4, sd=0.2),
#'   Species = factor(rep("B", n/3))
#' )
#'
#' species_C <- data.frame(
#'   Sepal.Length = rnorm(n/3, mean=6.5, sd=0.6),
#'   Sepal.Width = rnorm(n/3, mean=3.2, sd=0.3),
#'   Petal.Length = rnorm(n/3, mean=5.5, sd=0.5),
#'   Petal.Width = rnorm(n/3, mean=2, sd=0.2),
#'   Species = factor(rep("C", n/3))
#' )
#'
#' synthetic_data <- rbind(species_A, species_B, species_C)
#'
#' # Check assumptions
#' plot_histograms(synthetic_data, names(synthetic_data)[1:4])
#' check_normality(synthetic_data, names(synthetic_data)[1:4], "Species")
#' check_covariance_homogeneity(synthetic_data, names(synthetic_data)[1:4], "Species")
#' plot_pairplots(synthetic_data, names(synthetic_data)[1:4], "Species")
#'
#' # Split the Data
#' set.seed(123)
#' splitIndex <- createDataPartition(synthetic_data$Species, p = .7, list = FALSE, times = 1)
#' train_data <- synthetic_data[splitIndex, ]
#' test_data  <- synthetic_data[-splitIndex, ]
#'
#' # Perform LDA with Cross-Validation
#' train_control <- trainControl(method = "cv", number = 10)
#' lda_cv_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'                       data = train_data,
#'                       method = "lda",
#'                       trControl = train_control,
#'                       metric = "Accuracy")
#'
#' # Evaluate Model
#' evaluate_model(lda_cv_model, test_data, "Species")
#'
#' # Visualize LDA
#' plot_lda(lda_cv_model, test_data, lda_col = "LD1", color_var = "Species")
#' }