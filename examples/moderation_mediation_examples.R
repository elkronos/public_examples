# Load required packages
load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# Moderation analysis
moderation_analysis <- function(data) {
  data$XZ <- data$X * data$Z
  moderation_model <- lm(Y ~ X + Z + XZ, data = data)
  summary(moderation_model)
  
  data$group <- ifelse(data$Z < median(data$Z), "Low Z", "High Z")
  ggplot(data, aes(x = X, y = Y, color = group)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = c("Low Z" = "blue", "High Z" = "red")) +
    labs(title = "Moderation Effect of Z on the relationship between X and Y",
         x = "X (Independent variable)", y = "Y (Dependent variable)") +
    theme_minimal()
}

# Mediation analysis
mediation_analysis <- function(data) {
  mediation_model <- "
    M ~ a*X
    Y ~ c*X + b*M
    indirect_effect := a*b
    total_effect := c + (a*b)
  "
  
  fit_med <- sem(mediation_model, data = data)
  summary(fit_med, standardized = TRUE)
  
  plot_mediation(data)
}

# Plot mediation effect
plot_mediation <- function(data) {
  p1 <- ggplot(data, aes(x = X, y = M)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "X -> M",
         x = "X (Independent variable)", y = "M (Mediator)") +
    theme_minimal()
  
  p2 <- ggplot(data, aes(x = M, y = Y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "M -> Y",
         x = "M (Mediator)", y = "Y (Dependent variable)") +
    theme_minimal()
  
  p3 <- ggplot(data, aes(x = X, y = Y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "X -> Y",
         x = "X (Independent variable)", y = "Y (Dependent variable)") +
    theme_minimal()
  
  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
}

main <- function() {
  # Load required packages
  load_packages(c("ggplot2", "lavaan", "gridExtra"))
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Simulate data
  n <- 200
  X <- rnorm(n)             # Independent variable
  M <- 0.5 * X + rnorm(n)   # Mediator variable
  Y <- 0.5 * X + 0.5 * M + rnorm(n)  # Dependent variable
  Z <- rnorm(n)             # Moderator variable
  
  data <- data.frame(X, M, Y, Z)
  
  # Run moderation analysis and plot the results
  moderation_plot <- moderation_analysis(data)
  print(moderation_plot)
  
  # Run mediation analysis and plot the results
  mediation_analysis(data)
}

# Run the main function
main()
