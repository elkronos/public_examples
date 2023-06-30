library(forecast)
library(ggplot2)
library(plotly)
library(pracma)

#' detect_seasonality
#'
#' The function 'detect_seasonality' is used to identify and visualize the seasonality in a given time series. 
#' The function handles missing values, transforms the time series to handle non-stationarity (if needed), 
#' identifies and handles outliers, and tests for seasonality. If seasonality is detected, it decomposes the 
#' time series into its seasonal, trend, and irregular components, and plots the seasonal component for visualization.
#' 
#' @param time_series A numeric vector or an object of class 'ts'. The time series to be analyzed for seasonality. 
#' Missing values in the time series are automatically omitted.
#' @param transformation A string. The method for transforming the time series to handle non-stationarity. 
#' Defaults to "boxcox". Other options are "log" and "none".
#' @param lambda A numeric value. The Box-Cox transformation parameter. If not provided, it is automatically 
#' calculated using the 'BoxCox.lambda' function from the 'forecast' package.
#' @param outlier_method A string. The method for identifying and handling outliers. Defaults to "hampel".
#' @param k A numeric value. The number of iterations in the Hampel identifier for outlier detection. 
#' Defaults to 3.
#' @param test_type A string. The test to be used for testing seasonality. Defaults to "auto". Other options 
#' are "ch" and "ocsb".
#' @param window A string. The window type to be used for decomposing the time series. Defaults to "periodic".
#' @param detect_freq A logical value. Whether to automatically detect the frequency of the time series. 
#' Defaults to TRUE.
#' @param freq A numeric value. The frequency of the time series. If not provided, it is automatically 
#' calculated if 'detect_freq' is TRUE.
#' @param plot_color A string. The color of the line plot for the seasonal component. Defaults to "blue".
#' @param plot_linetype A string. The line type of the plot for the seasonal component. Defaults to "solid".
#' @param plot_title A string. The title of the plot. Defaults to "Seasonal Component".
#' @param plot_original A logical value. Whether to plot the original time series alongside the seasonal 
#' component. Defaults to TRUE.
#' 
#' @return A list containing the original time series, the transformed time series, the seasonal component, 
#' the decomposed time series, and the interactive plot.
#' 
#' @examples
#' \dontrun{
#'   # Load the AirPassengers data
#'   data("AirPassengers")
#'
#'   # Apply the detect_seasonality function
#'   results <- detect_seasonality(AirPassengers, transformation = "log", outlier_method = "hampel", 
#'                                 plot_color = "red", plot_linetype = "dashed", 
#'                                 plot_title = "Original vs Seasonal Component",
#'                                 plot_original = TRUE)
#'   # The function returns a list. You can access the different components like this:
#'   original_time_series <- results$original_time_series
#'   transformed_time_series <- results$transformed_time_series
#'   seasonal_component <- results$seasonal_component
#'   decomposed <- results$decomposed
#'   interactive_plot <- results$interactive_plot
#'
#'   # Show the plot
#'   print(interactive_plot)
#' }
#' 
#' @importFrom forecast BoxCox BoxCox.lambda
#' @importFrom ggplot2 aes geom_line labs theme_minimal ggplot
#' @importFrom pracma hampel
#' 
#' @seealso \code{\link[forecast]{BoxCox}}, \code{\link[forecast]{BoxCox.lambda}}, \code{\link[ggplot2]{aes}}, 
#' \code{\link[ggplot2]{geom_line}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{theme_minimal}}, 
#' \code{\link[ggplot2]{ggplot}}, \code{\link[pracma]{hampel}}
#' 
#' @export
#' @rdname detect_seasonality
detect_seasonality <- function(time_series, transformation = "boxcox", lambda = NULL, outlier_method = "hampel", k = 3, 
                               test_type = "auto", window = "periodic", detect_freq = TRUE, freq = 12, 
                               plot_color = "blue", plot_linetype = "solid", plot_title = "Seasonal Component",
                               plot_original = TRUE) {
  
  # Handle missing values
  time_series <- na.omit(time_series)
  
  # Handle non-stationary time series with transformation
  if (transformation == "boxcox") {
    if (is.null(lambda)) {
      lambda <- BoxCox.lambda(time_series)
    }
    time_series_transformed <- BoxCox(time_series, lambda)
  } else if (transformation == "log") {
    time_series_transformed <- log(time_series)
  } else {
    time_series_transformed <- time_series
  }
  
  # Detect and handle outliers
  if (outlier_method == "hampel") {
    time_series_transformed <- ts(hampel(time_series_transformed, k = k)$y, 
                                  start = start(time_series_transformed), frequency = frequency(time_series_transformed))
  }
  
  # Test for seasonality
  if(test_type == "auto"){
    ndiffs <- nsdiffs(time_series_transformed)
  } else if(test_type == "ch"){
    ndiffs <- ifelse(chTest(time_series_transformed)$p.value < 0.05, 1, 0)
  } else if(test_type == "ocsb"){
    ndiffs <- ifelse(OCSBtest(time_series_transformed)$p.value < 0.05, 1, 0)
  }
  
  # If ndiffs > 0, there is seasonality
  if (ndiffs > 0) {
    
    # Detect frequency
    if(is.null(freq)){
      if(detect_freq == TRUE){
        freq <- findfrequency(time_series_transformed)
      } else {
        freq <- frequency(time_series_transformed)
      }
    }
    
    # Change frequency of time series
    time_series_transformed <- ts(time_series_transformed, frequency = freq)
    
    # Decompose the time series
    decomposed <- stl(time_series_transformed, s.window = window)
    
    # Extract the seasonal component
    seasonal_component <- decomposed$time.series[, "seasonal"]
    
    # Scale the original time series and the seasonal component for comparison
    original_time_series_scaled <- scale(time_series)
    seasonal_component_scaled <- scale(seasonal_component)
    
    # Convert the time series to data frame before plotting
    original_time_series_df <- data.frame(Time = as.numeric(time(original_time_series_scaled)), 
                                          Original = original_time_series_scaled)
    seasonal_component_df <- data.frame(Time = as.numeric(time(seasonal_component_scaled)), 
                                        Seasonality = seasonal_component_scaled)
    plot_data <- merge(original_time_series_df, seasonal_component_df, by = "Time", all = TRUE)
    
    # Create interactive plot
    p <- ggplot(plot_data, aes(x = Time)) +
      geom_line(aes(y = Seasonality), color = plot_color, linetype = plot_linetype)
    
    if (plot_original) {
      p <- p + geom_line(aes(y = Original), color = "black", linetype = "dashed")
    }
    
    p <- p + theme_minimal() +
      labs(y = "Scaled Value", title = plot_title)
    
    # Return the original and transformed time series, seasonal component, decomposed series, and plot
    return(list("original_time_series" = time_series, "transformed_time_series" = time_series_transformed, 
                "seasonal_component" = seasonal_component, "decomposed" = decomposed, "interactive_plot" = p))
  } else {
    print("No significant seasonality detected")
  }
}