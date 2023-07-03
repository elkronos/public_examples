# Structural equation modeling functions

#' Clear and set up the environment
#'
#' This function removes all objects from the current R environment, turns off
#' the graphics device, and clears the shell.
#'
#' @return None
clearAndSetupEnvironment <- function() {
  rm(list = ls(all = TRUE))
  graphics.off()
  shell("cls")
}

#' Load data from a file or environment
#'
#' @param dataInput A data frame, a string specifying the name of a data frame in the environment, or a string specifying the path to the Excel file.
#'
#' @return A data frame containing the loaded data.
#'
#' @importFrom readxl read_excel
loadData <- function(dataInput) {
  if (is.character(dataInput)) {
    # Check if the string is the name of a data frame in the environment
    if (exists(dataInput, envir = .GlobalEnv) && is.data.frame(get(dataInput, envir = .GlobalEnv))) {
      data <- get(dataInput, envir = .GlobalEnv)
    } else {
      library(readxl)
      data <- read_excel(
        path = dataInput,
        col_names = TRUE
      )
    }
  } else if (is.data.frame(dataInput)) {
    data <- dataInput
  } else {
    stop("dataInput must be a data frame, a string specifying the name of a data frame in the environment, or a string specifying the path to an Excel file.")
  }
  return(data)
}

#' Convert columns to factors
#'
#' This function converts the specified columns of a data frame to factors.
#'
#' @param data A data frame.
#' @param columns A character vector specifying the columns to convert to factors.
#'
#' @return The data frame with the specified columns converted to factors.
convertColumnsToFactors <- function(data, columns) {
  for(column in columns) {
    data[[column]] <- as.factor(data[[column]])
  }
  return(data)
}

#' Define SEM model
#'
#' This function defines a structural equation model.
#'
#' @param formula A string specifying the model formula.
#'
#' @return The model formula.
defineModel <- function(formula) {
  return(formula)
}


#' Estimate SEM model
#'
#' This function estimates a structural equation model using the lavaan package.
#'
#' @param mod.id A string specifying the model formula.
#' @param data A data frame containing the data.
#'
#' @return The estimated model.
#'
#' @importFrom lavaan sem
estimateModel <- function(mod.id, data) {
  library(lavaan)
  mod.est <- sem(
    model = mod.id,
    data = data
  )
  return(mod.est)
}

#' Display model summary
#'
#' This function displays a summary of a structural equation model.
#'
#' @param mod.est The estimated model.
#'
#' @return None
displayModelSummary <- function(mod.est) {
  summary(mod.est)
  summary(mod.est, fit.measures = TRUE)
}

#' Plot SEM paths
#'
#' This function plots the paths of a structural equation model using the semPlot package.
#'
#' @param mod.est The estimated model.
#'
#' @return None
#'
#' @importFrom semPlot semPaths
plotSEMPaths <- function(mod.est) {
  library(semPlot)
  semPaths(
    object = mod.est,
    what = "path",
    whatLabels = "par"
  )
}

#' Create more detailed SEM plot
#'
#' This function creates a more detailed plot of the paths of a structural equation model
#' using the semPlot package.
#'
#' @param mod.est The estimated model.
#'
#' @return The created plot.
#'
#' @importFrom semPlot semPaths
createSEMPlot <- function(mod.est) {
  library(semPlot)
  P <- semPaths(
    object = mod.est,
    what = "path",
    whatLabels = "par",
    style = "ram",
    layout = "tree",
    rotation = 2,
    sizeMan = 7,
    sizeLat = 7,
    color = "lightgray",
    edge.label.cex = 1.2,
    label.cex = 1.3
  )
  return(P)
}

#' Main function to run everything
#'
#' @param dataInput A data frame or a string specifying the path to the Excel file.
#' @param factorColumns A character vector specifying the columns to convert to factors.
#' @param formula A string specifying the model formula.
#'
#' @return A list containing the estimated model and the plot.
#'
#' @examples
#' runAnalysis(
#'   dataInput = 'data_path.xlsx',
#'   factorColumns = c('rep', 'water', 'priming'),
#'   formula = '
#'     EA =~ aba + apx + pod
#'     YC =~ til + pl + grp + tgw
#'     gy ~ EA + YC'
#' )
runAnalysis <- function(dataInput, factorColumns, formula) {
  clearAndSetupEnvironment()
  
  data <- loadData(dataInput)
  
  data <- convertColumnsToFactors(data, factorColumns)
  
  model <- defineModel(formula)
  
  modelEstimate <- estimateModel(model, data)
  
  displayModelSummary(modelEstimate)
  
  plotSEMPaths(modelEstimate)
  
  plot <- createSEMPlot(modelEstimate)
  
  return(list("modelEstimate" = modelEstimate, "plot" = plot))
}

#' Convert summary statistics into a data frame
#'
#' @param modelEstimate The estimated model from runAnalysis.
#'
#' @return A data frame containing the summary statistics of the model.
convertSummaryToDataFrame <- function(modelEstimate) {
  summaryStats <- summary(modelEstimate, fit.measures = TRUE)
  # Convert the list of summary statistics to a data frame
  summaryStatsDf <- as.data.frame(summaryStats$fit)
  return(summaryStatsDf)
}

#' Generate an HTML report of the analysis
#'
#' @param runAnalysisOutput The output from runAnalysis.
#' @param outputFilePath A string specifying the path to the output HTML file.
#'
#' @return None
#'
#' @importFrom rmarkdown render
#' @importFrom knitr kable
generateHTMLReport <- function(runAnalysisOutput, outputFilePath) {
  library(rmarkdown)
  library(knitr)
  
  # Extract the model and the plot
  modelEstimate <- runAnalysisOutput$modelEstimate
  plot <- runAnalysisOutput$plot
  
  # Convert the summary statistics into a data frame
  summaryStatsDf <- convertSummaryToDataFrame(modelEstimate)
  
  # Create the R Markdown content
  rmdContent <- paste0(
    '---',
    '\n',
    'title: "Analysis Report"',
    '\n',
    'output: html_document',
    '\n',
    '---',
    '\n',
    '```{r echo=FALSE, results="asis"}',
    '\n',
    'library(knitr)',
    '\n',
    'summaryStatsDf <- ', deparse(summaryStatsDf),
    '\n',
    'kable(summaryStatsDf, caption = "Model Summary Statistics")',
    '\n',
    '```',
    '\n',
    '```{r, echo=FALSE}',
    '\n',
    'plot <- ', deparse(plot),
    '\n',
    'print(plot)',
    '\n',
    '```'
  )
  
  # Write the R Markdown content to a temporary file
  rmdFile <- tempfile(fileext = ".Rmd")
  writeLines(rmdContent, rmdFile)
  
  # Render the R Markdown file to HTML
  render(rmdFile, output_file = outputFilePath)
}




## Example data

# Create the dataset
createSampleData <- function() {
  rep <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  water <- c("FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD")
  priming <- c("NP", "NP", "NP", "HP", "HP", "HP", "OP", "OP", "OP", "NP", "NP", "NP", "HP", "HP", "HP", "OP", "OP", "OP")
  aba <- c(4.60, 5.70, 4.10, 7.10, 5.60, 6.30, 8.00, 7.40, 7.80, 7.60, 8.70, 7.20, 10.10, 10.60, 9.30, 11.00, 10.40, 10.80)
  apx <- c(0.90, 0.77, 0.81, 0.97, 0.91, 0.87, 1.73, 1.63, 1.45, 1.45, 1.30, 1.10, 1.95, 1.90, 1.85, 2.73, 2.63, 2.50)
  pod <- c(3.78, 4.40, 5.33, 6.73, 6.31, 5.40, 8.38, 7.40, 7.63, 7.31, 6.40, 6.61, 9.31, 9.28, 8.64, 11.38, 11.43, 10.71)
  ph <- c(91, 82, 87, 94, 87, 82, 110, 96, 92, 103, 89, 83, 104, 91, 84, 109, 100, 93)
  til <- c(11, 11, 10, 15, 14, 13, 17, 15, 14, 10, 13, 14, 17, 20, 18, 23, 19, 22)
  pl <- c(13.68, 16.90, 14.35, 18.43, 18.92, 15.36, 22.76, 20.42, 19.96, 20.89, 17.85, 16.27, 23.90, 20.85, 21.46, 27.29, 21.82, 23.45)
  grp <- c(75.0, 84.5, 70.2, 94.1, 88.8, 81.1, 117.3, 104.6, 100.2, 95.3, 89.5, 81.2, 116.6, 100.4, 109.6, 150.2, 124.6, 137.3)
  tgw <- c(17.25, 13.63, 15.61, 20.89, 19.66, 17.37, 22.25, 22.65, 20.39, 18.60, 20.40, 17.09, 23.36, 21.40, 20.26, 29.33, 25.70, 26.40)
  gy <- c(20.92, 23.96, 19.12, 28.77, 25.90, 30.33, 36.38, 35.74, 30.78, 27.98, 31.50, 20.18, 33.01, 40.88, 35.11, 40.56, 45.68, 47.25)
  
  data <- data.frame(
    rep = rep,
    water = water,
    priming = priming,
    aba = aba,
    apx = apx,
    pod = pod,
    ph = ph,
    til = til,
    pl = pl,
    grp = grp,
    tgw = tgw,
    gy = gy
  )
  
  return(data)
}

createSampleData() -> data


# Define columns to be converted to factors
factorColumns <- c('rep', 'water', 'priming')

# Define SEM model formula
formula <- '
  EA =~ aba + apx + pod
  YC =~ til + pl + grp + tgw
  gy ~ EA + YC'

# Run the analysis
results <- runAnalysis(data, factorColumns, formula)

# Get model estimates
model_estimate <- results$modelEstimate

# Check fit
variance_table <- lavaan::varTable(model_estimate)
