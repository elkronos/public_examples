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

#' Load data from a file
#'
#' This function reads an Excel file and loads the data into a data frame.
#'
#' @param filePath A string specifying the path to the Excel file.
#'
#' @return A data frame containing the loaded data.
#'
#' @importFrom readxl read_excel
loadData <- function(filePath) {
  library(readxl)
  data <- read_excel(
    path = filePath,
    col_names = TRUE
  )
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
#' This function runs the entire analysis workflow, including loading the data,
#' converting specified columns to factors, defining and estimating the model,
#' displaying a summary of the model, plotting the paths of the model, and creating a
#' more detailed plot of the paths.
#'
#' @param dataFilePath A string specifying the path to the Excel file.
#' @param factorColumns A character vector specifying the columns to convert to factors.
#' @param formula A string specifying the model formula.
#'
#' @return The created plot.
#'
#' @examples
#' runAnalysis(
#'   dataFilePath = 'data_path.xlsx',
#'   factorColumns = c('rep', 'water', 'priming'),
#'   formula = '
#'     EA =~ aba + apx + pod
#'     YC =~ til + pl + grp + tgw
#'     gy ~ EA + YC'
#' )
runAnalysis <- function(dataFilePath, factorColumns, formula) {
  clearAndSetupEnvironment()
  
  data <- loadData(dataFilePath)
  
  data <- convertColumnsToFactors(data, factorColumns)
  
  model <- defineModel(formula)
  
  modelEstimate <- estimateModel(model, data)
  
  displayModelSummary(modelEstimate)
  
  plotSEMPaths(modelEstimate)
  
  plot <- createSEMPlot(modelEstimate)
  
  return(plot)
}
