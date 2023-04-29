#' Find common columns between data frames
#'
#' This function takes a list of data frames and returns a data frame indicating which
#' columns are present in each pair of data frames.
#'
#' @param df_list A list of data frames
#'
#' @return A data frame with three columns: Data_Frame (the indices of the data frames
#' that were compared), Variable (the name of the common variable), and Common
#' (whether or not the variable is present in both data frames)
#'
#' @importFrom dplyr bind_rows colnames
#' @export
#'
#' @examples
#' df1 <- data.frame(ID = 1:5, Name = c("John", "Mary", "Bob", "Alice", "Tom"))
#' df2 <- data.frame(ID = 3:7, Age = c(25, 32, 46, 18, 57))
#' df3 <- data.frame(ID = 2:6, Gender = c("M", "F", "M", "F", "M"))
#' df_list <- list(df1, df2, df3)
#' find_ids(df_list) # Returns a data frame
# Load package
library(dplyr)
# Save function
find_ids <- function(df_list) {
  
  # Create a data frame to store the results
  result_df <- data.frame(Data_Frame = integer(),
                          Variable = character(),
                          Common = character(),
                          stringsAsFactors = FALSE)
  
  # Compare each pair of data frames
  for (i in 1:length(df_list)) {
    for (j in (i+1):length(df_list)) {
      
      # Check if index j is within bounds of df_list
      if(j <= length(df_list)) {
        df1 <- df_list[[i]]
        df2 <- df_list[[j]]
        
        # Compare each column in the two data frames
        for (col in colnames(df1)) {
          if (col %in% colnames(df2)) {
            # If the column is present in both data frames, add it to the result data frame
            result_df <- bind_rows(result_df, data.frame(Data_Frame = c(i, j),
                                                         Variable = col,
                                                         Common = "Yes",
                                                         stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  
  # Convert "Data_Frame" column to integer type
  result_df$Data_Frame <- as.integer(result_df$Data_Frame)
  
  # Return the result data frame
  return(result_df)
}

#' Visualize Data Connections
#'
#' Visualize the common columns between multiple data frames as a graph. 
#' This function uses the output from the \code{\link{find_ids}} function. 
#' 
#' @param df_list A list of data frames to analyze
#'
#' @return An undirected graph showing the common variables between the data frames
#'
#' @import igraph
#' @importFrom dplyr filter select distinct group_by mutate ungroup
#' @export
#' 
#' @examples 
#' # Load example data
#' df1 <- data.frame(ID1 = 1:5, Name = c("John", "Mary", "Bob", "Alice", "Tom"), Age = c(23, 34, 29, 19, 41))
#' df2 <- data.frame(ID1 = 3:7, Age = c(25, 32, 46, 18, 57), Gender = c("M", "F", "M", "F", "M"))
#' df3 <- data.frame(ID2 = 2:6, Gender = c("M", "F", "M", "F", "M"), City = c("NYC", "LA", "Chicago", "Houston", "Miami"))
#' df4 <- data.frame(ID2 = 2:6, Gender = c("M", "F", "M", "F", "M"), City = c("NYC", "LA", "Chicago", "Houston", "Miami"))
#'
#' # Save data as list
#' df_list <- list(df1, df2, df3, df4)
#'
#' # Create plot
#' vis_ids(df_list)
#'
#' @seealso \code{\link{find_ids}}
#' @keywords data, visualization, graph, igraph
#' @export
# Load package
library(igraph)
# Save function
vis_ids <- function(df_list) {
  # Get the results from the find_ids function
  results <- find_ids(df_list)
  
  # Debugging print statement
  print(unique(results$Data_Frame))
  
  # Create an empty graph object
  g <- graph.empty(n = length(unique(results$Data_Frame)))
  
  # Add the nodes (data frames) to the graph
  V(g)$name <- as.character(unique(results$Data_Frame))
  
  # Add the edges (common variables) to the graph
  edges <- results %>%
    filter(Common == "Yes") %>%
    select(Data_Frame, Variable) %>%
    distinct() %>%
    group_by(Variable) %>%
    mutate(n = n()) %>%
    filter(n > 1 | is.na(n)) %>%
    ungroup() %>%
    select(-n)
  
  g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)
  
  # Set the layout of the graph
  layout <- layout_with_kk(g)
  
  # Plot the graph
  plot(g, layout = layout, vertex.label.cex = 1.2, edge.label.cex = 1.2,
       vertex.color = "white", vertex.frame.color = "black", vertex.shape = "circle",
       edge.arrow.size = 0.5)
}


#' Clean variable names in a list of data frames
#'
#' This function cleans variable names in a list of data frames. It replaces spaces and non-alphanumeric characters with a specified separator (underscore by default) and converts the names to lowercase. The cleaned names are then assigned to the column names of each data frame.
#'
#' @param df_list A list of data frames to be cleaned.
#' @param replace_non_alphanumeric A logical value indicating whether non-alphanumeric characters should be replaced. Default is TRUE.
#' @param separator A character used to replace spaces and non-alphanumeric characters. Default is "_".
#' @return A list of data frames with cleaned variable names.
#' @importFrom utils gsub tolower
#' @examples
#' # Create example dataset
#' df1 <- data.frame("ID" = 1:5, "Name" = c("John Smith", "Samantha Jones", "David Lee", "Emily Brown", "Tom Davis"), 
#'                   "Age" = c(28, 35, 42, 29, 47), "Income (USD)" = c(50000, 75000, 90000, 62000, 85000))
#' df2 <- data.frame("ID" = 6:10, "Name" = c("Mary Johnson", "Alex Kim", "Rachel Chen", "Adam Garcia", "Linda Wong"), 
#'                   "Age" = c(32, 41, 26, 37, 49), "Income (USD)" = c(72000, 89000, 60000, 82000, 95000))
#' df3 <- data.frame("ID" = 11:15, "Name" = c("Erica Martinez", "Peter Chen", "Alicia Ramirez", "Jake Kim", "Karen Lee"), 
#'                   "Age" = c(45, 27, 33, 51, 39), "Income (USD)" = c(98000, 55000, 72000, 105000, 83000))
#'
#' # Combine into list
#' df_list <- list(df1, df2, df3)
#'
#' # Clean variable names
#' cleaned_df_list <- clean_varnames(df_list)
#'
#' # Print column names of first data frame
#' colnames(cleaned_df_list[[1]])
#'
#' @export
# Load clean_varnames function
clean_varnames <- function(df_list, replace_non_alphanumeric = TRUE, separator = "_") {
  stopifnot(is.list(df_list))
  # Loop through each data frame in the list
  for (i in seq_along(df_list)) {
    # Clean variable names in each data frame
    varnames <- gsub(" ", separator, tolower(gsub("\\s+", "", colnames(df_list[[i]]))))
    if (replace_non_alphanumeric) {
      varnames <- gsub("[^[:alnum:]]+", separator, varnames)
    }
    varnames <- gsub(paste0(separator, "+$"), "", varnames)
    colnames(df_list[[i]]) <- varnames
  }
  return(df_list)
}