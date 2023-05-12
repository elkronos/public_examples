#' ######################## Index and Table of Contents
#' 
#' # LINE 030 - create_graph() - Prepare data (dataframe to network graph)
#' # LINE 083 - group_summary() - Calculate descriptive network information.
#' # LINE 119 - plot_graph() - Plot a network graph
#' # LINE 163 - plot_degree_distribution() - Plot the degree distribution of nodes (asses peaks and distribution)
#' # LINE 201 - compare_graphs() - Compare differences among two networks
#' # LINE 260 - network_overlap() - Calculate overlap between two networks
#' # LINE 302 - network_metrics() - Calculate measures of centrality as well as other KPIs (cluster, small worldness, etc)
#' # LINE 355 - save_graph() - Save network plots
#' 
# Load libraries
library(igraph)
library(dplyr)
library(ggraph)
library(tidygraph)

#' Prepare graph input and create graph
#' 
#' This function takes a data frame and converts it into a graph object using the igraph package. The first two columns of the data frame are assumed to represent edges of the graph, and additional columns can be used to specify attributes of each edge. The function removes duplicate rows and rows with missing values before creating the graph. The resulting graph object can be directed or undirected.
#' 
#' @param df A data frame with at least two columns representing the edges of the graph.
#' @param directed Logical. If TRUE, the resulting graph will be directed. If FALSE, the graph will be undirected. Default is FALSE.
#' @param weight_col The name of a column in the data frame that specifies the weight of each edge. If NULL (default), all edges are assumed to have equal weight.
#' 
#' @return A graph object created from the input data frame using the igraph package. The graph will have an additional vertex attribute called "group", which specifies the connected component to which each vertex belongs.
#' 
#' @importFrom igraph graph_from_data_frame set_vertex_attr components membership E
#' @import dplyr
create_graph <- function(df, from_col, to_col, directed = FALSE, weight_col = NULL) {
  if (ncol(df) < 2) {
    stop("Data frame must have at least two columns.")
  }
  
  if (!(from_col %in% colnames(df)) || !(to_col %in% colnames(df))) {
    stop("Both 'from_col' and 'to_col' must be present in the data frame.")
  }
  
  # Remove duplicate rows and rows with missing values
  df <- unique(df) %>% na.omit()
  
  # Create edges data frame from the input data frame
  edges <- data.frame(from = df[[from_col]], to = df[[to_col]])
  
  # Assign the additional columns as edge attributes
  other_cols <- setdiff(colnames(df), c(from_col, to_col))
  for (i in seq_along(other_cols)) {
    edges[[other_cols[i]]] <- df[[other_cols[i]]]
  }
  
  graph <- graph_from_data_frame(edges, directed = directed)
  
  if (!is.null(weight_col) && weight_col %in% colnames(df)) {
    E(graph)$weight <- df[[weight_col]]
  }
  
  components <- components(graph)
  membership <- membership(components)
  
  graph <- set_vertex_attr(graph, "group", value = membership)
  return(graph)
}

#' Generate a summary of groups in a graph
#'
#' This function generates a summary of the groups in a graph, including the number
#' of groups, the size of each group, and the list of group members.
#'
#' @param graph A graph object from the igraph package.
#'
#' @return A list with the following components:
#' \item{num_groups}{The number of groups in the graph.}
#' \item{group_sizes}{A numeric vector containing the size of each group.}
#' \item{group_list}{A list of character vectors, where each element is the 
#'                    names of the vertices in the corresponding group.}
#'
#' @import igraph::is.null
#' @import igraph::vcount
#' @import igraph::ecount
#' @importFrom igraph V
#' @importFrom igraph split
#' @importFrom stats sapply
group_summary <- function(graph) {
  if (is.null(graph) || vcount(graph) == 0 || ecount(graph) == 0) {
    cat("Cannot generate group summary for a NULL or empty graph.\n")
    return(NULL)
  }
  
  group_list <- split(V(graph)$name, V(graph)$group)
  group_sizes <- sapply(group_list, length)
  num_groups <- length(group_list)
  
  cat("Number of groups:", num_groups, "\n")
  cat("Group sizes:", group_sizes, "\n")
  cat("Group members:\n")
  
  for (i in 1:length(group_list)) {
    cat("Group", i, ":", group_list[[i]], "\n")
  }
  
  return(list(num_groups = num_groups, group_sizes = group_sizes, group_list = group_list))
}


#' Plot a network graph using ggplot2 and ggraph
#'
#' This function takes a graph object and plots it as a network graph using ggplot2 and ggraph. It creates a force-directed layout using the Fruchterman-Reingold algorithm and plots the nodes as points with the node names as labels. The size and color of the nodes are determined by their degree and group, respectively.
#'
#' @param graph A graph object to plot.
#' @param title A character string for the plot title. Default is "Network Graph".
#' @param node_aes A character string of node aesthetics to add to the plot using aes_string(). 
#'   For example, "fill = 'blue', shape = 21, stroke = 'black'". Default is NULL.
#' @param edge_aes A character string of edge aesthetics to add to the plot using aes_string(). 
#'   For example, "color = 'gray80', alpha = 0.5, arrow = arrow(length = unit(0.1, 'inches'))". Default is NULL.
#'
#' @return A ggplot2 object representing the plotted network graph.
#'
#' @import tidygraph ggplot2 ggraph
plot_graph <- function(graph, title = "Network Graph", node_aes = NULL, edge_aes = NULL) {
  if (is.null(graph) || vcount(graph) == 0 || ecount(graph) == 0) {
    cat("Cannot plot a NULL or empty graph.\n")
    return(NULL)
  }
  
  tg <- as_tbl_graph(graph)
  
  gg <- ggraph(tg, layout = "fr") +
    geom_edge_link(aes(edge_alpha = after_stat(index)), show.legend = FALSE) +
    geom_node_point(aes(size = degree(graph), color = as.factor(group)), show.legend = FALSE) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_color_viridis_d() +
    theme_void() +
    labs(title = title)
  
  if (!is.null(node_aes)) {
    gg <- gg + geom_node_point(aes_string(node_aes), show.legend = FALSE)
  }
  if (!is.null(edge_aes)) {
    gg <- gg + geom_edge_link(aes_string(edge_aes), show.legend = FALSE)
  }
  
  return(gg)
}


#' Plot the degree distribution of a graph
#'
#' This function takes a graph object and plots its degree distribution, i.e., the distribution of node degrees in the graph. By default, the x and y axes are transformed logarithmically, but this can be changed using the \code{x_trans} and \code{y_trans} parameters.
#'
#' @param graph A graph object to plot.
#' @param title A character string for the plot title. Default is "Degree Distribution".
#' @param x_trans A character string specifying the transformation to use for the x axis. Default is "log10".
#' @param y_trans A character string specifying the transformation to use for the y axis. Default is "log10".
#' @param ccdf A logical value indicating whether to plot the complementary cumulative distribution function (CCDF) of the degree distribution instead of the raw distribution. Default is FALSE.
#'
#' @return A ggplot2 object representing the plotted degree distribution.
#'
#' @import ggplot2 scales igraph
#' 
#' @seealso \code{\link{degree}}, \code{\link{group_summary}}, \code{\link{modularity}}, \code{\link{mean_distance}}
#' 
#' @references Clauset, A., Shalizi, C.R., Newman, M.E.J. (2009). Power-law
plot_degree_distribution <- function(graph, title = "Degree Distribution", x_trans = "log10", y_trans = "log10", ccdf = FALSE) {
  if (is.null(graph) || vcount(graph) == 0 || ecount(graph) == 0) {
    cat("Cannot plot a NULL or empty graph.\n")
    return(NULL)
  }
  
  degree_values <- degree(graph)
  degree_freq <- as.data.frame(table(degree_values))
  colnames(degree_freq) <- c("degree", "frequency")
  degree_freq$degree <- as.numeric(as.character(degree_freq$degree))
  
  if (ccdf) {
    degree_freq$frequency <- cumsum(degree_freq$frequency, fromLast = TRUE)
  }
  
  ggplot(degree_freq, aes(x = degree, y = frequency)) +
    geom_point() +
    geom_line(group = 1) +
    scale_x_continuous(trans = x_trans, breaks = scales::trans_breaks(x_trans, function(x) 10^x)) +
    scale_y_continuous(trans = y_trans, breaks = scales::trans_breaks(y_trans, function(x) 10^x)) +
    labs(title = title, x = "Degree", y = "Frequency") +
    theme_minimal()
}


#' Compare Multiple Graphs
#'
#' This function compares multiple graphs by computing their group summary, modularity score,
#' and average path length. It also calculates the overlap between groups in all graphs.
#'
#' @param ... A variable number of igraph objects to be compared.
#' 
#' @return A list containing the following items:
#' \item{summaries}{A list of group summaries for each input graph.}
#' \item{modularities}{A numeric vector of modularity scores for each input graph.}
#' \item{avg_path_lengths}{A numeric vector of average path lengths for each input graph.}
#'
#' @importFrom igraph group_summary modularity mean_distance V
compare_graphs <- function(...) {
  graphs <- list(...)
  
  if (any(sapply(graphs, is.null))) {
    cat("Cannot compare graphs if one or more of them are NULL.\n")
    return(NULL)
  }
  
  n_graphs <- length(graphs)
  summaries <- vector("list", length = n_graphs)
  modularities <- numeric(n_graphs)
  avg_path_lengths <- numeric(n_graphs)
  
  for (i in 1:n_graphs) {
    cat("\nGraph", i, ":\n")
    summaries[[i]] <- group_summary(graphs[[i]])
    modularities[i] <- modularity(graphs[[i]], V(graphs[[i]])$group)
    avg_path_lengths[i] <- mean_distance(graphs[[i]], directed = FALSE)
  }
  
  cat("\nModularity scores:\n")
  for (i in 1:n_graphs) {
    cat("Graph", i, "modularity:", modularities[i], "\n")
  }
  
  cat("\nAverage path length:\n")
  for (i in 1:n_graphs) {
    cat("Graph", i, "average path length:", avg_path_lengths[i], "\n")
  }
  
  cat("\nGroup overlaps:\n")
  max_groups <- max(sapply(summaries, function(x) length(x$group_list)))
  
  for (i in 1:max_groups) {
    cat("Overlap between group", i, "in all graphs:\n")
    overlaps <- Reduce(intersect, lapply(summaries, function(x) x$group_list[[i]]))
    cat(overlaps, "\n")
  }
}


#' Compute overlap between two graphs
#'
#' This function computes the overlap between two graphs and returns a list with information about the overlap.
#'
#' @param graph1 A graph object.
#' @param graph2 A graph object.
#'
#' @return A list with the following elements:
#' \itemize{
#'     \item \code{common_nodes}: A character vector of node names that are present in both graphs.
#'     \item \code{common_edges}: A graph object representing the edges that are present in both graphs.
#'     \item \code{num_common_nodes}: The number of nodes that are present in both graphs.
#'     \item \code{num_common_edges}: The number of edges that are present in both graphs.
#' }
#'
#' @details
#' The function first checks if either of the input graphs are null or empty. If either graph is null or has no nodes or edges, the function returns a message and NULL. The function then computes the overlap between the two graphs and returns a list with information about the overlap. The overlap is defined as the set of nodes and edges that are present in both graphs.
#'
network_overlap <- function(graph1, graph2) {
  if (is.null(graph1) || is.null(graph2) || vcount(graph1) == 0 || ecount(graph1) == 0 || vcount(graph2) == 0 || ecount(graph2) == 0) {
    cat("Cannot compute overlap for NULL or empty graphs.\n")
    return(NULL)
  }
  
  common_nodes <- intersect(V(graph1)$name, V(graph2)$name)
  common_edges <- intersection(graph1, graph2, byname = TRUE)
  
  overlap <- list(
    common_nodes = common_nodes,
    common_edges = common_edges,
    num_common_nodes = length(common_nodes),
    num_common_edges = ecount(common_edges)
  )
  
  return(overlap)
}


#' Calculate centrality measures and other metrics
#'
#' This function calculates various centrality measures and other metrics for a given graph. The function takes a graph object, as created by the \code{\link[igraph]{graph_from_edgelist}} or \code{\link[igraph]{graph_from_data_frame}} functions from the \code{igraph} package. The function computes the following measures:
#' 
#' \itemize{
#'   \item Degree centrality: the number of edges that a node has divided by the total number of possible edges.
#'   \item Closeness centrality: the inverse of the sum of the shortest path lengths between a node and all other nodes in the graph.
#'   \item Betweenness centrality: the number of shortest paths that pass through a node, divided by the total number of shortest paths.
#'   \item Eigenvector centrality: a measure of the importance of a node based on the importance of its neighbors.
#'   \item Subgraph centrality: a measure of the centrality of a node based on the centrality of its subgraphs.
#'   \item Clustering coefficient: the fraction of pairs of a node's neighbors that are connected by an edge.
#'   \item Small worldness: a measure of the degree to which a graph is more efficient at information transfer than a random graph with the same degree distribution.
#'   \item Coreness: a measure of the connectivity of a node to highly connected nodes in the graph.
#' }
#' 
#' @param graph A graph object, as created by the \code{\link[igraph]{graph_from_edgelist}} or \code{\link[igraph]{graph_from_data_frame}} functions from the \code{igraph} package.
#' @param normalize A logical value indicating whether the betweenness centrality and eigenvector centrality measures should be normalized.
#' @param label_col The name of the column in the data frame from which to obtain labels for the nodes in the graph. If this parameter is provided, the node labels will be added to the output data frame.
#'
#' @return A data frame containing the calculated centrality measures and other metrics.
#'
#' @import igraph
network_metrics <- function(graph, normalize = TRUE, label_col = NULL) {
  if (is.null(graph) || vcount(graph) == 0 || ecount(graph) == 0) {
    cat("Cannot compute metrics for a NULL or empty graph.\n")
    return(NULL)
  }
  
  # Centrality measures
  degree_centrality <- degree(graph) / (vcount(graph) - 1)
  closeness_centrality <- closeness(graph)
  betweenness_centrality <- betweenness(graph, normalized = normalize)
  eigenvector_centrality <- evcent(graph, scale = normalize)$vector
  subgraph_centrality <- subgraph_centrality(graph)
  
  # Other metrics
  clustering_coeff <- transitivity(graph, type = "undirected")
  
  # Calculate small worldness using igraph
  rand_graph <- random.graph.game(vcount(graph), p=mean(degree(graph))/(vcount(graph)-1), directed = FALSE)
  sw <- (mean(transitivity(graph, type = "undirected"))/mean(transitivity(rand_graph, type = "undirected"))) / (mean_distance(graph)/mean_distance(rand_graph))
  
  core <- coreness(graph)
  
  if (!is.null(label_col) && label_col %in% colnames(df)) {
    V(graph)$label <- df[[label_col]]
  }
  
  metrics_df <- data.frame(
    name = V(graph)$name,
    group = V(graph)$group,
    degree_centrality = degree_centrality,
    closeness_centrality = closeness_centrality,
    betweenness_centrality = betweenness_centrality,
    eigenvector_centrality = eigenvector_centrality,
    subgraph_centrality = subgraph_centrality,
    clustering_coefficient = clustering_coeff,
    small_worldness = sw,
    coreness = core
  )
  
  return(metrics_df)
}

#' Save network graph
#'
#' This function saves a network graph to a file in PDF, PNG, JPEG, TIFF, or BMP format.
#'
#' @param graph A graph object. It should not be NULL and should contain at least one vertex and one edge.
#' @param file_name A character string specifying the name of the output file.
#' @param format A character string specifying the format of the output file. The default is "pdf".
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @return NULL if the graph is NULL
save_graph <- function(graph, file_name, format = "pdf") {
  if (is.null(graph) || vcount(graph) == 0 || ecount(graph) == 0) {
    cat("Cannot save a NULL or empty graph.\n")
    return(NULL)
  }
  
  if (tolower(format) %in% c("pdf", "png", "jpeg", "tiff", "bmp")) {
    g <- plot_graph(graph)
    ggsave(file_name, g, device = format, dpi = 300)
  } else {
    cat("Unsupported file format. Supported formats are: PDF, PNG, JPEG, TIFF, and BMP.\n")
  }
}

#' UAT Example: Generate and analyze synthetic network
#' 
#' This script generates a synthetic network using randomly generated edge weights and performs various analyses on the network. 
#' 
#' @import igraph
#' @importFrom ggplot2 ggtitle
#'
#' @examples
#' # Generate synthetic dataset
#' set.seed(123)
#' 
#' example_df <- data.frame(
#'   source = c("A", "A", "B", "C", "C", "D", "E", "E"),
#'   target = c("B", "C", "C", "D", "E", "E", "F", "G"),
#'   weight = c(1, 5, 3, 4, 2, 1, 3, 6)
#' )
#' 
#' example_graph <- create_graph(
#'   df = example_df,
#'   from_col = "source",
#'   to_col = "target",
#'   directed = FALSE,
#'   weight_col = "weight"
#' )
#' 
#' # Summarize graph
#' summary_info <- group_summary(example_graph)
#' 
#' # Plot graph
#' graph_plot <- plot_graph(example_graph, title = "Synthetic Network")
#' graph_plot
#' 
#' # Look at distribution of degrees
#' degree_dist_plot <- plot_degree_distribution(graph, title = "Degree Distribution (Synthetic Network)")
#' 
#' # Compare graph (specifying 1 graph)
#' compare_graphs(example_graph)
#' 
#' # Create a second data set
#' example_df2 <- data.frame(
#'   source = c("A", "A", "B", "C", "C", "D", "E", "F"),
#'   target = c("B", "C", "C", "D", "E", "E", "F", "B"),
#'   weight = c(2, 6, 6, 1, 2, 2, 4, 1)
#' )
#'
#' # Create second graph
#' example_graph2 <- create_graph(
#'   df = example_df2,
#'   from_col = "source",
#'   to_col = "target",
#'   directed = FALSE,
#'   weight_col = "weight"
#' )
#' 
#' # Compare two networks
#' compare_graphs(example_graph, example_graph2)
#' 
#' # Assess network overlap
#' overlap <- network_overlap(example_graph, example_graph2)
#' 
#' # Calculate network metrics
#' metric_values <- network_metrics(example_graph)
#' 
#' # Test save_graph function
#' # Uncomment the following line to save the graph as a PDF
#' # save_graph(graph, "synthetic_network.pdf")
#'
#' @export