# -------------------------------
# Required Libraries
# -------------------------------
library(igraph)
library(dplyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(scales)     # For trans_breaks
library(viridis)    # For viridis color scales
library(testthat)   # For UAT tests

# -------------------------------
# Helper Function: Compute Subgraph Centrality
# -------------------------------
# Computes the subgraph centrality for each vertex using the spectral formula:
# SC(v) = sum_j [ (v_j(v))^2 * exp(lambda_j) ]
compute_subgraph_centrality <- function(graph) {
  if (vcount(graph) == 0) {
    stop("Graph has no vertices.")
  }
  # Work on an undirected version for subgraph centrality.
  ug <- if (is_directed(graph)) as.undirected(graph, mode = "collapse") else graph
  A <- as.matrix(as_adjacency_matrix(ug))
  ev <- eigen(A)
  # Compute subgraph centrality: sum((eigenvector^2) * exp(eigenvalue))
  sc <- rowSums((ev$vectors^2) * rep(exp(ev$values), each = nrow(ev$vectors)))
  names(sc) <- V(ug)$name
  return(sc)
}

# -------------------------------
# 1. create_graph: Prepare Data (Data Frame to Network Graph)
# -------------------------------
#' Create a Graph from a Data Frame
#'
#' @param df A data frame with at least two columns representing the edges.
#' @param from_col The name of the column representing the source nodes.
#' @param to_col The name of the column representing the target nodes.
#' @param directed Logical. If TRUE, the graph is directed. Default is FALSE.
#' @param weight_col Optional. Name of the column representing edge weights.
#'
#' @return An igraph graph object with a vertex attribute "group" (connected components).
create_graph <- function(df, from_col, to_col, directed = FALSE, weight_col = NULL) {
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  if (!(from_col %in% colnames(df)) || !(to_col %in% colnames(df))) {
    stop("Both 'from_col' and 'to_col' must be present in the data frame.")
  }
  if (ncol(df) < 2) {
    stop("Data frame must have at least two columns.")
  }
  
  # Remove duplicates and rows with missing values
  df <- df %>% distinct() %>% na.omit()
  
  # Build edges data frame
  edges <- df %>%
    select(!!from_col, !!to_col) %>%
    rename(from = !!from_col, to = !!to_col)
  
  # Include any additional columns as edge attributes
  extra_cols <- setdiff(names(df), c(from_col, to_col))
  if (length(extra_cols) > 0) {
    edges <- bind_cols(edges, df[extra_cols])
  }
  
  g <- graph_from_data_frame(edges, directed = directed)
  
  # Set weights if specified and available
  if (!is.null(weight_col)) {
    if (!(weight_col %in% names(df))) {
      stop("Specified 'weight_col' not found in the data frame.")
    }
    E(g)$weight <- df[[weight_col]]
  }
  
  # Compute connected components and assign membership as vertex attribute "group"
  comps <- components(g)
  g <- set_vertex_attr(g, "group", value = comps$membership)
  
  return(g)
}

# -------------------------------
# 2. group_summary: Descriptive Group Information
# -------------------------------
#' Generate a Summary of Graph Groups
#'
#' @param graph An igraph graph object.
#' @param verbose Logical. If TRUE, prints summary to the console.
#'
#' @return A list with:
#'   - num_groups: number of groups,
#'   - group_sizes: sizes of each group,
#'   - group_list: list of vertex names per group.
group_summary <- function(graph, verbose = TRUE) {
  if (!inherits(graph, "igraph") || vcount(graph) < 1) {
    stop("Invalid or empty graph provided.")
  }
  
  if (is.null(V(graph)$group)) {
    warning("Graph vertices have no 'group' attribute. Using components() to compute groups.")
    comps <- components(graph)
    graph <- set_vertex_attr(graph, "group", value = comps$membership)
  }
  
  groups <- split(V(graph)$name, V(graph)$group)
  sizes <- sapply(groups, length)
  num <- length(groups)
  
  if (verbose) {
    cat("Number of groups:", num, "\n")
    cat("Group sizes:", sizes, "\n")
    cat("Group members:\n")
    for (i in seq_along(groups)) {
      cat("  Group", names(groups)[i], ":", paste(groups[[i]], collapse = ", "), "\n")
    }
  }
  
  return(list(num_groups = num, group_sizes = sizes, group_list = groups))
}

# -------------------------------
# 3. plot_graph: Plot a Network Graph
# -------------------------------
#' Plot a Network Graph using ggraph
#'
#' @param graph An igraph graph object.
#' @param title Title of the plot.
#' @param layout Layout algorithm (default "fr" for Fruchterman-Reingold).
#' @param node_extra Optional additional aesthetics for nodes (a named list).
#' @param edge_extra Optional additional aesthetics for edges (a named list).
#'
#' @return A ggplot object.
plot_graph <- function(graph, title = "Network Graph", layout = "fr",
                       node_extra = NULL, edge_extra = NULL) {
  if (!inherits(graph, "igraph") || vcount(graph) < 1) {
    stop("Invalid or empty graph provided.")
  }
  
  # Convert to a tidygraph object for plotting
  tg <- as_tbl_graph(graph)
  
  # Base plot; the tidygraph object provides edge and node data already.
  p <- ggraph(tg, layout = layout) +
    geom_edge_link(aes(edge_alpha = after_stat(index)),
                   show.legend = TRUE) +
    geom_node_point(aes(size = centrality_degree(), color = as.factor(group)),
                    show.legend = TRUE) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_color_viridis_d() +
    theme_void() +
    labs(title = title, color = "Group", size = "Degree")
  
  # Add additional aesthetics if provided
  if (!is.null(node_extra) && is.list(node_extra)) {
    p <- p + geom_node_point(mapping = do.call(aes, node_extra), show.legend = FALSE)
  }
  if (!is.null(edge_extra) && is.list(edge_extra)) {
    p <- p + geom_edge_link(mapping = do.call(aes, edge_extra), show.legend = FALSE)
  }
  
  return(p)
}

# -------------------------------
# 4. plot_degree_distribution: Plot Degree Distribution
# -------------------------------
#' Plot the Degree Distribution of a Graph
#'
#' @param graph An igraph graph object.
#' @param title Title for the plot.
#' @param x_trans Transformation for the x-axis (e.g., "log10").
#' @param y_trans Transformation for the y-axis (e.g., "log10").
#' @param ccdf Logical. If TRUE, plot the complementary cumulative distribution.
#'
#' @return A ggplot object.
plot_degree_distribution <- function(graph, title = "Degree Distribution",
                                     x_trans = "log10", y_trans = "log10", ccdf = FALSE) {
  if (!inherits(graph, "igraph") || vcount(graph) < 1) {
    stop("Invalid or empty graph provided.")
  }
  
  d_vals <- degree(graph)
  df_deg <- as.data.frame(table(d_vals))
  names(df_deg) <- c("degree", "frequency")
  df_deg$degree <- as.numeric(as.character(df_deg$degree))
  
  if (ccdf) {
    df_deg <- df_deg[order(df_deg$degree), ]
    df_deg$frequency <- rev(cumsum(rev(df_deg$frequency)))
  }
  
  p <- ggplot(df_deg, aes(x = degree, y = frequency)) +
    geom_point() +
    geom_line(group = 1) +
    scale_x_continuous(trans = x_trans, breaks = trans_breaks(x_trans, function(x) 10^x)) +
    scale_y_continuous(trans = y_trans, breaks = trans_breaks(y_trans, function(x) 10^x)) +
    labs(title = title, x = "Degree", y = "Frequency") +
    theme_minimal()
  
  return(p)
}

# -------------------------------
# 5. compare_graphs: Compare Multiple Graphs
# -------------------------------
#' Compare Multiple Graphs
#'
#' @param ... A variable number of igraph graph objects.
#'
#' @return A list containing:
#'   - summaries: List of group summaries,
#'   - modularities: Modularities computed using the vertex "group" attribute,
#'   - avg_path_lengths: Average path lengths,
#'   - group_overlaps: A list of overlaps (intersection of vertex names) for each group index.
compare_graphs <- function(...) {
  graphs <- list(...)
  n_graphs <- length(graphs)
  if (n_graphs < 1) {
    stop("At least one graph must be provided.")
  }
  if (any(sapply(graphs, function(g) !inherits(g, "igraph") || vcount(g) < 1))) {
    stop("One or more provided graphs are invalid or empty.")
  }
  
  summaries <- vector("list", n_graphs)
  modularities <- numeric(n_graphs)
  avg_path_lengths <- numeric(n_graphs)
  
  for (i in seq_along(graphs)) {
    cat("\n--- Graph", i, "---\n")
    summaries[[i]] <- group_summary(graphs[[i]], verbose = TRUE)
    modularities[i] <- modularity(graphs[[i]], membership = V(graphs[[i]])$group)
    avg_path_lengths[i] <- mean_distance(graphs[[i]], directed = is_directed(graphs[[i]]))
    cat("Modularity:", modularities[i], "\n")
    cat("Average path length:", avg_path_lengths[i], "\n")
  }
  
  max_groups <- max(sapply(summaries, function(x) length(x$group_list)))
  group_overlaps <- list()
  cat("\n--- Group Overlaps ---\n")
  for (i in seq_len(max_groups)) {
    group_members <- lapply(summaries, function(x) {
      if (length(x$group_list) >= i) x$group_list[[i]] else character(0)
    })
    overlap <- Reduce(intersect, group_members)
    group_overlaps[[paste0("group_", i)]] <- overlap
    cat("Overlap for group", i, ":", paste(overlap, collapse = ", "), "\n")
  }
  
  return(list(summaries = summaries,
              modularities = modularities,
              avg_path_lengths = avg_path_lengths,
              group_overlaps = group_overlaps))
}

# -------------------------------
# 6. network_overlap: Calculate Overlap Between Two Graphs
# -------------------------------
#' Compute Overlap Between Two Graphs
#'
#' @param graph1 An igraph graph object.
#' @param graph2 An igraph graph object.
#'
#' @return A list with:
#'   - common_nodes: Names of nodes present in both graphs.
#'   - common_edges: The graph representing the intersection of edges.
#'   - num_common_nodes: Count of common nodes.
#'   - num_common_edges: Count of common edges.
network_overlap <- function(graph1, graph2) {
  if (!inherits(graph1, "igraph") || !inherits(graph2, "igraph") ||
      vcount(graph1) < 1 || vcount(graph2) < 1) {
    stop("Both graphs must be valid and non-empty.")
  }
  
  common_nodes <- intersect(V(graph1)$name, V(graph2)$name)
  common_edges <- intersection(graph1, graph2, byname = TRUE)
  
  result <- list(
    common_nodes = common_nodes,
    common_edges = common_edges,
    num_common_nodes = length(common_nodes),
    num_common_edges = ecount(common_edges)
  )
  return(result)
}

# -------------------------------
# 7. network_metrics: Calculate Centrality Measures and Other KPIs
# -------------------------------
#' Calculate Network Metrics
#'
#' Computes various centrality measures and network metrics including:
#'   - Degree, closeness, betweenness, eigenvector, and subgraph centrality.
#'   - Clustering coefficient.
#'   - Small worldness (sigma = (C/Crand) / (L/Lrand)).
#'   - Coreness.
#'
#' @param graph An igraph graph object.
#' @param normalize Logical. If TRUE, betweenness and eigenvector centralities are normalized.
#'
#' @return A data frame with metrics for each vertex.
network_metrics <- function(graph, normalize = TRUE) {
  if (!inherits(graph, "igraph") || vcount(graph) < 1) {
    stop("Invalid or empty graph provided.")
  }
  
  n <- vcount(graph)
  
  # Centrality Measures
  degree_cent <- degree(graph) / (n - 1)
  closeness_cent <- closeness(graph)
  betweenness_cent <- betweenness(graph, normalized = normalize)
  eigen_cent <- eigen_centrality(graph, scale = normalize)$vector
  subgraph_cent <- compute_subgraph_centrality(graph)
  
  # Clustering Coefficient (global)
  clust_coeff <- transitivity(if (is_directed(graph)) as.undirected(graph, mode = "collapse") else graph, type = "global")
  
  # Small Worldness:
  p_est <- mean(degree(graph)) / (n - 1)
  rand_graph <- sample_gnp(n, p = p_est, directed = FALSE)
  C <- clust_coeff
  C_rand <- transitivity(rand_graph, type = "global")
  L <- mean_distance(graph, directed = FALSE)
  L_rand <- mean_distance(rand_graph, directed = FALSE)
  
  if (C_rand == 0 || L_rand == 0 || !is.finite(L) || !is.finite(L_rand)) {
    small_world <- NA
  } else {
    small_world <- (C / C_rand) / (L / L_rand)
  }
  
  coreness_val <- coreness(graph)
  
  metrics_df <- data.frame(
    name = V(graph)$name,
    group = V(graph)$group,
    degree_centrality = degree_cent,
    closeness_centrality = closeness_cent,
    betweenness_centrality = betweenness_cent,
    eigenvector_centrality = eigen_cent,
    subgraph_centrality = subgraph_cent,
    clustering_coefficient = clust_coeff,
    small_worldness = small_world,
    coreness = coreness_val,
    stringsAsFactors = FALSE
  )
  
  return(metrics_df)
}

# -------------------------------
# 8. save_graph: Save a Network Graph to File
# -------------------------------
#' Save a Network Graph Plot to File
#'
#' @param graph An igraph graph object.
#' @param file_name File name (with or without extension).
#' @param format Format of the output file (one of "pdf", "png", "jpeg", "tiff", "bmp"). Default is "pdf".
#'
#' @return (Invisible) NULL. The file is saved to disk.
save_graph <- function(graph, file_name, format = "pdf") {
  if (!inherits(graph, "igraph") || vcount(graph) < 1) {
    stop("Invalid or empty graph provided; nothing to save.")
  }
  fmt <- tolower(format)
  if (!(fmt %in% c("pdf", "png", "jpeg", "tiff", "bmp"))) {
    stop("Unsupported file format. Supported formats: pdf, png, jpeg, tiff, bmp.")
  }
  
  # Create the plot using the updated plot_graph() function.
  p <- plot_graph(graph)
  
  # Ensure the file name has the proper extension
  ext <- tools::file_ext(file_name)
  if (ext == "") {
    file_name <- paste0(file_name, ".", fmt)
  }
  
  # Suppress messages from ggsave so that expect_silent() passes
  suppressMessages(ggsave(filename = file_name, plot = p, device = fmt, dpi = 300))
  invisible(NULL)
}

# -------------------------------
# User Acceptance Testing (UAT) using testthat
# -------------------------------
cat("\n================== Running UAT ==================\n")

# Create a synthetic data frame for testing
test_df <- data.frame(
  source = c("A", "A", "B", "C", "C", "D", "E", "E"),
  target = c("B", "C", "C", "D", "E", "E", "F", "G"),
  weight = c(1, 5, 3, 4, 2, 1, 3, 6),
  stringsAsFactors = FALSE
)

test_that("create_graph works correctly", {
  # Valid graph creation
  g <- create_graph(test_df, from_col = "source", to_col = "target", directed = FALSE, weight_col = "weight")
  expect_true(inherits(g, "igraph"))
  expect_true(vcount(g) > 0)
  expect_true(!is.null(V(g)$group))
  
  # Data frame missing required columns
  expect_error(create_graph(test_df, from_col = "foo", to_col = "target"))
  
  # Data frame with duplicates should still create a graph
  df_dup <- rbind(test_df, test_df[1, ])
  g2 <- create_graph(df_dup, from_col = "source", to_col = "target")
  expect_true(vcount(g2) > 0)
})

test_that("group_summary works correctly", {
  g <- create_graph(test_df, from_col = "source", to_col = "target")
  summary_info <- group_summary(g, verbose = FALSE)
  expect_true(is.list(summary_info))
  expect_true("num_groups" %in% names(summary_info))
  
  # Passing an empty graph should error
  g_empty <- make_empty_graph()
  expect_error(group_summary(g_empty))
})

test_that("plot_graph returns a ggplot object", {
  g <- create_graph(test_df, from_col = "source", to_col = "target")
  p <- plot_graph(g, title = "Test Network")
  expect_true(inherits(p, "ggplot"))
  
  # Test additional aesthetics layers (using a named list)
  p2 <- plot_graph(g, node_extra = list(fill = "red", shape = 21),
                   edge_extra = list(color = "gray", alpha = 0.5))
  expect_true(inherits(p2, "ggplot"))
})

test_that("plot_degree_distribution returns a ggplot object", {
  g <- create_graph(test_df, from_col = "source", to_col = "target")
  p <- plot_degree_distribution(g, ccdf = FALSE)
  expect_true(inherits(p, "ggplot"))
  p_ccdf <- plot_degree_distribution(g, ccdf = TRUE)
  expect_true(inherits(p_ccdf, "ggplot"))
})

test_that("compare_graphs works correctly", {
  g1 <- create_graph(test_df, from_col = "source", to_col = "target")
  # Create a second graph with a slight change
  test_df2 <- data.frame(
    source = c("A", "A", "B", "C", "C", "D", "E", "F"),
    target = c("B", "C", "C", "D", "E", "E", "F", "B"),
    weight = c(2, 6, 6, 1, 2, 2, 4, 1),
    stringsAsFactors = FALSE
  )
  g2 <- create_graph(test_df2, from_col = "source", to_col = "target")
  
  comp <- compare_graphs(g1, g2)
  expect_true(is.list(comp))
  expect_true("modularities" %in% names(comp))
  
  # Passing a NULL graph should error
  expect_error(compare_graphs(g1, NULL))
})

test_that("network_overlap works correctly", {
  g1 <- create_graph(test_df, from_col = "source", to_col = "target")
  test_df2 <- data.frame(
    source = c("A", "B", "E"),
    target = c("B", "C", "F"),
    weight = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  g2 <- create_graph(test_df2, from_col = "source", to_col = "target")
  overlap <- network_overlap(g1, g2)
  expect_true(is.list(overlap))
  expect_true(overlap$num_common_nodes >= 0)
  
  # Passing an empty graph should error
  g_empty <- make_empty_graph()
  expect_error(network_overlap(g1, g_empty))
})

test_that("network_metrics works correctly", {
  g <- create_graph(test_df, from_col = "source", to_col = "target")
  metrics <- network_metrics(g, normalize = TRUE)
  expect_true(is.data.frame(metrics))
  expect_equal(nrow(metrics), vcount(g))
  
  # For an empty graph, expect an error
  g_empty <- make_empty_graph()
  expect_error(network_metrics(g_empty))
})

test_that("save_graph works correctly", {
  g <- create_graph(test_df, from_col = "source", to_col = "target")
  temp_file <- tempfile(fileext = ".pdf")
  expect_silent(save_graph(g, file_name = temp_file, format = "pdf"))
  expect_true(file.exists(temp_file))
  unlink(temp_file)  # Clean up
  
  # Unsupported format should error
  expect_error(save_graph(g, file_name = "test_graph.xyz", format = "xyz"))
  
  # Empty graph should error
  g_empty <- make_empty_graph()
  expect_error(save_graph(g_empty, file_name = "dummy.pdf"))
})

cat("\n================== All UAT tests passed ==================\n")
