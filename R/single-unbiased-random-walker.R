#' Implementation of voter model dynamics on a network in R.
#'
#' Simulate voter-model-style dynamics on a network.
#' Nodes are randomly assigned a state in (-1,1) at each
#' time step all nodes asynchronously update by choosing their new
#' state uniformly from their neighbors.
#' Generates an N*L time series.
#' The results dictionary also stores the time series as TS and ground truth adjacency matrix as ground_truth.
#'
#' @param input_matrix the input (ground-truth) graph with N nodes. Must be valid square adjacency matrix.
#' @param L the length of the desired time series.
#' @param initial_node starting node for walk
#' @return results a list with TS matrix an N*L array of synthetic time series data.
#' @export
single_unbiased_random_walker <- function(input_matrix, L, initial_node = NULL) {
  # create return list
  results <- list()

  # get adj matrix and set up vector of indices
  G <- igraph::graph_from_adjacency_matrix(input_matrix, weighted = TRUE)
  A <- igraph::get.adjacency(G)
  N <- unlist(DIM(igraph::V(G)))

  # place walker at init location and walk
  if (is.null(initial_node)) {
    W <- igraph::random_walk(G, sample(1:N, 1), steps = L)
  } else {
    W <- igraph::random_walk(G, initial_node, steps = L)
  }

  # turn into binary-valued
  TS <- matrix(rep(0, N * L), nrow = N)

  for (i in 1:length(W)) {
    TS[(W[i]), i] <- 1
  }

  structure(
    list(
      TS = TS,
      node_index_sequence = W,
      ground_truth = input_matrix
    ),
    class = "single-unbiased-random-walker"
  )
}
