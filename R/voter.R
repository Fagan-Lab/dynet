#' Implementation of voter model dynamics on a network in R.
#'
#' Simulate voter-model-style dynamics on a network.
#' Nodes are randomly assigned a state in (-1,1) at each
#' time step all nodes asynchronously update by choosing their new
#' state uniformly from their neighbors.
#' Generates an N*L time series.
#' The results dictionary also stores the time series as TS and ground truth adjacency matrix as ground_truth.
#'
#' @param inputMatrix the input (ground-truth) graph with `N` nodes. Must be valid square adjacency matrix.
#' @param L the length of the desired time series.
#' @param noise if noise is present, with this probability a node's state will be randomly redrawn from (-1,1) \cr
#' independent of its neighbors' states. If 'automatic', set noise to 1/N.
#' @return results a list with TS matrix an N*L array of synthetic time series data.


# get dimensions of vertices helper function
DIM <- function(...) {
  args <- list(...)
  lapply(args, function(x) {
    if (is.null(dim(x))) {
      return(length(x))
    }
    dim(x)
  })
}

#' @export
voter <- function(inputMatrix, L, noise = NULL) {
  # create return list
  results <- list()

  # get adj matrix
  G <- igraph::graph_from_adjacency_matrix(inputMatrix, weighted = TRUE)
  # get num of nodes in igraph obj
  N <- unlist(DIM(igraph::V(G)))


  # noise input validation
  if (is.null(noise)) {
    noise <- 0
  }
  if (identical(noise, "automatic") || identical(noise, "auto")) {
    noise <- 1 / N
  }
  if (!is.numeric(noise)) {
    stop("noise must be a number, 'automatic', or NULL")
  }

  # get adj mat
  transitions <- igraph::get.adjacency(G)
  # sum cols of adjmat for n columns
  colSums <- unname(tapply(igraph::E(G)$weight, (seq_along(igraph::E(G)$weight) - 1) %/% ncol(transitions), sum))
  # divide each vertex's weight by sum of edge weights for that vertex
  colSumIter <- 1
  currColSumIter <- 1
  for (i in 1:length(igraph::E(G)$weight)) {
    igraph::E(G)$weight[i] <- igraph::E(G)$weight[i] / colSums[colSumIter]
    if (currColSumIter == ncol(transitions)) {
      currColSumIter <- 0
      colSumIter <- colSumIter + 1
    }
  }

  # generate time series
  TS <- matrix(rep(0, N * L), nrow = N)
  randNum <- runif(N) # random uniform distribution
  for (i in 1:nrow(TS)) {
    if (randNum[i] < .5) {
      TS[i, 1] <- 1
    } else {
      TS[i, 1] <- -1
    }
  }

  # indices to help with random sampling
  indices <- 1:N

  # iterate through time series data and use transition graph weights as sum
  for (t in 2:L) {
    TS[, t] <- TS[, t - 1]
    for (i in indices) {
      TS[i, t] <- sample(c(TS[, t]), 1, replace = FALSE, prob = c(G[i]))
      if (!is.null(noise)) {
        if (runif(1) < noise) {
          if (runif(1) < .5) {
            TS[i, t] <- 1
          } else {
            TS[i, t] <- -1
          }
        }
      }
    }
  }
  results[["ground_truth"]] <- inputMatrix
  results[["TS"]] <- TS

  return(results)
}
