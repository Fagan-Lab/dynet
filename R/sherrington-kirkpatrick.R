#' Generate an ising model-like time series on a graph
#'
#' Simulate Kinetic Ising model dynamics on a ground truth network
#'
#' @param input_matrix The input (ground-truth) adjacency matrix of a graph with `N` nodes. Must be valid square adjacency matrix.
#' @param L The length of the desired time series.
#' @param noise True or false value to generate noise
#' @return An N * L array of synthetic time series data
#' @export
simulate_sherrington <- function(input_matrix, L, noise = FALSE) {
  # create return list
  results <- list()

  # get num of nodes in igraph obj
  N <- ncol(input_matrix)

  # get transition probability matrix of G
  A <- input_matrix
  W <- matrix(data = 0, nrow = N, ncol = N)
  for (i in 1:nrow(A)) {
    if (sum(A[,i]) > 0) {
      W[,i] = A[,i] / sum(A[,i])
    }
  }

  # init a time series of ones
  ts <- matrix(data = 1, nrow = L, ncol = N)

  # iterate size of time series
  for (i in 2:L-1) {
    h <- sum(W * ts[i,])
    p <- 1 / (1 + exp(-2 * h))
    if (noise) {
      ts[i+1,] = p - stats::rnorm(N)
    }
    else {
      temp = as.vector(p - stats::rnorm(N))
      temp[temp < 0] <- -1.0
      temp[temp >= 0] <- 1.0
      ts[i+1,] = temp
    }
  }

  structure(
    list(
      TS = aperm(ts),
      ground_truth = input_matrix
    ),
    class = "sherrington-kirkpatrick-ising"
  )
}
