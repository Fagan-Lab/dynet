#' Implementation to simulate a Ising-Glauber model of oscillators
#'
#' Simulate Ising-Glauber model on a ground truth network
#'
#' @param input_matrix The input (ground-truth) adjacency matrix of a graph with N nodes. Must be valid N*N square adjacency matrix.
#' @param L Integer length of the desired time series.
#' @param init Vector initial condition, which must have binary value (0 or 1) and must have length N
#' @param beta Float  Inverse temperature tuning the likelihood that a node switches its state. Default to 2.
#' @return An N * L observations on N nodes.
#' @export
simulate_ising <- function(input_matrix, L, init = NULL, beta = 2) {
  # get num of nodes in adj matrix
  N <- ncol(input_matrix)
  degs <- sum(input_matrix)
  ts <- matrix(data = 0, nrow = N, ncol = L)
  if (is.null(init)) {
    init <- stats::runif(N)
  }
  ts[, 1] <- round(init)
  # simulate time series
  for (t in 1:(L - 1)) {
    state <- ts[, t]
    num_act_nei <- state %*% input_matrix
    hamltn <- (degs - 2 * num_act_nei) / degs
    thrds <- 1 / (1 + exp(beta * hamltn))
    # probabilty of switching state
    probs <- ifelse(state == 1, thrds, 1 - thrds)
    next_ <- ifelse(stats::runif(N) < probs, 1 - state, state)
    ts[, t + 1] <- next_
  }

  structure(
    list(
      TS = ts,
      ground_truth = input_matrix
    ),
    class = "ising-glauber"
  )
}
