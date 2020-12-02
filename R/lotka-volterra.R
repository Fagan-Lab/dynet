#' Implementation to simulate a Lotka-Volterra model on a network
#'
#' Simulate time series on a network from the Lotka-Volterra model
#'
#' @param input_matrix The input (ground-truth) adjacency matrix of a graph with `N` nodes. Must be valid square adjacency matrix.
#' @param L The length of the desired time series.
#' @param init Initial condition vector. If not specified an initial condition is uniformly generated from 0 to the nodes' carrying capacity.
#' @param gr Growth rate vector. If not specified, default to 1 for all nodes.
#' @param cap Carrying capacity vector. If not specified, default to 1 for all nodes.
#' @param inter N*N Matrix of interaction weights between nodes. If not specified, default to a zero-diagonal matrix whose i,j entry is (j - i) / (N - 1)
#' @param dt Float or vector of sizes of time steps when simulating the continuous-time dynamics.
#' @param stochastic Boolean determining whether to simulate the stochastic or deterministic dynamics.
#' @param pertb Vector of perturbation magnitude of nodes' growth. If not specified, default to 0.01 for all nodes.
#' @return An N * L array of synthetic time series data.
#' @export
simulate_lotka <- function(input_matrix, L, init=NULL, gr=NULL, cap=NULL, inter=NULL, dt=1e-2, stochastic=TRUE, pertb=NULL) {
  # create return list
  results <- list()

  # get num of nodes in adj matrix
  N <- ncol(input_matrix)

  # init the model's parameters if not specified
  if (is.null(gr)) {
    gr <- rep(1, N)
  }
  if (is.null(cap)) {
    cap <- rep(1, N)
  }
  if (is.null(inter)) {
    wei <- 1 / (N - 1)
    full <- matrix(data = wei, nrow = N, ncol = N)
    inter <- matrix(data = 0, nrow = N, ncol = N)
    inter_uppertri <- inter[lower.tri(inter)] <- 0
    inter_lowertri <- inter[upper.tri(inter)] <- 0
    inter <- inter + inter_uppertri - inter_lowertri
  }

  if (is.null(stochastic) && is.null(pertb)) {
    pertb <- 1e-2 * rep(1, N)
  }

  # randomly initialize an initial condition if not specified
  ts <- matrix(data = 0, nrow = N, ncol = L)
  if (is.null(init)) {
    init <- stats::runif(1, min = 0, max = cap)
  }
  ts[,0] <- init

  # define the function of dynamics
  mat <- ifelse(input_matrix == 1, inter, 0.0) + diag(rep(-1, N))
  mat <- mat / cap

  dyn <- function(t, y, parms) {
    list(c(y * (gr + (mat %*% y))))
  }

  # simulate time series
  if (length(dt) == 1) {
    dt <- dt * rep(1, L - 1)
  }

  # deterministic dynamics
  if (stochastic) {
    tryCatch( {
      out <- deTestSet::dopri5(y = init, func = dyn, times = 0:sum(dt), parms = NULL)
      for (i in 1:L-1) {
        ts[i+1,] <- out[dt[i]+1,][1:length(init)+1]
      }
    }, error = function(e) {
      message <- "Integration not successful. Change sizes of time steps or parameters."
    })
  }
  else {
    for (t in 1:L-1) {
      state <- ts[,t]
      next_ <- state + (state * (gr + (mat %*% state)) * dt[t])
      next_ <- next_ + (state * stats::rnorm(1, sd = 4) * sqrt(dt[t]))
      ts[,t+1] <- next
    }
  }
  ts[is.na(ts)] = 0

  structure(
    list(
      TS = aperm(ts),
      ground_truth = input_matrix,
      time_steps = sum(dt)
    ),
    class = "lotka-volterra"
  )
}
