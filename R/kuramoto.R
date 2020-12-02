#' Implementation to simulate a Kuramoto model of oscillators
#'
#' Simulate Kuramoto model on a ground truth network
#'
#' @param input_matrix The input (ground-truth) adjacency matrix of a graph with `N` nodes. Must be valid square adjacency matrix.
#' @param L Integer length of the desired time series.
#' @param dt Float size of timestep for numerical integration.
#' @param strength Float coupling strength (prefactor for interaction terms).
#' @param phases Vector of of initial phases.
#' @param freqs Vector of internal frequencies.
#' @return An N * L array of synthetic time series data.
#' @export
simulate_kuramoto <- function(input_matrix, L, dt = 0.01, strength = 0, phases = NULL, freqs = NULL) {
  # create return list
  results <- list()

  # get num of nodes in adj matrix
  N <- ncol(input_matrix)

  theta_0 <- NULL
  omega <- NULL

  if (!is.null(phases)) {
    if (length(phases) != N) {
      stop("Initial conditions must be None or lists of length N.")
    }
    theta_0 <- phases
  }
  else {
    theta_0 <- 2 * pi * stats::runif(N)
  }

  if (!is.null(freqs)) {
    if (length(freqs) != N) {
      stop("Initial conditions must be None or lists of length N.")
    }
    omega <- freqs
  }
  else {
    omega <- stats::runif(N, min = .9, max = 1.1)
  }

  times <- seq(dt, L * dt, length.out = L)
  one <- rep(1, N)
  ddt_theta <- function(t, y, parms) {
    prefactor <- strength / N
    first <- outer(one, y)
    second <- outer(y, one)
    list(omega + (prefactor * (input_matrix * sin(first - second)) %*% one)) # return the rate of change
  }

  parms <- c(g = omega, strength = strength, input_matrix = input_matrix)
  init <- theta_0
  out <- deSolve::ode(y = init, times = times, func = ddt_theta, parms = parms)
  out <- out[, -1]
  ts <- out
  ts <- ts %% (2 * pi)
  ts <- pracma::flipdim(aperm(ts))
  rownames(ts) <- NULL

  structure(
    list(
      TS = ts,
      ground_truth = input_matrix,
      internal_frequencies = omega
    ),
    class = "kuramoto"
  )
}
