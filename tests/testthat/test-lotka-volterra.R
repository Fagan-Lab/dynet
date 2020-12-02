context("Testing Lotka Volterra Model Dynamics")

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

SEED_VALUE <- 12345

### Ising Model Dynamics --------------------------------------------------
test_that("Test Lotka Volterra Model Dynamics Ground Truth", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  results <- simulate_lotka(input, 5, .5)

  expect_equal(results$ground_truth, input)
})

test_that("Test Lotka Volterra Model Dynamics Time Series Dimensions", {
  input <- matrix(
    cbind(
      c(1, 0, 0),
      c(0, 1, 0),
      c(0, 1, 0)
    ),
    ncol = 3
  )

  results <- simulate_lotka(input, 3)

  expect_equal(nrow(results$TS), nrow(input))
  expect_equal(ncol(results$TS), 3)
})

test_that("Test Lotka Volterra Model Dynamics Stochastic", {
  input <- matrix(
    cbind(
      c(1, 0, 0),
      c(0, 1, 0),
      c(0, 1, 0)
    ),
    ncol = 3
  )

  output <- matrix(
    cbind(
      c(0, 0, 0),
      c(0.9997649, 0.9980487, 0.9993069),
      c(0.9982652, 0.9857588, 0.9949008)
    ),
    ncol = 3
  )

  func_output <- simulate_lotka(input, L = 3, init = c(0.9662720, 0.7750888, 0.9066647), dt = c(5, 3, 1))
  expect_equal(round(func_output$TS, 6), round(output, 6))
  expect_equal(func_output$time_steps, 9)
})

test_that("Test Lotka Volterra Model Dynamics Non-Stochastic", {
  input <- matrix(
    cbind(
      c(1, 0, 0),
      c(0, 1, 0),
      c(0, 1, 0)
    ),
    ncol = 3
  )

  output <- matrix(
    cbind(
      c(0, 0, 0),
      c(0, 0, 0),
      c(0, 0, 0)
    ),
    ncol = 3
  )

  func_output <- simulate_lotka(input,
    L = 3, init = c(0.9662720, 0.7750888, 0.9066647),
    dt = c(5, 3, 1), stochastic = FALSE, inter = c(0, 0, 0)
  )
  expect_equal(func_output$TS, output)
  expect_equal(func_output$ground_truth, input)
  expect_equal(func_output$time_steps, 9)
})
