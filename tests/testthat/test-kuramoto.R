context("Testing Kuramoto Model Dynamics")

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

SEED_VALUE <- 12345

### Ising Model Dynamics --------------------------------------------------
test_that("Test Kuramoto Model Dynamics Ground Truth", {
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

test_that("Test Kuramoto Model Dynamics Time Series Dimensions", {
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

test_that("Test Kuramoto Model Dynamics Random", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  results_random <- simulate_kuramoto(input, 4)
  results_random2 <- simulate_kuramoto(input, 4)

  expect_equal(results_random$ground_truth, input)
  expect_equal(results_random2$ground_truth, input)

  with_seed(SEED_VALUE, {
    results_not_random <- simulate_kuramoto(input, 4)
    results_not_random2 <- simulate_kuramoto(input, 4)

    expect_equal(results_not_random$ground_truth, input)
    expect_equal(results_not_random2$ground_truth, input)
    expect_false(identical(results_random$TS, results_random2$TS))
  })
})

test_that("Test Kuramoto Model Dynamics Non-Random", {
  input <- matrix(
    cbind(
      c(1, 0, 0),
      c(0, 1, 0),
      c(0, 1, 0)
    ),
    ncol = 3
  )

  func_output <- simulate_kuramoto(input, 3, dt = 1, strength = 1, phases = c(1, 2, 3), freqs = c(1.0, 1.05, 1.06))
  func_output2 <- simulate_kuramoto(input, 3, dt = 1, strength = 1, phases = c(1, 2, 3), freqs = c(1.0, 1.05, 1.06))
  expect_equal(round(func_output$TS, 3), round(func_output2$TS, 3))
})
