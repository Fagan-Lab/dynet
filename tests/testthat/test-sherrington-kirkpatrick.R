context("Testing Sherrington Kirk Patrick Kinetic Ising Model Dynamics")

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

SEED_VALUE <- 12345

### Ising Model Dynamics --------------------------------------------------
test_that("Test Kinetic Ising Model Dynamics Ground Truth", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  results <- simulate_sherrington(input, 5, .5)

  expect_equal(results$ground_truth, input)
})

test_that("Test Kinetic Ising Model Dynamics Time Series Dimensions", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  results <- simulate_sherrington(input, 5)

  expect_equal(nrow(results$TS), nrow(input))
  expect_equal(ncol(results$TS), 5)
})

test_that("Test Kinetic Ising Model Dynamics Random", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  results_random <- simulate_sherrington(input, 5)
  results_random2 <- simulate_sherrington(input, 5)

  expect_equal(results_random$ground_truth, input)
  expect_equal(results_random2$ground_truth, input)

  with_seed(SEED_VALUE, {
    results_not_random <- simulate_sherrington(input, 5, .5)
    results_not_random2 <- simulate_sherrington(input, 5, .5)

    expect_equal(results_not_random$ground_truth, input)
    expect_equal(results_not_random2$ground_truth, input)
    expect_false(identical(results_random$TS, results_random2$TS))
  })
})

test_that("Test Kinetic Ising Model Dynamics Noise", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  results_random <- simulate_sherrington(input, 5, noise = TRUE)
  results_random2 <- simulate_sherrington(input, 5)

  expect_equal(results_random$ground_truth, input)
  expect_equal(results_random2$ground_truth, input)

  with_seed(SEED_VALUE, {
    results_not_random <- simulate_sherrington(input, 5, .5)
    results_not_random2 <- simulate_sherrington(input, 5, .5)

    expect_equal(results_not_random$ground_truth, input)
    expect_equal(results_not_random2$ground_truth, input)
    expect_false(identical(results_random$TS, results_random2$TS))
  })
})
