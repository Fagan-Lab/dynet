context("Testing Single Unbaised Random Walker Dynamics")

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

SEED_VALUE <- 12345

### Random Walker Model Dynamics --------------------------------------------------
test_that("Test Random Walker Ground Truth", {
  input <- matrix(
    cbind(
      c(1, 1, 0, 0),
      c(1, 1, 0, 0),
      c(0, 0, 1, 1),
      c(0, 0, 1, 1)
    ),
    nrow = 4
  )

  results <- single_unbiased_random_walker(input, 5)

  expect_equal(results$ground_truth, input)
})

test_that("Test Random Walker Time Series Dimensions", {
  input <- matrix(
    cbind(
      c(1, 1, 0, 0),
      c(1, 1, 0, 0),
      c(0, 0, 1, 1),
      c(0, 0, 1, 1)
    ),
    nrow = 4
  )

  results <- single_unbiased_random_walker(input, 5, 2)

  expect_equal(ncol(results$TS), 5)
})

test_that("Test Random Walker Psuedo Random", {
  input <- matrix(
    cbind(
      c(1, 1, 0, 0),
      c(1, 1, 0, 0),
      c(0, 0, 1, 1),
      c(0, 0, 1, 1)
    ),
    nrow = 4
  )

  results_random <- single_unbiased_random_walker(input, 4)
  results_random2 <- single_unbiased_random_walker(input, 4)

  expect_equal(results_random$ground_truth, input)
  expect_equal(results_random2$ground_truth, input)

  with_seed(SEED_VALUE, {
    results_not_random <- single_unbiased_random_walker(input, 4)
    results_not_random2 <- single_unbiased_random_walker(input, 5)

    expect_equal(results_not_random$ground_truth, input)
    expect_equal(results_not_random2$ground_truth, input)
    expect_equal(length(results_not_random$TS), 16)
    expect_equal(length(results_not_random2$TS), 20)
  })
})
