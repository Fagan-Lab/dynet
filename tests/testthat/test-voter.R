context("Testing Voter Model Dynamics")

### Voter Model Dynamics --------------------------------------------------
test_that("Test Voter Model Dynamics Ground Truth", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )
  
  L = 5
  results <- voter(input, L)
  
  expect_equal(results[["ground_truth"]], input)
})

test_that("Test Voter Model Dynamics Time Series Dimensions", {
  input <- matrix(
    cbind(
      c(1.0, 1.0, 2.0, 3.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )
  
    L = 5
    results <- voter(input, 5)
  
  expect_equal(nrow(results[["TS"]]), nrow(input))
  expect_equal(ncol(results[["TS"]]), L)
})
