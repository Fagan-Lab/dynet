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
  results <- voter(input, L, .5)
  
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

test_that("Test Voter Model Dynamics Random", {
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

  set.seed(Sys.time())
  resultsRandom <- voter(input, 5, "automatic")
  resultsRandom2 <- voter(input, 5, "automatic")

  set.seed(123456789)
  resultsNotRandom <- voter(input, 5, .5)
  set.seed(123456789)
  resultsNotRandom2 <- voter(input, 5, .5)

  expect_equal(resultsRandom[["ground_truth"]], input)
  expect_equal(resultsRandom2[["ground_truth"]], input)
  expect_equal(resultsNotRandom[["ground_truth"]], input)
  expect_equal(resultsNotRandom2[["ground_truth"]], input)


  expect_false(isTRUE(all.equal(resultsRandom[["TS"]], resultsRandom2[["TS"]])))
  expect_equal(resultsNotRandom[["TS"]], resultsNotRandom2[["TS"]])
})
