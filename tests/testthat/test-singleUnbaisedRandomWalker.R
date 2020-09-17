context("Testing Single Unbaised Random Walker Dynamics")

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

  L <- 5
  results <- singleUnbaisedRandomWalker(input, L)

  expect_equal(results[["ground_truth"]], input)
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

  L <- 20
  results <- singleUnbaisedRandomWalker(input, 5)

  expect_equal(ncol(results[["TS"]]), L)
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

  L <- 5

  set.seed(Sys.time())
  resultsRandom <- singleUnbaisedRandomWalker(input, 4)
  resultsRandom2 <- singleUnbaisedRandomWalker(input, 4)

  set.seed(123456789)
  resultsNotRandom <- singleUnbaisedRandomWalker(input, 4)
  set.seed(123456789)
  resultsNotRandom2 <- singleUnbaisedRandomWalker(input, 5)

  expect_equal(resultsRandom[["ground_truth"]], input)
  expect_equal(resultsRandom2[["ground_truth"]], input)
  expect_equal(resultsNotRandom[["ground_truth"]], input)
  expect_equal(resultsNotRandom2[["ground_truth"]], input)


  expect_false(isTRUE(all.equal(resultsRandom[["TS"]], resultsRandom2[["TS"]])))
  expect_equal(resultsNotRandom[["TS"]], resultsNotRandom2[["TS"]])
})
