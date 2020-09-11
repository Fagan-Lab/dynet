test_that("pipe works", {
  multiply <- function(x, y) {
    x * y
  }

  expect_equal(2 %>% multiply(2), 4)
})
