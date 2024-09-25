test_that("returning the ratio works", {
  set.seed(12)
  df <- data.frame(x = 1, y = c(
    rnorm(100, mean = 2, sd = 3), rnorm(100, mean = 10, sd = 3))
  )
  expect_equal(infl_pt(df, "x", "y"), 6.652001)
})
