test_that("returning the ratio works", {
  set.seed(12)
  expect_equal(infl_pt(x = 1, y = c(
    rnorm(100, mean = 2, sd = 3), rnorm(100, mean = 10, sd = 3)
  )), 6.652001)
})

test_that("no min found when all ratios are equivalent", {
  set.seed(12)
  expect_equal(infl_pt(x = 1, y = rep(1, 10)
  ), NA_integer_, ignore_attr=TRUE)
})

