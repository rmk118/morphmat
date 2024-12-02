test_that("function returns expected value", {
  set.seed(123)
  fc <- fake_crustaceans(n = 100,
                         L50 = 100,
                         allo_params = c(1, 0.2, 1.1, 0.2))
  expect_equal(round(broken_stick_stevens(
    fc,
    xvar = "x",
    yvar = "y",
    verbose = FALSE
  ), 4), 121.4316)

  x1 <- c(1:100)
  y1 <- c(1:75) * 2 + rnorm(75, 0, 10)
  y2 <- c(76:100) * 4 + rnorm(25, 3, 10)
  testdat <- data.frame(x = x1, y = c(y1, y2))
  expect_equal(broken_stick_stevens(
    testdat,
    xvar = "x",
    yvar = "y",
    verbose = FALSE
  ),
  66)
})
