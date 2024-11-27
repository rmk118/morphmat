test_that("Stevens wrapper works", {
  set.seed(123)
  fc <- fake_crustaceans(n = 100,
                         L50 = 100,
                         allo_params = c(1, 0.2, 1.1, 0.2))
  expect_equal(round(broken_stick(
    fc,
    xvar = "x",
    yvar = "y",
    method = "stevens"
  ), 4), 121.4316)
})
