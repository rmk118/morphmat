test_that("Piecewise regression wrapper works", {
  set.seed(123)
  fc <- fake_crustaceans(n = 100,
                         L50 = 100,
                         allo_params = c(1, 0.2, 1.1, 0.2))
  expect_equal(round(piecewise_mods(
    fc,
    xvar = "x",
    yvar = "y",
    method = "stevens"
  ), 4), 121.4316)
})
