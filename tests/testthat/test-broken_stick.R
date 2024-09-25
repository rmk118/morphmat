test_that("preliminary broken stick function works", {
  expect_equal(broken_stick(dat=iris, x="x", y="y", method="chngpt"), 1)
})
