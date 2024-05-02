test_that("mu is right", {
  x = myncurve(10,5,6)
  expect_equal(x$mu, 10)
})
test_that("sigma is right", {
  x = myncurve(10,5,6)
  expect_equal(x$sigma, 5)
})
test_that("a is right", {
  x = myncurve(10,5,6)
  expect_equal(x$a, 6)
})
