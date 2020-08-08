test_that("all arguments except XY", {
  XY <- generate_sin_data(10)
  expect_error(cart_greedy(XY, depth = 0))
  expect_error(cart_greedy(XY, depth = "a"))
  expect_error(cart_greedy(XY, threshold = 0))
  expect_error(cart_greedy(XY, threshold = F))
  expect_error(cart_greedy(XY, samle = "True"))
  expect_error(cart_greedy(XY, random = "True"))
})

test_that("argument XY", {
  expect_error(cart_greedy(1))
  expect_error(cart_greedy(matrix(1:4, 1)))
  expect_error(cart_greedy(matrix(c(1,1,1,2,1,2), 2, 3)))

  XY <- generate_sin_data(10)
  expect_s3_class(cart_greedy(XY), "Baum")
  expect_s3_class(cart_greedy(XY), "R6")

})
