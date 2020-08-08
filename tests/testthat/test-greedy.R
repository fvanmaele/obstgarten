test_that("cart_greedy: all arguments except XY", {
  XY <- generate_sin_data(10)
  expect_error(cart_greedy(XY, depth = 0))
  expect_error(cart_greedy(XY, depth = "a"))
  expect_error(cart_greedy(XY, threshold = 0))
  expect_error(cart_greedy(XY, threshold = F))
  expect_error(cart_greedy(XY, samle = "True"))
  expect_error(cart_greedy(XY, random = "True"))
})

test_that("cart_greedy: argument XY", {
  expect_error(cart_greedy(1))
  expect_error(cart_greedy(matrix(1:4, 1)))
  expect_error(cart_greedy(matrix(c(1,1,1,2,1,2), 2, 3)))

  XY <- generate_sin_data(10)
  expect_s3_class(cart_greedy(XY), "Baum")
  expect_s3_class(cart_greedy(XY), "R6")
})

test_that("cart_part", {
  XY <- generate_sin_data(10)
  res <- cart_part(0.5, 1,XY)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("A1", "A2"))
  expect_equal(ncol(XY), ncol(res[[1]]))
  expect_gt(nrow(XY), nrow(res[[1]]))
  expect_equal(ncol(XY), ncol(res[[2]]))
  expect_gt(nrow(XY), nrow(res[[2]]))
  expect_equal(colnames(XY), colnames(res[[1]]))
  expect_equal(colnames(XY), colnames(res[[2]]))
})

test_that("cart_grid", {
  XY <- generate_sin_data(10)
  res <- cart_grid(XY, ncol(XY)-1, R_hat)
  expect_equal(dim(res), c(nrow(XY), 2, ncol(XY)-1))
  expect_equal(dimnames(res[,,1])[[2]], c("s", "R"))
})

test_that("R_hat and C_hat", {
  y1 <- sample(-10:10, 10, replace = FALSE)/10
  y2 <- sample(-10:10, 10, replace = FALSE)/10
  R_res <- R_hat(y1, y2)
  C_res <- C_hat(y1, y2)
  expect_true(is.numeric(R_res))
  expect_length(R_res, 1)
  expect_true(is.numeric(C_res))
  expect_length(C_res, 1)
})

test_that("R_min", {
  XY <- generate_sin_data(10)
  res <- R_min(XY, ncol(XY)-1)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("j", "s"))
})
