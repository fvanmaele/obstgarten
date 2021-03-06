test_that("cart_greedy: all arguments except XY", {
  XY <- generate_sin_data(10)
  expect_error(cart_greedy(XY, depth = 0))
  expect_error(cart_greedy(XY, depth = "a"))
  expect_error(cart_greedy(XY, threshold = 0))
  expect_error(cart_greedy(XY, threshold = F))
  expect_error(cart_greedy(XY, random = "True"))
})

test_that("cart_greedy: argument XY", {
  expect_error(cart_greedy(1))
  expect_error(cart_greedy(matrix(1:4, 1)))
  expect_error(cart_greedy(matrix(c(1,1,1,2,1,2), 2, 3)))

  # d=1
  XY <- generate_sin_data(10)
  depth_s <-  sample(2:5, 1)
  res <- cart_greedy(XY, depth = depth_s)
  expect_s3_class(res, "Baum")
  expect_s3_class(res, "R6")

  # d>1
  d <- sample(2:5, 1)
  XY <- generate_mult_data(n=50, d=d, sigma=diag(d), mu=rep(0., d))[[1]]
  depth_s <-  sample(2:5, 1)
  res <- cart_greedy(XY, depth = depth_s)
  expect_s3_class(res, "Baum")
  expect_s3_class(res, "R6")
})

test_that("cart_greedy: argument random", {
  d <- sample(1:5, 1)
  m <- sample(1:d, 1)
  XY <- generate_mult_data(n=50, d=d, sigma=diag(d), mu=rep(0., d))[[1]]
  depth_s <-  sample(2:5, 1)

  # m > d -> error
  expect_error(cart_greedy(XY, depth = depth_s, random = TRUE, m = d+1))

  res <- cart_greedy(XY, depth = depth_s, random = TRUE, m = m)
  expect_s3_class(res, "Baum")
  expect_s3_class(res, "R6")
})

# See [Richter, p.9]
test_that("cart_greedy: duplicate data", {
  M <- generate_sin_data(100, sigma=0.2)
  M <- rbind(M, M) # create second copy of data
  T1 <- cart_greedy(M, depth=10, threshold=1)
  expect_s3_class(T1, "Baum")
  expect_s3_class(T1, "R6")
  expect_null(T1$validate())
})

# See [Richter, p.177]
test_that("cart_greedy: fully generated tree", {
  n <- 150
  M <- generate_sin_data(n, sigma=0.2)
  dimnames(M) <- list(NULL, c(1, "y"))
  T2 <- cart_greedy(M, depth=20, threshold=1)
  leaves <- T2$obstkorb()
  expect_equal(length(T2$nodes[leaves]), n)
  expect_equal(sort(sapply(T2$nodes[leaves], function(n) `$`(n, "y"))),
               sort(M[, "y"]))
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

test_that("cart_predict", {
  n <- 100
  M <- generate_sin_data(n, sigma=0.2)
  dimnames(M) <- list(NULL, c(1, "y"))
  T2 <- cart_greedy(M, depth=20, threshold=1)
  res <- numeric(n)
  for (i in 1:n) {
    x <- M[, "1"][[i]]
    res[[i]] <- cart_predict(x, T2$root)
  }
  expect_equal(res, M[, "y"])
})

test_that("cart_greedy, malformed data", {
  XY <- data.frame(x1 = c(1.5, 1.5, 2), x2 = c(2.2, 2.2, 3), y = c(2, 3, 2))
  T2 <- cart_greedy(XY)
  expect_null(T2$validate())

  XY <- matrix(c(1.5, 1.5, 2, 2.2, 2.2, 3, 2, 3, 2), nrow=3)
  dimnames(XY) <- list(NULL, c(1, 2, "y"))
  T3 <- cart_greedy(XY)
  expect_null(T3$validate())
})
