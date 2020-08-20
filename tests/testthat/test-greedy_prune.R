test_that("cart_greedy_prune: argument lambda", {
  XY <- generate_sin_data(10)
  expect_error(cart_greedy_prune(XY, depth = 3, lambda = -1))
})


test_that("cart_greedy_prune: argument XY", {
  d <- sample(2:5, 1)
  XY <- generate_mult_data(n=50, d=d, sigma=diag(d), mu=rep(0., d))[[1]]
  depth_s <-  sample(2:5, 1)
  res <- cart_greedy_prune(XY, depth = depth_s, lambda = 0.01)
  expect_type(res, "list")
  expect_s3_class(res[[1]], "Baum")
  expect_s3_class(res[[1]], "R6")
  expect_type(res[[2]], "logical")
  expect_equal(length(res[[2]]), length(res[[1]]$nodes))
})


test_that("cart_greedy_prune: argument random", {
  d <- sample(1:5, 1)
  m <- sample(1:d, 1)
  XY <- generate_mult_data(n=50, d=d, sigma=diag(d), mu=rep(0., d))[[1]]
  depth_s <-  sample(2:5, 1)

  res <- cart_greedy_prune(XY, depth = depth_s, random = TRUE, m = m)
  expect_type(res, "list")
  expect_s3_class(res[[1]], "Baum")
  expect_s3_class(res[[1]], "R6")
  expect_type(res[[2]], "logical")
  expect_equal(length(res[[2]]), length(res[[1]]$nodes))
})


test_that("cart_prune", {
  XY <- generate_sin_data(10)
  Tp <- cart_greedy_prune(XY, depth = 5, lambda = 0.01)
  res <- cart_prune(Tp[[1]]$root, Tp[[2]])
  expect_type(res, "logical")
  expect_equal(length(Tp[[2]]), length(res))
  expect_equal(1, sum(res, na.rm = TRUE))
})


test_that("cart_predict_pruned", {
  n <- 10
  M <- generate_sin_data(n, sigma=0.2)
  dimnames(M) <- list(NULL, c(1, "y"))
  T2 <- cart_greedy_prune(M, depth=20, threshold=1, lambda = 0)
  res <- numeric(n)
  for (i in 1:n) {
    x <- M[, "1"][[i]]
    res[[i]] <- cart_predict_pruned(x, T2[[1]]$root, T2[[2]])
  }
  expect_equal(res, M[, "y"])
})
