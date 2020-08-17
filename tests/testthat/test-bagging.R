test_that("bagging: argument assertions", {
  x <- generate_sin_data(10)
  expect_error(bagging(x_train=x, x_test=x, B=12))
  expect_error(bagging(x, B="f"))
  expect_error(bagging(x_train=x, x_test=x, B=10L, regression=2))
  expect_error(bagging(x_train=x, x_test=x, B=10L, regression="hey"))
})

test_that("bagging: argument x_train and x_test", {
  expect_error(bagging(x_train=2, x_test=2, B=10L))
  expect_error(bagging(x_train="A", x_test="A", B=10L))
  expect_error(bagging(x_train=generate_sin_data(10), x_test=10), B=10L)
  expect_error(bagging(x_train=matrix(c(1,1,2,2,3,3), 2, 3)),
               x_test=matrix(c(1,1,2,2,3,3), 2, 3), B=10L)

  x <- generate_sin_data(10)
  B <- sample(1:10, 1)
  res <- bagging(B=B, x_train=x, x_test=x)
  expect_type(res, "double")
})

test_that("bagging: argument random forest", {
  d <- sample(1:5, 1)
  m <- sample(1:d, 1)
  x <- generate_mult_data(n=50, d=d, sigma=diag(d), mu=rep(0., d))[[1]]

  # m > d -> error
  expect_error(bagging(B=10L, x_train = XY1, x_test = XY2, random = TRUE, m = d+1))

  # set m
  res <- bagging(B=10L, x_train=x, x_test=x, depth=3, m=m, regression=TRUE, use_parallel=FALSE, random_forest = TRUE)
  expect_type(res, "double")

  # m <- NULL
  res <- bagging(B=10L, x_train=x, x_test=x, depth=3, regression=TRUE, use_parallel=FALSE, random_forest = TRUE)
  expect_type(res, "double")
})
