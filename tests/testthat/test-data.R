test_that("generate_sin_data", {
  n <- sample(10:20,1)
  sigma <- sample(1:3,1)/10

  #reg=T
  data <- generate_sin_data(n, sigma = sigma, reg = T, grid = runif(n, min=0, max=1))
  expect_length(data, n*2)
  for (i in data[1:n]) {
    expect_lte(i, 1)
    expect_gte(i, 0)
  }
  expect_equal(dimnames(data)[[2]], c("x", "y"))

  #reg=F
  data <- generate_sin_data(n, sigma = sigma, reg = F)
  expect_length(data, n*3)
  for (i in data[1:(n*2)]) {
    expect_lte(i, 1)
    expect_gte(i, 0)
  }
  expect_equal(dimnames(data)[[2]], c("x1", "x2", "y"))
})

test_that("generate_sin_2D", {
  n <- sample(10:20,1)
  sigma <- sample(1:3,1)/10
  k <- sample(5:15,1)
  data <- generate_sin_2D(n, sigma = sigma, k=k)

  expect_length(data[[1]], n)
  for (i in 1:n) {
    expect_lte(data[[i,1]], k)
    expect_gte(data[[i,1]], -k)
    expect_lte(data[[i,2]], k)
    expect_gte(data[[i,2]], -k)
  }
  expect_equal(dimnames(data)[[2]], c("x1", "x2", "y"))
})

test_that("generate_mult_data", {
  n <- sample(10:20,1)
  sigma <- sample(1:3,1)/10
  d <- sample(1:5,1)
  data <- generate_mult_data(n, d, sd=sigma)

  expect_length(data[[1]], d+1)
  expect_length(data[[1]][[1]], n)
  expect_equal(dimnames(data[[1]])[[2]][[d+1]], c("y"))
  expect_length(data[[2]], d)
  expect_length(data[[3]], d*d)
})

test_that("prepare_iris", {
  tsr <- sample(1:9,1)/10
  data <- prepare_iris(training_set_ratio=tsr)
  expect_length(data[[1]][[1]], tsr*150)
  expect_length(data[[2]][[1]], (1-tsr)*150)
})


