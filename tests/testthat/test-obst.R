test_that("as.list.Gabel", {
  G1 <- Gabel$new()
  G1$label <- 1L
  G2 <- Gabel$new()
  G2$label <- 2L
  G3 <- Gabel$new()
  G3$label <- 3L
  G4 <- Gabel$new()
  G4$label <- 4L
  G5 <- Gabel$new()
  G5$label <- 5L
  G1$append(G2, G3)
  G2$append(G4, G5)

  G1$s <- rnorm(1)
  G1$j <- sample(1:42, 1)
  G2$s <- rnorm(1)
  G2$j <- sample(1:42, 1)
  G3$y <- rnorm(1)
  G4$y <- rnorm(1)
  G5$y <- rnorm(1)

  lst <- as.list(G1)
  expect_type(lst, "list")
  expect_equal(attr(lst, "label"), 1L)
  expect_equal(attr(lst, "s"), G1$s)
  expect_equal(attr(lst, "j"), G1$j)
  expect_equal(attr(lst[[1]], "label"), 2L)
  expect_equal(attr(lst[[1]], "s"), G2$s)
  expect_equal(attr(lst[[1]], "j"), G2$j)
  expect_equal(attr(lst[[2]], "label"), 3L)
  expect_equal(attr(lst[[2]], "y"), G3$y)
  expect_equal(attr(lst[[1]][[1]], "label"), 4L)
  expect_equal(attr(lst[[1]][[2]], "label"), 5L)
  expect_equal(attr(lst[[1]][[1]], "y"), G4$y)
  expect_equal(attr(lst[[1]][[2]], "y"), G5$y)
})

test_that("Baum$append", {
  T1 <- Baum$new()
  expect_s3_class(T1, "Baum")
  expect_s3_class(T1, "R6")
  G1 <- T1$root
  expect_s3_class(G1, "Gabel")
  expect_s3_class(G1, "R6")

  G1$points <- matrix(1:6, ncol=2, dimnames=list(NULL, c(1, "y")))
  G2 <- Gabel$new()
  G2$points <- matrix(c(1:2, 4:5), ncol=2, dimnames=list(NULL, c(1, "y")))
  G3 <- Gabel$new()
  G3$points <- matrix(c(3, 6), ncol=2, dimnames=list(NULL, c(1, "y")))
  T1$append(G1, G2, G3) # append to G1 ($label == 1L)

  expect_identical(G1$childL, G2)
  expect_equal(G2$label, 2L)
  expect_identical(G1$childR, G3)
  expect_equal(G3$label, 3L)

  # ensure labels are not updated from argument label
  T1$append(G2, Gabel$new(), Gabel$new())
  expect_equal(G2$childL$label, 4L) # 4L
  expect_equal(G2$childR$label, 5L) # 5L
})

test_that("Baum$validate", {
  T2 <- Baum$new()
  T2$root$points <- matrix(1:6, ncol=2, dimnames=list(NULL, c(1, "y")))
  expect_error(T2$validate())

  T2$root$y <- rnorm(1)
  T2$root$s <- rnorm(1)
  T2$root$j <- sample(1:42, 1)
  expect_error(T2$validate())

  T2$append(T2$root, Gabel$new(), Gabel$new())
  T2$root$y <- NA
  expect_error(T2$validate())

  T2$root$childL$y <- rnorm(1)
  T2$root$childR$y <- rnorm(1)
  expect_error(T2$validate())

  T2$root$childL$points <- matrix(
    c(1:2, 4:5), ncol=2, dimnames=list(NULL, c(1, "y")))
  T2$root$childR$points <- matrix(
    c(3, 6), ncol=2, dimnames=list(NULL, c(1, "y")))

  T2$root$label <- 0L
  T2$root$childL$label <- 0L
  T2$root$childR$label <- 0L
  expect_error(T2$validate())
})
