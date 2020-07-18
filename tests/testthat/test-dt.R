context("interface for CART algorithms (data.tree)")
requireNamespace("data.tree", quietly = TRUE)

test_that("data.tree is CART", {
  acme <- data.tree::Node$new("Acme Inc.")
  acme$s <- 0.5
  acme$j <- 1L
  acme$y <- 2L
  expect_true(isCart(acme, mode = "classification"))

  child1 <- acme$AddChild("Accounting")
  child1$s <- 0.5
  child1$j <- 2L
  child1$y <- 0.1

  child2 <- acme$AddChild("New Software")
  child2$s <- 1
  child2$j <- 1L
  child2$y <- 1.5
  expect_true(isCart(acme, mode = "regression"))
})

# TODO: test fuer ungueltige CARTs
