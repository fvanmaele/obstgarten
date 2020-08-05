# test data for random forest
generate_sin_data2 <- function(n, sigma=0.2, reg=TRUE) {
  if (sigma <= 0.) {
    stop("Error: sigma has to be > 0")
  }

  eps <- rnorm(n, mean=0, sd=sigma)

  if (reg) {
    a <- sample(-0.01:0.01, 1)
    b <- sample(-0.01:0.01, 1)
    M <- matrix(runif(n, min=0, max=1))
    M <- cbind(M, M[,1]+a, M[,1]+b)
    for (j in 1:ncol(M)) {
      for (i in 1:nrow(M)) {
        if(M[i,j] > 1){
          M[i,j] <- 1
        }
        if(M[i,j] < 0){
          M[i,j] <- 0
        }
      }
    }
    y <- c()
    for (i in 1:n) {
      y <- c(y, sin(2 * pi * mean(M[[i,1]], M[[i,2]], M[[i,3]])) + eps[i])
    }
    ret <- cbind(M, y)
    colnames(ret) <- c("x1", "x2", "x3", "y")
    return(ret)
  }
  else {
    x1 <- runif(n, min=0, max=1)
    x2 <- runif(n, min=0, max=1)
    k <- x2 - 0.5 - 0.3 * sin(2 * pi * x1)
    y <- rep(1, times=n)
    y[(k - eps) > 0.] <- 2
    ret <- matrix(c(x1, x2, y), nrow=n, ncol=3)
    colnames(ret) <- c("x1", "x2", "y")
    return(ret)
  }
}

#' Create a regression tree random based on training data.
#'
#' @param XY matrix with columns \eqn{1..d} (representing the training data
#'   \eqn{X_i}) and column \eqn{y} (for the values \eqn{Y_i}). Unless sample
#'   is set to TRUE, it is required that there are no observations
#'   \eqn{(X_{i_1}, Y_{i_1})} and \eqn{(X_{i_2}, Y_{i_2})} with \eqn{X_{i_1}
#'   = X_{i_2}}, but \eqn{Y_{i_1} \neq Y_{i_2}}.
#' @param depth The amount of steps before halting the algorithm (defaults to
#'   10)
#' @param threshold (integer)
#' @param sample (logical)
#' @param mode
#'
#' @return
#' @export
cart_random_tree <- function(XY, depth = 10L, threshold = 1L, sample = FALSE) {
  stopifnot(depth > 0L)
  stopifnot(threshold > 0L)
  d <- ncol(XY) - 1L
  n <- nrow(XY)

  # random number of leaves
  stopifnot(n > 1)
  t <- sample((n/2):n, 1) # TODO: not sure if t is defined correctly
  print(t)

  # Check data for duplicates (cf. [Richter 1.2, p.9])
  # TODO: add test for this case
  XY <- unique(XY)
  if (sample == FALSE) {
    m <- min(apply(XY, MARGIN=2, function(c) length(unique(c))))
    stopifnot(m == nrow(XY))
  } else {
    stop("function not implemented")
  }

  # Initialize regression tree
  Cart <- Baum$new()
  Root <- Cart$root # $label 1L
  Root$points <- XY

  # step k = 1, ...
  leaves <- list(Root)

  for (i in 1:depth) {
    k <- length(leaves)
    for (node in leaves) {
      cat("8<--------\n")
      print(node$parent)
      print(node)
      cat("\n")

      if(nrow(node$points) > threshold) {
        # random dimensions to minimise on
        stopifnot(d > 1)
        m <- sample(1:(d-1), 1) # m random dimensions
        S <- c(sample(1:d, m, replace = FALSE), d+1)

        # optimal subdivision
        params <- R_min(node$points[,S], m, mode = "regression") # minimise with j out of S
        stopifnot(all(!is.na(params$j), !is.infinite(params$j)))
        stopifnot(all(!is.na(params$s), !is.infinite(params$s)))
        params$j <- S[params$j]

        # update attributes of parent
        node$j <- params$j
        node$s <- params$s # node$points (of data) set in previous iteration
        node$y <- NA

        # update attributes of left child
        rows_lt <- node$points[, node$j] < node$s
        childL <- Gabel$new()
        childL$points <- node$points[rows_lt, , drop=FALSE]
        childL$y <- sum(childL$points[, "y"]) / length(childL$points[, "y"])

        # update attributes of right child
        rows_gt <- !rows_lt
        childR <- Gabel$new()
        childR$points <- node$points[rows_gt, , drop=FALSE]
        childR$y <- sum(childR$points[, "y"]) / length(childR$points[, "y"])

        # append leaves to tree
        Cart$append(node, childL, childR)
        leaves <- append(leaves, c(childL, childR))
      } else {
        stopifnot(length(node$points) > 0)
        message("threshold not reached for node ", node$label)
      }
    }

    # The tree must not have more than t leaves
    if(length(leaves) >= t){
      message("reached t leaves ", t)
      break
    }

    # pop leaves from stack
    if (k >= 1) {
      leaves <- tail(leaves, -k)
    } else {
      message("no more leaves with reached threshold")
      break
    }
  }
  return(Cart)
}
