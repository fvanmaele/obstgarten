#' Title
#'
#' @param x
#'
#' @return
#' @export
which_is_min <- function(x) {
  y <- seq_along(x)[x == min(x, na.rm = TRUE)]
  if (length(y) > 1L) {
    sample(y, 1L)
  } else {
    y
  }
}

#' Title
#'
#' @param y1
#' @param y2
#' @param n
#'
#' @return
R_sum <- function(y1, y2, n) {
  c1_hat <- sum(y1) / n
  c2_hat <- sum(y2) / n

  return(sum((y1 - c1_hat)^2) + sum((y2 - c2_hat)^2))
}

#' Risk factor R_n depending on split point and split index. (regression tree)
#'
#' @param s split point (numeric)
#' @param j split index (integer)
#' @param A partition of training data (matrix)
#' @param n size of training data (integer)
#'
#' @return risk factor
R_hat <- function(s, j, A, n) {
  rows_lt <- which(A[, j] < s)
  rows_gt <- setdiff(1:nrow(A), rows_lt)
  A1 <- A[rows_lt, , drop=FALSE] # x_j <  s
  A2 <- A[rows_gt, , drop=FALSE] # x_j >= s

  # XXX: ignore partitions which can contain no data points
  if (length(A1) > 0) {
    return(R_sum(A1[, "y"], A2[, "y"], n))
  }
  return(NA)
}

#' Optimize split index and split node for leaf nodes v_1 and v_2, based on the
#' partition of training data in an inner node v. (regression tree)
#'
#' Ties between found minima are broken at random.
#' @param A partition of training data (X_i, Y_i) (matrix)
#' @param n size of the set of training data (integer)
#' @param d dimension of the training data X_i1..X_id
#'
#' @return
R_hat_min <- function(A, n, d) {
  # argmin: s_j \in (X_1j, .., X_nj) \sub A, j \in (1, .., d)
  G <- array(dim=c(nrow(A), 2, d), dimnames=list(
    NULL, c("s", "R"), sapply(1:d, function(i) paste0("j = ", i))))
  min_s <- matrix(nrow=d, ncol=2, dimnames=list(NULL, c("s", "R")))

  for (j in 1:d) {
    for (i in seq_along(A[, j])) {
      s <- A[i, j]
      G[i, "s", j] <- s
      G[i, "R", j] <- R_hat(s, j, A, n)
    }
    min_i <- which_is_min(G[, "R", j])
    min_s[j, ] <- G[min_i, , j]
  }

  j_hat <- which_is_min(min_s[, "R"])
  s_hat <- min_s[j_hat, "s"]
  return(list(j = j_hat, s = s_hat))
}

#' Create a regression tree greedily based on training data.
#'
#' @param XY matrix with columns $1..$d (representing the
#' training data X_i) and column $y (for the corresponding values Y_i)
#' @param depth The amount of steps before halting the algorithm
#' (defaults to 10)
#' @param threshold
#' @param mode
#'
#' @return
#' @export
cart_greedy <- function(XY, depth = 10, threshold = 1, mode = "regression") {
  stopifnot(depth > 0)
  stopifnot(threshold > 0)
  n <- nrow(XY)
  d <- ncol(XY)-1

  # Initialize tree
  Cart <- Baum$new()
  Root <- Cart$root # $label 1L
  Root$points <- XY

  # step k = 1, ...
  leaves <- list(Root)

  for (i in 1:depth) {
    k <- length(leaves)

    for (node in leaves) {
      if(length(node$points > threshold)) {
        params <- R_hat_min(node$points, n, d) # optimal subdivision
        node$j <- params$j
        node$s <- params$s # node$points (of data) set in previous iteration
        node$y <- NA

        # sanity check
        str(params)
        print(node)
        stopifnot(all(!is.na(node$j), !is.infinite(node$j)))
        stopifnot(all(!is.na(node$s), !is.infinite(node$s)))

        # update attributes of left child
        rows_lt <- which(node$points[, node$j] < node$s) # FIXME: memoise?
        childL <- Gabel$new()
        childL$points <- node$points[rows_lt, , drop=FALSE]
        childL$y <- sum(childL$points[, "y"]) / n # FIXME: memoise?

        # update attributes of right child
        rows_gt <- setdiff(1:nrow(node$points), rows_lt) # FIXME: memoise?
        childR <- Gabel$new()
        childR$points <- node$points[rows_gt, , drop=FALSE]
        childR$y <- sum(childR$points[, "y"]) / n # FIXME: memoise?

        # update tree and stack
        Cart$append(node$label, childL, childR)
        leaves <- append(leaves, c(childL, childR))
      } else {
        stopifnot(length(node$points) > 0)
        message("threshold not reached for node ", node$label)
      }
    }
    if (k >= 1) {
      leaves <- tail(leaves, -k)
    } else {
      message("no more leaves with reached threshold")
      break
    }
  }
  return(Cart)
}
