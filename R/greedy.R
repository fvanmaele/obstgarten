#' Title
#'
#' @param s
#' @param j
#' @param A
#'
#' @return
#' @export
#'
#' @examples
cart_part <- function(s, j, A) {
  rows_lt <- A[, j] < s
  rows_gt <- !rows_lt

  return(list(A1 = A[rows_lt, , drop=FALSE],
              A2 = A[rows_gt, , drop=FALSE]))
}

#' Title
#'
#' @param A
#' @param d
#' @param f
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
cart_grid <- function(A, d, f, ...) {
  stopifnot(length(formals(f)) == 2)
  G_dn <- list(NULL, c("s", "R"), sapply(1:d, function(i) paste0("j = ", i)))
  G <- array(dim=c(nrow(A), 2, d), dimnames=G_dn)

  # argmin: s_j \in (X_1j, .., X_nj) \sub A, j \in (1, .., d)
  for (j in 1:d) {
    for (i in seq_along(A[, j])) {
      s <- A[i, j]
      P <- cart_part(s, j, A) # new partition A1, A2

      if (nrow(P$A1) > 0) {
        R <- f(P$A1[, "y"], P$A2[, "y"], ...)
      } else {
        message("no data points for partition j = ", j, ", s = ", s)
        R <- NA_real_ # no data points in new partition, skip
      }
      G[i, "s", j] <- s
      G[i, "R", j] <- R
    }
  }
  return(G)
}

R_hat <- function(y1, y2) {
  c1_hat <- sum(y1) / length(y1)
  c2_hat <- sum(y2) / length(y2)

  return(sum((y1 - c1_hat)^2) + sum((y2 - c2_hat)^2))
}

C_hat <- function(y1, y2) {
  c1_p <- max(sapply(unique(y1), function(k) length(y1[y1 == k]) / length(y1)))
  c2_p <- max(sapply(unique(y2), function(k) length(y2[y2 == k]) / length(y2)))

  return(length(y1) * (1 - c1_p) + length(y2) * (1 - c2_p))
}

#' Find optimal split index \eqn{j} and split point \eqn{s} for a given
#' partition A (regression or classification tree)
#'
#' @param A subset of training data \eqn{(X_i, Y_i)} (matrix)
#' @param d dimension of the training data \eqn{X_{i_1}..X_{i_d}}
#' @param mode
#'
#' @return list containing optimal parameters \eqn{j}, \eqn{s}
#' @export
R_min <- function(A, d, mode = "regression") {
  if (mode == "regression") {
    G <- cart_grid(A, d, R_hat)
  } else if (mode == "classification") {
    G <- cart_grid(A, d, C_hat)
  } else {
    stop("invalid mode: must be regression or classification")
  }
  min_s <- apply(G, MARGIN=3, function(M) {
    idx <- which.min(M[, "R"]) # TODO: break ties at random
    M[idx, "s"]
  })

  j_hat <- which.min(min_s) # TODO: break ties at random
  s_hat <- min_s[[j_hat]]
  return(list(j = j_hat, s = s_hat))
}

#' Create a regression tree greedily based on training data.
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
cart_greedy <- function(XY, depth = 10L, threshold = 1L, sample = FALSE) {
  stopifnot(depth > 0L)
  stopifnot(threshold > 0L)
  d <- ncol(XY) - 1L

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
      if(nrow(node$points) > threshold) {
        # optimal subdivision
        params <- R_min(node$points, d, mode = "regression")
        stopifnot(all(!is.na(params$j), !is.infinite(params$j)))
        stopifnot(all(!is.na(params$s), !is.infinite(params$s)))

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
