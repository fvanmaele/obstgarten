# c_sum <- function(y, n) {
#   return(sum(y) / n)
# }

# mem_c_sum <- memoise::memoise(c_sum)

R_sum <- function(y1, y2, n) {
  c1_hat <- sum(y1) / n # mem_c_sum(y1, n)
  c2_hat <- sum(y2) / n # mem_c_sum(y2, n)

  return(sum((y1 - c1_hat)^2) + sum((y2 - c2_hat)^2))
}

# mem_R_sum <- memoise::memoise(R_sum)

# R_part <- function(s, j, A) {
#
# }

# mem_R_part <- memoise::memoise(R_sum)

#' Risk factor R_n depending on split point and split index. (regression tree)
#'
#' @param s split point (numeric)
#' @param j split index (integer)
#' @param A partition of training data (matrix)
#' @param n size of training data (integer)
#' @return risk factor
R_hat <- function(s, j, A, n) {
  rows_lt <- which(A[, j] < s)
  rows_gt <- setdiff(1:nrow(A), rows_lt)

  A1 <- A[rows_lt, , drop=FALSE]
  A2 <- A[rows_gt, , drop=FALSE]

  return(R_sum(A1[, "y"], A2[, "y"], n)) # FIXME: memoise
}

#' Optimize split index and split node for leaf nodes v_1 and v_2, based on the
#' partition of training data in an inner node v. (regression tree)
#'
#' Ties between found minima are broken at random.
#' @param A partition of training data (X_i, Y_i) (matrix)
#' @param n size of the set of training data (integer)
#' @param d dimension of the training data X_i1..X_id
R_hat_min <- function(A, n, d) {
  s_min <- list(rep(NULL, d))

  # TODO: sample of d (or argument) for random forests
  for (j in 1:d) {
    s <- A[, j] # argmin: s \in (X_1j, .., X_nj) \sub A
    sj_min <- optim(par = s, fn = R_hat, j=j, A=A, n=n)

    if (length(sj_min$par) > 1) {
      s_min[[j]] <- sample(sj_min$par, 1L)
    } else {
      s_min[[j]] <- sj_min$par
    }
  }

  # XXX: reduce number of samples to 1?
  # (optim() over 2 parameters -> sample on $par)
  j_min <- seq_along(s_min)[s_min == min(s_min)]
  if (length(j_min) > 1L) {
    j_hat <- sample(j_min, 1L)
  } else {
    j_hat <- j_min
  }

  s_hat <- s_min[[j_hat]]
  return(c(j = j_hat, s = s_hat))
}

#' Create a regression tree greedily based on training data.
#' @param XY matrix with columns $1..$d (representing the
#' training data X_i) and column $y (for the corresponding values Y_i)
#' @param steps The amount of iterations before halting the algorithm
#' (defaults to 10)
#' @param threshold
#' @param mode
#' @return
#' @export
cart_greedy <- function(XY, depth = 10, threshold = 1, mode = "regression") {
  n <- nrow(XY)
  d <- ncol(XY)-1

  # Initialize tree
  Cart <- Baum$new()
  Root <- Cart$root # $label 1L
  Root$points <- XY

  # step k = 1, ...
  to_process <- list(Root)
  iter = 0

  while (length(to_process) > 0) {
    if (iter > depth)
      break

    for (node in to_process) {
      to_process <- tail(to_process, -1)

      if(length(node$points > threshold)) {
        params <- R_hat_min(node$points, n, d) # optimal subdivision
        node$j <- params$j
        node$s <- params$s # node$points (of data) set in previous iteration
        node$y <- NA

        # update attributes of left child
        rows_lt <- which(node$points[, j] < s) # FIXME: memoise
        childL <- Gabel$new()
        childL$points <- node$points[rows_lt, , drop=FALSE]
        childL$y <- sum(childL$points[, "y"]) / n # FIXME: memoise

        # update attributes of right child
        rows_gt <- setdiff(1:nrow(node$points), rows_lt) # FIXME: memoise
        childR <- Gabel$new()
        childR$points <- node$points[rows_gt, , drop=FALSE]
        childR$y <- sum(childR$points[, "y"]) / n # FIXME: memoise

        # update tree and stack
        Cart$append(node$label, childL, childR)
        to_process <- append(to_process, c(childL, childR))
      } else {
        message("threshold not reached for node ", node$label)
      }
    }
    iter <- iter+1
  }
}
