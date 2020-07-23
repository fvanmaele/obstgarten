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
#' @param A partition of training data (data.frame)
#' @param n size of training data (integer)
#' @return risk factor
R_hat <- function(s, j, A, n) {
  A1 <- dplyr::filter(A, get(j) <  s)
  A2 <- dplyr::filter(A, get(j) >= s)

  # return(mem_R_sum(A1$y, A2$y, n))
  return(R_sum(A1$y, A2$y, n))
}

#' Optimize split index and split node for leaf nodes v_1 and v_2, based on the
#' partition of training data in an inner node v. (regression tree)
#'
#' Ties between found minima are broken at random.
#' @param A partition of training data (X_i, Y_i) (data.frame)
#' @param n size of the set of training data (integer)
#' @param d dimension of the training data X_i1..X_id
R_hat_min <- function(A, n, d) { # todo: 1:d as parameter for random forests
  s_min <- list(rep(NULL, d))

  for (j in 1:d) {
    sj_min <- optim(par=A[j,], fn=R_hat, j, A, n) # optimize by s \in (X_1j, .., X_nj)

    if (length(sj_min$par) > 1) {
      s_min[[j]] <- sample(sj_min$par, 1L)
    } else {
      s_min[[j]] <- sj_min$par
    }
  }

  # XXX: reduce number of samples to 1? (optim() over 2 parameters -> sample on $par)
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
#' @param df A data frame containing the training data (X_i1 ... X_id Y_i) as columns.
#' @param steps The amount of iterations before halting the algorithm. If unspecified,
#' @param threshold
#' @param mode
#' @return
#' @export
cart_greedy <- function(df, depth = 10, threshold = 1, mode = "regression") {
  n <- nrow(df)
  d <- ncol(df) - 1

  # Initialize tree
  Cart <- Tree$new()
  Root <- Cart$root # $label 1L
  Root$points <- df

  # Compute optimal subdivision
  params <- R_hat_min(df, n, d)
  Root$j <- params$j
  Root$s <- params$s

  # step k = 1, ...
  to_process <- list(Root) # stack for nodes to process
  iter = 0

  while (length(leaves) > 0) {
    if (iter > depth)
      break

    for (node in leaves) {
      to_process <- tail(leaves, -1)

      if(length(node$points > threshold)) {
        params <- R_hat_min(node$points, n, d) # optimal subdivision
        node$j <- params$j
        node$s <- params$s # node$points (of data) set in previous iteration
        node$y <- NA

        childL <- Gabel$new()
        childL$points <- dplyr::filter(node$points, get(node$j) < node$s) # FIXME: memoise
        childL$y <- sum(childL$points$Y) / n # FIXME: memoise

        childR <- Gabel$new()
        childR$points <- dplyr::filter(node$points, get(node$j) >= node$s) # FIXME: memoise
        childR$y <- sum(childR$points$Y) / n # FIXME: memoise

        # Update tree and stack
        Cart$append(node$label, childL, childR)
        to_process <- append(to_process, c(childL, childR))
      } else {
        message("threshold not reached for node ", node$label)
      }
    }
    iter <- iter+1
  }
}
