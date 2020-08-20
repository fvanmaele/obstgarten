#' Cart partition
#' @description partition data at a given split point and index
#' @param s split point at which the data is separated (`numeric`)
#' @param j dimension at which the data is separated (`numeric`)
#' @param A data (`data.frame` or `matrix`)
#'
#' @return `list` with the following elements:
#' - `$A1`: The rows `A[, j] < s`
#' - `$A2`: The rows `A[, j] >= s` (taken by complment)
#' @export
#'
#' @examples cart_part(0.5,1,generate_sin_data(10))
cart_part <- function(s, j, A) {
  rows_lt <- A[, j] < s
  rows_gt <- !rows_lt

  return(list(A1 = A[rows_lt, , drop=FALSE],
              A2 = A[rows_gt, , drop=FALSE]))
}

#' cart_grid
#' @description computes risk R for data points
#' @details
#' @param A data (`data.frame` or `matrix`)
#' @param d dimensions (numeric)
#' @param f Minimizer `R_hat` or `C_hat` for regression and classification,
#'   respectively (`closure`)
#' @param quantile use `quantile()` instead of data points to compute the risk.
#'   (`logical`, defaults to `FALSE`)
#' @param q_threshold minimal amount of data points for using quantiles.
#'   (`integer`, defaults to `100L`)
#' @param q_pct amount of probabilities for `quantile()`, in pct. of the data
#'   set size, generated with `seq()`. (`numeric`, defaults to `0.25`)
#'
#' @return A 3D `array` with dimensions (`j`, `s`, `R`).
#' @export
#'
#' @examples cart_grid(generate_sin_data(100), 1, R_hat)
#'
cart_grid <- function(A, d, f, quantile = FALSE, q_threshold = 100L, q_pct = 0.25) {
  stopifnot(length(formals(f)) == 2)
  stopifnot(q_pct > 0 && q_pct < 1)
  stopifnot(is.integer(q_threshold))
  stopifnot(is.logical(quantile))

  # disable quantiles if threshold of data points is not reached
  if (nrow(A) <= q_threshold) {
    quantile <- FALSE
  }
  G_dn <- list(NULL, c("s", "R"), sapply(1:d, function(i) paste0("j = ", i)))
  if (quantile) {
    q_len <- ceiling(q_pct * nrow(A))
    G <- array(dim=c(q_len, 2, d), dimnames=G_dn)
  } else {
    G <- array(dim=c(nrow(A), 2, d), dimnames=G_dn)
  }

  # argmin: s_j \in (X_1j, .., X_nj) \sub A, j \in (1, .., d)
  for (j in 1:d) {
    if (quantile) {
      probs = seq(0, 1, length.out = q_len)
      stopifnot(length(probs) > 0)

      Q <- quantile(A[, j], probs = probs, names = FALSE)
      stopifnot(length(Q) == q_len)
    } else {
      Q <- A[, j]
    }

    for (i in seq_along(Q)) { # seq_along(A[, j])
      s <- Q[[i]] # s <- A[i, j]
      P <- cart_part(s, j, A) # new partition A1, A2

      if (nrow(P$A1) > 0) {
        R <- f(P$A1[, "y"], P$A2[, "y"])
      } else {
        #message("no data points for partition j = ", j, ", s = ", s)
        R <- NA_real_ # no data points in new partition, skip
      }
      G[i, "s", j] <- s
      G[i, "R", j] <- R
    }
  }
  return(G)
}

#' Minimizer function for regression
#' @param y1 (`numeric`)
#' @param y2 (`numeric`)
#' @export
R_hat <- function(y1, y2) {
  c1_hat <- sum(y1) / length(y1)
  c2_hat <- sum(y2) / length(y2)

  return(sum((y1 - c1_hat)^2) + sum((y2 - c2_hat)^2))
}

#' Minimizer function for classification
#' @param y1 (`numeric`)
#' @param y2 (`numeric`)
#' @export
C_hat <- function(y1, y2) {
  c1_p <- max(sapply(unique(y1), function(k) length(y1[y1 == k]) / length(y1)))
  c2_p <- max(sapply(unique(y2), function(k) length(y2[y2 == k]) / length(y2)))

  return(length(y1) * (1 - c1_p) + length(y2) * (1 - c2_p))
}

#' Find optimal subdivision for CART
#' @description
#' Find optimal split index \eqn{j} and split point \eqn{s} for a given
#' partition A (regression or classification tree)
#'
#' @param A subset of training data \eqn{(X_i, Y_i)} (`matrix`)
#' @param d dimension of the training data \eqn{X_{i_1}..X_{i_d}} (`integer`)
#' @param mode `regression` or `classification`
#' @param ... remaining arguments passed on to the next function
#'
#' @return list containing optimal parameters \eqn{j}, \eqn{s}
#' @export
R_min <- function(A, d, mode = "regression", ...) {
  if (mode == "regression") {
    G <- cart_grid(A, d, R_hat, ...)
  } else if (mode == "classification") {
    G <- cart_grid(A, d, C_hat, ...)
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

#' Greedy algorithm for CART
#' @description Create a regression or classification tree tree greedily based
#' on training data.
#' @details
#' Unless `sample` is set to `TRUE`, it is required that there are no observations
#' \eqn{(X_{i_1}, Y_{i_1})} and \eqn{(X_{i_2}, Y_{i_2})} with \eqn{X_{i_1} =
#' X_{i_2}}, but \eqn{Y_{i_1} \neq Y_{i_2}}.
#' @param XY matrix with columns \eqn{1..d} (representing the training data
#'   \eqn{X_i}) and column \eqn{y} (for the values \eqn{Y_i}).
#' @param depth The amount of steps before halting the algorithm (defaults to
#'   10)
#' @param threshold The minimum amount of data points for dividing a leaf (`integer`)
#' @param sample Use `sample()` to (`logical`)
#' @param mode `regression` or `classification` specifies whether to train
#'   a regression or classification tree, respectively. Default is "regression"
#' @param random generates CART for random forest (`logical`, default `FALSE`)
#' @param m The
#' default: 0 so it only has to be set at random=TRUE (`numeric`)
#' @param quantile whether to use quantiles for computing the optimal
#'   subdivision (`logical`, defaults to `FALSE`)
#' @param q_threshold minimal of data points for using quantiles (`integer`,
#'   defaults to `100L`)
#' @param q_pct amount of probabilities for `quantile()`, in pct. of the data
#'   set size. (`numeric`, defaults to `0.25`)
#' @return A regression or classification tree modeled after the training data
#'   (`Baum`)
#'
#' n <- 150
#' M <- generate_sin_data(n, sigma=0.2)
#' dimnames(M) <- list(NULL, c(1, "y"))
#' T2 <- cart_greedy(M, depth=20, threshold=1)
#' T2$validate()
#' @export
cart_greedy <- function(XY, depth = 10L, mode="regression", threshold = 1L,
                        sample = FALSE, random = FALSE, m = 0L,
                        quantile = FALSE, q_threshold = 100L, q_pct = 0.25) {
  stopifnot("XY is not an data.frame with more than one col and row"= (is.data.frame(XY) | is.matrix(XY)) & ncol(XY) > 1 & nrow(XY) > 1)
  stopifnot("depth is not numeric and greater than 0"= is.numeric(depth) & depth > 0L)
  stopifnot("threshold is not numeric and greater than 0"= is.numeric(threshold) & threshold > 0L)
  stopifnot("sample is not logical"= is.logical(sample))
  stopifnot("random is not logical"= is.logical(random))
  d <- ncol(XY) - 1L

  if(random){
    n <- nrow(XY)
    # random number of leaves
    t <- sample((n/2):n, 1) # TODO: not sure if t is defined correctly
    # print(str_c("t ", t))
  }

  # Check data for duplicates (cf. [Richter 1.2, p.9])
  # TODO: add test for this case
  if (sample == TRUE) {
    stop("function not implemented")
  } else {

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
      stopifnot("parameter m must be smaller or equal to dimension d"= m <= d)
      if(random & d > 1) {
        # random dimensions to minimize on
        S <- c(sample(1:d, m, replace = FALSE), d+1) # [X_i1 .. X_im] .. Y_i
      } else {
        S <- 1:(d+1)
        m <- d
      }
      # optimal subdivision
      if(nrow(unique(node$points[, S, drop=FALSE])) > threshold) {
        # TODO: pass correct range to R_min (instead of d -> 1:d)
        # minimize with j out of S
        params <- R_min(node$points[, S, drop=FALSE], m, mode = mode,
                        quantile = quantile, q_threshold = q_threshold, q_pct = q_pct)
        stopifnot(all(!is.na(params$j), !is.infinite(params$j)))
        stopifnot(all(!is.na(params$s), !is.infinite(params$s)))
        #params$j <- S[params$j]

        # update attributes of parent
        # TODO: pass correct range to R_min (instead of S[params$j])
        node$j <- S[params$j]
        node$s <- params$s # node$points (of data) set in previous iteration
        #node$y <- NA

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
    if(random && length(leaves) >= t){
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

#' CART decision rule
#' @description evaluate the CART decision rule
#' @details The decision rule is given by
#'   \deqn{f(x)\sum{m=1}{#T}y(w_m)*I_{A_m}(x)} where \eqn{A_m} is the CART
#'   induced partition and \eqn{y(w_m)} the weights for the leaves \eqn{w_1,
#'   ..., w_{#T}}. The evaluation of \eqn{f} is done recursively by checking the
#'   split points and split indices of the tree nodes.
#' @param x data point to evaluate (`vector`)
#' @param node entry point (`Gabel`), typically the CART root (`Baum$root`)
#' @return The predicted `y` value (`numeric` for regression, `integer` for
#'   classification)
#' @export
#'
cart_predict <- function(x, node) { # list or vector
  # check if vector x matches tree dimension
  stopifnot(length(x) == (ncol(node$points)-1))

  if (is.null(node$childR) && is.null(node$childL)) {
    # leaf node found -> return value
    return(node$y)
  } else if (x[[node$j]] < node$s) {
    return(cart_predict(x, node$childL))
  } else {
    return(cart_predict(x, node$childR))
  }
}

