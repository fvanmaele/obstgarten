#' Title
#'
#' @param x
#' @param node
#' @param mask
#'
#' @return
#' @export
#'
#' @examples
cart_predict_prune <- function(x, node, mask) { # list or vector
  stopifnot(length(x) == (ncol(node$points)-1))
  stopifnot(is.vector(mask, mode = "logical"))

  if ((is.null(node$childR) && is.null(node$childL)) ||
      !mask[[node$childR$label]] && !mask[[node$childL$label]]) {
    # leaf node found -> return value
    return(node$y)
  } else if (x[[node$j]] < node$s) {
    return(cart_predict(x, node$childL, mask))
  } else {
    return(cart_predict(x, node$childR, mask))
  }
}

#' Title
#'
#' @param node
#' @param mask
#'
#' @return
#' @export
#'
#' @examples
cart_prune <- function(node, mask) { # list or vector
  if ((is.null(node$childR) && is.null(node$childL)) ||
      !mask[[node$childR$label]] && !mask[[node$childL$label]]) {
    return(mask)
  } else {
    mask[[node$childL$label]] <- FALSE
    mask <- cart_prune(node$childL, mask)

    mask[[node$childR$label]] <- FALSE
    mask <- cart_prune(node$childR, mask)
    return(mask)
  }
}


cart_greedy_prune <- function(XY, depth = 10L, mode="regression", threshold = 1L,
                              sample = FALSE, random = FALSE, m = 0L,
                              quantile = FALSE, q_threshold = 100L, q_pct = 0.25, lambda = "CV") {

  Cart <- cart_greedy(XY, depth = depth, mode = mode, threshold = threshold,
                      sample = sample, random = random, m = m,
                      quantile = quantile, q_threshold = q_threshold, q_pct = q_pct)

  return(Cart)
}
