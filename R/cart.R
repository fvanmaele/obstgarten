

#' Title
#'
#' @param x
#' @param node
#'
#' @return
#' @export
#'
#' @examples
cart_predict = function(x, node) { # list or vector
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

