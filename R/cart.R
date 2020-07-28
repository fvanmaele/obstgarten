#' Title
#'
#' @param node
#' @param d
#'
#' @return
#' @export
#'
#' @examples
cart_partition = function(node, d) {
  stopifnot(d == (ncol(node$points) - 1L))
  y_val <- numeric(0)

  recurse <- function(node, d) {
    if (!is.null(node$childL) && !is.null(node$childR)) {
      row_s <- rep(NA, d)
      row_s[[node$j]] <- node$s

      return(rbind(recurse(node$childL, d), row_s,
                   recurse(node$childR, d)))
    }
    else if (is.null(node$childL) && is.null(node$childR)) {
      y_val <<- c(y_val, node$y) # DFS for y values
      return(NULL)
    }
    else {
      stop("none or both of node$childL and node$childR must be set")
    }
  }
  part <- recurse(node, d)
  return(list(part = part, y = y_val))
}

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

#' Title
#'
#' @return
#' @export
#'
#' @examples
cart_plot = function(node) {
  XY <- node$points
  if (is.null(XY)) {
    stop("no data available in node")
  }
  if (ncol(XY) > 3) {
    stop("data must be 1 or 2-dimensional")
  }
  else if (ncol(XY) == 3) {
    stop("function not implemented")
  }
  else {
    # TODO: cache partition (e.g for subsequent plots)
    stop() # TODO
    # Combined plot
    plot(XY)
    plot(x, y, type="l")
  }
}
