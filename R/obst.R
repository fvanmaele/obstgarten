#' R6 class representing a CART node
Gabel <- R6::R6Class("Gabel",
  public = list(
    #' @field childL (Gabel)
    childL = NULL,
    #' @field childR (Gabel)
    childR = NULL,
    #' @field parent (Gabel)
    parent = NULL,
    #' @field label unique label for node (integer, defaults to NA_integer_)
    label = NA_integer_,
    #' @field depth (integer)
    depth = 0L,
    #' @field points (matrix)
    points = NULL, # training data inside the set A(v)
    #' @field s (numeric)
    s = NA_real_,
    #' @field j (integer)
    j = NA_integer_,
    #' @field y
    y = NA, # < NA_integer_, NA_real_

    #' @description
    #' @return logical TRUE if a node is a leaf (both childL and childR are
    #'   NULL), FALSE otherwise.
    isObst = function() {
      all(is.null(self$childL), is.null(self$childR))
    },

    #' @description
    #' Print a summary of node attributes
    print = function(...) {
      cat("Knoten: \n")
      cat("  Label: ", self$label, "\n", sep = "")
      if (!is.null(self$parent))
        cat("  Elternknoten: ", self$parent$label, "\n", sep = "")
      if(!is.null(self$childL))
        cat("  Kind (L): ", self$childL$label, "\n", sep = "")
      if(!is.null(self$childR))
        cat("  Kind (R): ", self$childR$label, "\n", sep = "")

      cat("  s: ", self$s, "\n", sep = "")
      cat("  j: ", self$j, "\n", sep = "")
      cat("  y: ", self$y, "\n", sep = "")
      cat("  Teilbaumtiefe: ", self$depth, "\n", sep = "")
      cat("  Blatt: ", self$isObst(), "\n", sep = "")

      invisible(self)
    },

    append = function(Child1, Child2) { # Parent (self), Child1, Child2
      # disallow appending if parent node is not a leaf
      stopifnot(is.null(self$childL))
      stopifnot(is.null(self$childR))

      # update attributes for left child
      Child1$parent <- self
      Child1$depth  <- self$depth + 1L

      # update attributes for right child
      Child2$parent <- self
      Child2$depth  <- self$depth + 1L

      # update attributes for parent
      self$childL <- Child1
      self$childR <- Child2
    }
  )
)

#' Coerce a Gabel object to a nested list
#'
#' @param node (Gabel)
#'
#' @return
#' @export
#'
#' @examples
as.list.Gabel <- function(node) {
  if (is.null(node$childL) && is.null(node$childR)) {
    return(structure(
      list(), y = node$y, s = NA, j = NA, depth = node$depth,
      label = node$label, parent = node$parent$label, leaf = node$isObst())
    )
  } else if (!is.null(node$childL) && !is.null(node$childR)) {
    return(structure(
      list(as.list.Gabel(node$childL), as.list.Gabel(node$childR)),
      y = NA, s = node$s, j = node$j, depth = node$depth,
      label = node$label, parent = node$parent$label, leaf = node$isObst())
    )
  } else {
    stop("none or both of node$childL and node$childR must be set")
  }
}

#' R6 class representing a CART
Baum <- R6::R6Class("Baum",
  public = list(
    #' @field nodes
    nodes = list(), # Assumption: (Node.$label == i) => (nodes[[i]] == Node)
    #' @field root
    root = NULL,
    # size = 0L,

    #' @description
    #' Create a new Baum object.
    initialize = function() {
      self$root <- Gabel$new()
      self$root$label <- 1L
      self$root$depth <- 0L
      self$nodes[[1]] <- self$root
    },

    #' @description
    #' @return logical vector with TRUE for leaf nodes and FALSE otherwise. Note
    #'   that leaves are in BFS order.
    obstkorb = function() {
      sapply(self$nodes, function(node) node$isObst())
    },

    #' @description
    #' Add a pair of nodes to the binary tree.
    #' @param label
    #' @param Node
    #'
    #' @return
    append = function(label, Child1, Child2) { # Parent, Child1, Child2
      Parent <- self$nodes[[label]] # range check with [[
      stopifnot(Parent$label == label)

      # update labels
      Child1$label <- length(self$nodes) + 1L
      Child2$label <- length(self$nodes) + 2L

      # update edges
      Parent$append(Child1, Child2)

      # append nodes to list
      self$nodes <- append(self$nodes, Child1)
      self$nodes <- append(self$nodes, Child2)
    },

    validate = function() {
      # ensure all node labels are unique
      v1 <- sapply(self$nodes, function(node) node$label)
      v2 <- seq_along(self$nodes)
      stopifnot(identical(v1, v2))

      # ensure all leaves have $j, $s unset (NA) and $y set
      idx <- self$obstkorb()
      a1 <- sapply(self$nodes[idx], function(node) {
        all(is.na(node$j), is.na(node$s), !is.na(node$y))
      })
      stopifnot(all(a1))

      # ensure all inner nodes have $y unset (NA) and $j, $s set
      a2 <- sapply(self$nodes[!idx], function(node) {
        all(!is.na(node$j), !is.na(node$s), is.na(node$y))
      })
      stopifnot(all(a2))

      # ensure all leaves have at least 1 data point
      a3 <- sapply(self$nodes[idx], function(node) {
        length(node$points) >= 1
      })
      stopifnot(all(a3))
    }
  )
)
