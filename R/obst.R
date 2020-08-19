#' R6 class representing a CART
#' @description
#' Data structure (recursive) representing regression and classification trees (CART).
#' Edges are bi-directional (inspired from `data.tree::Node`). Data is stored in
#' environments (`R6`, reference semantics).
#'
#' @details
#' A CART is a binary tree with the following properties:
#' - Every inner node (a node with two child nodes) is assigned a split index
#'   \eqn{j\in{1..d}} and a split point \eqn{s\in R}.
#' - Every leaf (a node with no child nodes) is assigned a value
#'   \eqn{y\in R} (for regression) or \eqn{y\in{1,..,K}} (for classification)
#'
#' Except for the append method, populating the node attributes in a coherent
#' manner is left to the user. For an automated way in doing this, see the
#' `Baum` class.
#' @export
Gabel <- R6::R6Class("Gabel",
  public = list(
    #' @field childL Left child node (`Gabel`, defaults to `NULL`)
    childL = NULL,
    #' @field childR Right child node (`Gabel`, defaults to `NULL`)
    childR = NULL,
    #' @field parent Parent node. A node without parent is the root of the tree
    #' (`Gabel`, defaults to `NULL`)
    parent = NULL,
    #' @field label node label (`integer`, defaults to `NA_integer_`)
    label = NA_integer_,
    #' @field depth depth of the node in the tree, that is the shortest path
    #' \eqn{v_{0} -> .. -> v_{n-1} -> v} from the tree root (`integer`)
    depth = 0L,
    #' @field points (matrix)
    #' TODO: redundant copies of data points in child nodes of CART
    points = NULL,
    #' @field s split point for CART inner node (`numeric`)
    s = NA_real_,
    #' @field j split index for CART inner node (`integer`)
    j = NA_integer_,
    #' @field y value for CART leafs
    y = NA, # < NA_integer_, NA_real_

    #' @description
    #' Determine if a node is a leaf node.
    #' @return `TRUE` if node is a leaf (both `childL` and `childR` are
    #'   `NULL`), `FALSE` otherwise.
    isObst = function() {
      all(is.null(self$childL), is.null(self$childR))
    },

    #' @description
    #' Determine if a node is a root node.
    #' @return `TRUE` if node is a root (parent is set to `NULL`), `FALSE` otherwise.
    isRoot = function() {
      is.null(parent)
    },

    #' @description
    #' Print a summary of node attributes. Attributes set to `NULL` (and the
    #' `$points` attribute) are not printed.
    print = function() {
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

    #' @description
    #' Append a branch (two child nodes to a given node) to a leaf node. Appending
    #' to a node that is not a leaf is not allowed by this method.
    #'
    #' Appending nodes updates the following attributes:
    #' - The `$parent` and `$depth` attributes of the child nodes (`Child1`, `Child2`);
    #' - the `$childL` and `$childR` attributes of the parent node (self).
    #'
    #' @param Child1 The left child node. (`Gabel`)
    #' @param Child2 The right child node. (`Gabel`)
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
#' @details
#' The following attributes are set:
#' - `y` (`Gabel$y`) for leaf nodes;
#' - `s` (`Gabel$s`), `j` (`Gabel$j`) for inner nodes;
#' - `depth` (`Gabel$depth`);
#' - `label` (`Gabel$label`);
#' - `parent` (`Gabel$parent$label`);
#' - `leaf` (the value from `Gabel$isObst()`).
#'
#' It is assumed there are no cycles in the argument node. A node should either
#' have none or both of `childL` and `childR` set.
#' @param node Entry point to follow recursively, usually the root node of
#' the tree (`Gabel`)
#' @return a list of lists representing the tree structure and attributes of
#' `node` (see Details).
#' @export
as_list_Gabel <- function(node) {
  if (is.null(node$childL) && is.null(node$childR)) {
    return(structure(
      list(), y = node$y, s = NA, j = NA, depth = node$depth,
      label = node$label, parent = node$parent$label, leaf = node$isObst())
    )
  } else if (!is.null(node$childL) && !is.null(node$childR)) {
    return(structure(
      list(as_list_Gabel(node$childL), as_list_Gabel(node$childR)),
      y = NA, s = node$s, j = node$j, depth = node$depth,
      label = node$label, parent = node$parent$label, leaf = node$isObst())
    )
  } else {
    stop("none or both of node$childL and node$childR must be set")
  }
}

#' R6 class to build a CART
#' @description
#' A helper class to build a CART based on `Gabel` nodes. Nodes are accessible
#' through a list, for easy introspection with `apply`. Nodes are labeled in BFS
#' order.
#'
#' As `Gabel`, data is stored in environments (`R6`, reference semantics).
#' @export
Baum <- R6::R6Class("Baum",
  public = list(
    #' @field nodes List of `Gabel` nodes in the tree, in BFS order. A node's
    #' label and its index in this list must match. In particular,
    #' `(Node$label == i) => (identical(nodes[[i]], Node) == TRUE)` holds.
    nodes = list(),
    #' @field root The root node of the tree. (`Gabel`)
    root = NULL,

    #' @description
    #' Create a new Baum object.
    #' @details
    #' The root node is initialized with label `1L`
    #' and depth `0L`.
    #' @examples
    #' T1 <- Baum$new()
    #' T1$root$label # 1L
    initialize = function() {
      self$root <- Gabel$new()
      self$root$label <- 1L
      self$root$depth <- 0L
      self$nodes[[1]] <- self$root
    },

    #' @description
    #' Return leaf nodes in the tree.
    #' @seealso
    #' \link{Gabel$isObst()}
    #' @return logical vector with TRUE for leaf nodes, and FALSE otherwise. As
    #' `$nodes`, this vector is in BFS order.
    obstkorb = function() {
      sapply(self$nodes, function(node) node$isObst())
    },

    #' @description
    #' Return depth of the tree.
    #' @details
    #' The tree depth is defined as the maximum depth of leaves in the tree.
    #' @return (`integer`)
    depth = function() {
      leaves <- self$obstkorb()
      max(sapply(self$nodes[leaves], function(node) `$`(node, "depth")))
    },

    #' @description
    #' Add a pair of nodes to the binary tree.
    #' @param Parent The parent node to append to. (`Gabel`)
    #' @param Child1 The left child node to append. (`Gabel`)
    #' @param Child2 The right child node to append. (`Gabel`)
    append = function(Parent, Child1, Child2) { # Parent, Child1, Child2
      stopifnot(identical(self$nodes[[Parent$label]], Parent))

      # update labels
      Child1$label <- length(self$nodes) + 1L
      Child2$label <- length(self$nodes) + 2L

      # update edges
      Parent$append(Child1, Child2)

      # append nodes to list
      self$nodes <- append(self$nodes, Child1)
      self$nodes <- append(self$nodes, Child2)
    },

    #' @description
    #' Check the CART for consistency.
    #' @details
    #' The following conditions are verified:
    #' - all node labels are unique;
    #' - all leaves have `$j` and `$s` set to `NA`;
    #' - all leaves have `$y` not set to `NA`;
    #' - all inner nodes have `$j` and `$s` not set to `NA`;
    #' - all leaves have at least 1 data point.
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

      # ensure all inner nodes have $j, $s set
      a2 <- sapply(self$nodes[!idx], function(node) {
        all(!is.na(node$j), !is.na(node$s))
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
