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
    #' @return logical TRUE if a node is a leaf, FALSE otherwise.
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
    }
  )
)

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
    #' @return logical vector with TRUE for leaf nodes and FALSE otherwise
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

      # disallow appending if parent node is not a leaf
      stopifnot(is.null(Parent$childL))
      stopifnot(is.null(Parent$childR))

      # update attributes for parent
      Parent$childL <- Child1
      Parent$childR <- Child2

      # update attributes for left child
      Child1$label  <- length(self$nodes) + 1L
      Child1$parent <- Parent
      Child1$depth  <- Child1$depth + 1L

      # update attributes for right child
      Child2$label  <- length(self$nodes) + 2L
      Child2$parent <- Parent
      Child2$depth  <- Child2$depth + 1L

      # append nodes to list
      self$nodes <- append(self$nodes, Child1)
      self$nodes <- append(self$nodes, Child2)
    },

    #' @description
    #' @param node entry point
    #' @param d dimension
    #' @return
    partition = function(node, d) {
      if (!is.null(node$childL) && !is.null(node$childR)) {
        # check if partition attributes are set
        stopifnot(!anyNA(node$j, node$s))

        # parent node (node$j, $node$s set)
        P <- matrix(node$label, ncol = d, dimnames = list(paste0("V")))
        P[, node$j] <- node$s

        # descend recursively
        return(rbind(self$partition(node$childL, d), P,
                     self$partition(node$childR, d)))
      }
      else if (is.null(node$childL) && is.null(node$childR)) {
        # leaf node (node$y set)
        return() # NULL (noop for rbind)
      }
      else {
        stop("none or both of node$childL and node$childR must be set")
      }
    },

    #' @description
    #' @param x vector representing a data point
    #' @return the expected value by evaluating the tree
    predict = function(x, node = self$root) { # list or vector
      # check if vector x matches tree dimension
      # TODO: implement dim/d (length x) as separate (Gabel?) attribute
      stopifnot(length(x) == (ncol(node$points)-1))

      if (is.null(node$childR) && is.null(node$childL)) {
        # leaf node found -> return value
        return(node$y)
      } else if (x[[node$j]] < node$s) {
        return(self$predict(x, node$childL))
      } else {
        return(self$predict(x, node$childR))
      }
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

      # TODO: cycles
    },

    #' @description
    #' Plot a CART with 1 or 2-dimensional data
    plot = function() {
      XY <- self$root$points
      if (is.null(XY)) {
        stop("no data available in root node")
      }
      if (ncol(XY) > 2) {
        # TODO: support contour plots for 2-dimensional data
        stop("only 1-dimensional plots are supported for now")
      }

      # TODO: cache for subsequent plots (or set in arguments)
      a <- min(XY[, 1])
      b <- max(XY[, 1])

      # TODO: check size of x and y
      o <- self$nodes[self$obstkorb()]
      x <- rbind(a, self$getPartition(self$root, 1), b)[, 1]
      y <- sapply(o, function(s) `$`(s, "y"))

      # Combined plot
      ggplot() + geom_point(data=X) + geom_step(data=data.frame(x1 = x, y = y))
    }
  )
)
