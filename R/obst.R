#' R6 class representing a CART node
Gabel <- R6::R6Class("Gabel",
  public = list(
    childL = NULL,
    childR = NULL,
    parent = NULL,
    label = NA_integer_, # unique node labeling through integers 1...N
    depth = 0L,
    points = NULL, # training data inside the set A(v)
    # XXX: Dimension d von A(v) als getrenntes Attribut?

    # attributes (CART)
    s = NA_real_,
    j = NA_integer_,
    y = NA, # < NA_integer_, NA_real_

    # create edges between tree nodes (bi-directional)
    setChildren = function(Node1, Node2) { # &Node1, &Node2
      # set child pointers
      self$childL <- Node1 # *Gabel childL = Node1
      self$childR <- Node2 # *Gabel childR = Node2

      # set parent pointers
      Node1$parent <- self
      Node2$parent <- self

      # increment depth
      Node1$depth <- self$depth + 1
      Node2$depth <- self$depth + 1
    },

    isObst = function() {
      all(is.null(self$childL), is.null(self$childR))
    },

    # TODO: print partition
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

Baum <- R6::R6Class("Baum",
  public = list(
    #' @field nodes
    nodes = list(), # Assumption: (Node.$label == i) => (nodes[[i]] == Node)
    #' @field leaves
    leaves = list(),
    #' @field root
    root = NULL,
    # partition = NULL,
    # dim = NA,

    #' @description
    #' Create a new Baum object.
    initialize = function() {
      self$root <- Gabel$new()
      self$root$label  <- 1L
      self$root$depth  <- 0L
      self$nodes[[1]]  <- self$root
      self$leaves[[1]] <- self$root
    },

    #' @description
    #' @return list of leaf nodes
    obstkorb = function() {
      return(self$nodes[which(!is.na(self$leaves))])
    },

    #' @description
    #' Add a pair of nodes to the binary tree. Bi-directional edges are set
    #' from a specified (unique) label.
    #'
    #' @details
    #' If this method is used exclusively for creating a tree, no cycles
    #' are possible: the labels of child nodes are incremented from the label
    #' of the parent node  by +1 and +2 for the left and right node, respectively)
    #' @param label
    #' @param Node
    #' @return
    append = function(label, Node1, Node2) { # Parent, Child1, Child2
      parentNode <- self$nodes[[label]] # range check with [[

      # disallow appending if parent node is not a leaf
      stopifnot(is.null(parentNode$childL))
      stopifnot(is.null(parentNode$childR))

      # increment labels from $label, left to right
      Node1$label <- label+1
      Node2$label <- label+2

      # set bi-directional edges and append nodes
      parentNode$setChildren(Node1, Node2)
      self$nodes[[label+1]] <- Node1
      self$nodes[[label+2]] <- Node2

      # update labels of leaves
      self$leaves[c(label, label+1, label+2)] <- c(NA, label+1, label+2)
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
        P <- matrix(ncol = d, dimnames = list(paste0("V"), node$label))
        P[, node$j] <- node$s

        # descend recursively
        return(rbind(partition(node$childL, d), P, partition(node$childR, d)))
      } else if (is.null(node$childL) && is.null(node$childR)) {
        # leaf node (node$y set)
        return() # NULL (noop for rbind)
      } else {
        stop("none or both of node$childL and node$childR must be set")
      }
    },

    #' @description
    #' @param x vector representing a data point
    #' @return the expected value by evaluating the tree
    predict = function(x) { # list or vector
      #TODO
    },

    #' @description
    #' Print the CART to standard output
    print = function(...) {
      #TODO: traverse tree (DFS)
      invisible(self)
    },

    #' @description
    plot = function(df) {
      df <- self$root$points
      if (is.null(df)) {
        stop("no data available in root node")
      }
      if (ncol(df) > 2) {
        # TODO: support contour plots for 2-dimensional data
        stop("only 1-dimensional plots are supported")
      }

      # TODO: cache for subsequent plots (or set in arguments)
      a <- min(df[, 1])
      b <- max(df[, 1])
      # TODO: check size of x and y
      x <- rbind(a, self$getPartition(self$root, 1), b)[, 1]
      y <- sapply(self$obstkorb(), function(s) `$`(s, "y"))

      plot(df[, 1], df[, 2]) # data points
      plot(x, y, type = "l") # decision rule
    }
  )
)
