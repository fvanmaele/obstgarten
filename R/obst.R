Gabel <- R6::R6Class("Gabel",
  public = list(
    childL = NULL,
    childR = NULL,
    parent = NULL,
    label = NA_integer_, # unique node labeling through integers 1...N
    depth = 0L,
    partition = NULL, # partition A(v) of the training data X for node v

    # attributes (CART)
    s = NA_real_,
    j = NA_integer_,
    y = NA, # < NA_integer_, NA_real_

    # create edges between tree nodes (bi-directional)
    setChildren = function(Node1, Node2) { # &Node1, &Node2
      # check labels for uniqueness
      stopifnot("each node must be identified by a label"
                = !anyNA(c(self$label, Node1$label, Node2$label)))
      stopifnot("a node cannot be a child of itself"
                = self$label != Node1$label)
      stopifnot("a node cannot be a child of itself"
                = self$label != Node2$label)

      # set child pointers
      self$childL <- Node1 # *Gabel childL = Node1
      self$childR <- Node2 # *Gabel childR = Node2

      # set parent pointers and increment depth
      Node1$parent <- self
      Node2$parent <- self
      Node1$depth  <- self$depth + 1
      Node2$depth  <- self$depth + 1
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
      if(!is.null(self$partition))
        cat("  Partition: ", "\n")
        print(partition)

      invisible(self)
    }
  )
)

Baum <- R6::R6Class("Baum",
  public = list(
    nodes = list(), # Assumption: (Node.$label == i) => (nodes[[i]] == Node)
    leaves = list(),

    initialize = function(Root) {
      stopifnot("the root node must have label 1" = Root$label == 1L)
      stopifnot("the root node must have depth 0" = Root$depth == 0L)

      self$nodes[[1]] <- Root
      self$leaves[[1]] <- 1L
    },

    obstkorb = function() {
      return(self$nodes[which(!is.na(self$leaves))])
    },

    #' @description
    #' Add a pair of nodes to the binary tree. Bi-directional edges are set
    #' from a specified (unique) label.
    #'
    #' If this method is used exclusively for creating the tree, no cycles
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

      # set bi-directional edges with Node::setChildren
      parentNode$setChildren(Node1, Node2)

      # append nodes to tree
      self$nodes[[label+1]] <- Node1
      self$nodes[[label+2]] <- Node2

      # update labels of leaves
      self$leaves[c(label, label+1, label+2)] <- c(NA, label+1, label+2)
    },

    print = function(...) {
      # TODO: traverse tree (DFS)
      invisible(self)
    }
  )
)
