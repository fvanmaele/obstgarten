Obst <- R6::R6Class("Obst",
  public = list(
    # differentiate between unset (NA) and undefined (NULL)
    child_left = NA,
    child_right = NA,
    parent = NA,
    label = NA_integer_, # unique node labeling through integers 0...N
    depth = 0L,
    partition = list(), # induced partition of space X (CART algorithm)

    # attributes (CART)
    s = NA_real_,
    j = NA_integer_,
    y = NA, # < NA_integer_, NA_real_

    initialize = function(label) {
      stopifnot(is.integer(label))
      self$label <- label # label is required
    },

    # create edges between tree nodes (bi-directional)
    setChildren = function(Node1, Node2) { # &Node1, &Node2
      stopifnot(class(Node1) == "Obst")
      stopifnot(class(Node2) == "Obst")

      # check labels for uniqueness
      stopifnot("each node must be identified by a label"
                = !anyNA(c(self$label, Node1$label, Node2$label)))
      stopifnot("a node cannot be a child of itself"
                = self$label != Node1$label)
      stopifnot("a node cannot be a child of itself"
                = self$label != Node2$label)

      # set child pointers
      self$child_left  <- Node1 # *Obst child_left = Node1
      self$child_right <- Node2 # *Obst child_right = Node2

      # set parent pointers
      Node1$parent <- self
      Node2$parent <- self

      # increment depth
      Node1$depth <- self$depth + 1
      Node2$depth <- self$depth + 1
    },

    is_leaf = function() {
      all(is.na(child_left), is.na(child_right))
    },

    print = function(...) {
      cat("Knoten: \n")
      cat("  s: ", self$s, "\n", sep = "")
      cat("  s: ", self$j, "\n", sep = "")
      cat("  y: ", self$y, "\n", sep = "")
      cat("  Teilbaumtiefe:  ", self$depth, "\n", sep = "")
      cat("  Blatt:  ", self$is_leaf(), "\n", sep = "")

      invisible(self)
    }
  )
)

Obstbaum <- R6::R6Class("Obstbaum",
  public = list(
    # Assumption: (Node.$label == i) => (nodes[[i]] == Node)
    nodes = list(),
    # In particular, nodes[[0]] == root
    root = function() { return(nodes[[0]]) },

    initialize = function(Node) {
      stopifnot(class(Node) == "Obst")
      stopifnot("the root node must have label 0" = Node$label == 0L)
      self$nodes[[0]] <- Node
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
      parent <- self$nodes[[label]] # range check with [[

      # labels are initialized inside the tree
      stopifnot(identical(Node1$label, NA_integer_))
      stopifnot(identical(Node2$label, NA_integer_))

      # disallow appending if parent is not a leaf
      stopifnot(is.na(parent$child_left))
      stopifnot(is.na(parent$child_right))

      # increment labels from $label, left to right
      Node1$label = label + 1
      Node2$label = label + 2

      # use Node::setChildren for remaining logic
      parent$setChildren(Node1, Node2)
    },

    dfs = function() {
      # TODO
    }

    validate = function() {
      # TODO: check for cycles (DFS)
    },

    print = function(...) {
      # TODO: traverse tree (DFS)
      invisible(self)
    }
  )
)
