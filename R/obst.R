Obstbaum <- R6::R6Class("Obstbaum",
  public = list(
    root = NA, # differentiate between unset (NA) and undefined (NULL)
    nodes = list(),

    initialize = function(Node) {
      stopifnot(class(Node) == "Obst")
      self$root   <- Node
      self$depth  <- 1
      self$nodes  <- list(Node)
      self$leaves <- list(Node)
    },

    addBranch = function(Node1, Node2) {
      stopifnot(class(Node1) == "Obst")
      stopifnot(class(Node2) == "Obst")

      # nodes[]: add *Node1, *Node2
      self$nodes <- append(self$nodes, c(Node1, Node2))
    },

    validate = function() {
      # TODO: check for cycles (DFS)
    },

    print = function() {
      # TODO: start at the root
    }
  )
)

Obst <- R6::R6Class("Obst",
  public = list(
    child_left = NA, # differentiate between unset (NA) and undefined (NULL)
    child_right = NA,
    parent = NA,

    # attributes (CART)
    s = NA_real_,
    j = NA_integer_,
    y = NA, # < NA_integer_, NA_real_,

    # create edges between tree nodes (bi-directional)
    setChildren = function(Node1, Node2) {
      stopifnot(class(Node1) == "Obst")
      stopifnot(class(Node2) == "Obst")

      # set child pointers
      self$child_left  <- Node1 # *Obst child_left = Node1
      self$child_right <- Node2 # *Obst child_right = Node2

      # set parent pointers
      Node1$parent <- self
      Node2$parent <- self
    },

    print = function(...) {

    }
  )
)
