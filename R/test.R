L <- function(y, s){
  return((y-s)>1e-5)
}

createTree <- function(X, Y, depth){
  T <- data.tree::Node$new("0", s = mean(X), depth = 0)
  for(i in 1:depth){
    data.tree::T$AddChild(sprintf("%d", i), s = mean(X), depth = i)

  }
}

.Tree <- setClass("Tree",
                    slots = c(childs = "list", depth = "numeric", s = "numeric", y = "numeric")
)
.BinaryTree <- setClass("BinaryTree",
                  contains = "Tree",
                  slots = c(parent = "list")
)
a <- .Tree(childs = list(b1, b2), depth = 0, s = 0.5)
b1 <- .Tree(childs = list(c1), depth = 1, s = 0.25)
b2 <- .Tree(childs = list(c2), depth = 1, s = 0.25)
c1 <- .Tree(childs = list(), depth = 2, y = )
c2 <- .Tree(childs = list(), depth = 2)

#---------------------------------------
#R6 test
library(R6)

Tree <- R6Class("Tree", list(
  name = NULL,
  root = NA,
  initialize = function(name, root) {
    stopifnot(is.character(name), length(name) == 1)

    self$name <- name
    self$root <- root
  },

  print = function(...) {
    cat("Baum: \n")
    cat("  Name: ", self$name, "\n", sep = "")
    cat("  Tiefe:  ", self$get_depth(), "\n", sep = "")
    invisible(self)
  },

  set_root = function(root){
    self$root <- root
  },

  get_root = function(){
    self$root
  },

  get_depth = function(){
    return(self$get_root()$get_depth())
  }
))

Node <- R6Class("Node", list(
  s = 0,
  left_child = 0,
  right_child = 0,
  initialize = function(s = 0) {
    self$s <- s
  },

  print = function(...) {
    cat("Knoten: \n")
    cat("  s: ", self$s, "\n", sep = "")
    cat("  Teilbaumtiefe:  ", self$get_depth(), "\n", sep = "")
    cat("  Blatt:  ", self$is_leef(), "\n", sep = "")
    invisible(self)
  },

  add_left_child = function(node){
    self$left_child <- node
  },

  add_right_child = function(node){
    self$right_child <- node
  },

  get_left_child = function(){
    self$left_child
  },

  get_right_child = function(){
    self$right_child
  },

  get_depth = function(){
    if(class(self$left_child) != "Node" && class(self$right_child) != "Node"){
      return(1)
    }
    if(class(self$left_child) == "Node" && class(self$right_child) != "Node"){
      return(self$left_child$get_depth() + 1)
    }
    if(class(self$left_child) != "Node" && class(self$right_child) == "Node"){
      return(self$right_child$get_depth() + 1)
    }
    return(max(self$left_child$get_depth() + 1, self$right_child$get_depth() + 1))
  },

  is_leef = function(){
    return( self$get_depth() == 1 )
  }
))

a <- Node$new()
a$add_left_child(Node$new(1))
a$add_right_child(Node$new(2))
a$left_child$add_left_child(Node$new(3))
a$left_child$add_right_child(Node$new(4))
b_tree1 <- Tree$new("binary tree 1", a)
b_tree1
a
a$left_child$left_child

create_tree <- function(node, depth){
  if(depth > 0){
    node$add_left_child(Node$new())
    node$add_right_child(Node$new())
    create_tree(node$get_left_child(), depth - 1)
    create_tree(node$get_right_child(), depth - 1)
  }
}
b <- Node$new()
create_tree(b, 10)
b_tree2 <- Tree$new("binary tree 2", b)
b_tree2
b


