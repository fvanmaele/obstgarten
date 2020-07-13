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

# https://adv-r.hadley.nz/r6.html
library(R6)

Tree <- R6Class("Tree", list(
  name = NULL,
  root = NA,
  sub_root = NA,
  initialize = function(name, root) {
    stopifnot(is.character(name), length(name) == 1)

    self$name <- name
    self$root <- root
  },

  print = function(...) {
    cat("Baum: \n")
    cat("  Name: ", self$name, "\n", sep = "")
    cat("  Tiefe:  ", self$get_depth(), "\n", sep = "")
    cat("  S:  ", self$get_s(), "\n", sep = "")
    cat("  Y:  ", self$get_y(), "\n", sep = "")

    x <- c(0, self$get_s(), 1)
    for (i in 1:(length(x)-1)) {
      x[i] <- mean(x[i:(i+1)])
    }
    x <- x[1:(length(x)-1)]
    y <- self$get_y()
    plot(x,y)
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
  },

  get_s = function(){
    return(self$get_root()$get_s())
  },

  get_y = function(){
    return(self$get_root()$get_y())
  }
))

Node <- R6Class("Node", list(
  s = NULL,
  y = NULL,
  left_child = 0,
  right_child = 0,
  initialize = function() {

  },

  print = function(...) {
    cat("Knoten: \n")
    cat("  s: ", self$s, "\n", sep = "")
    cat("  y: ", self$y, "\n", sep = "")
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

  get_s = function(){
    if(class(self$left_child) != "Node" && class(self$right_child) != "Node"){
      return(self$s)
    }
    if(class(self$left_child) == "Node" && class(self$right_child) != "Node"){
      return(c(self$left_child$get_s(), self$s))
    }
    if(class(self$left_child) != "Node" && class(self$right_child) == "Node"){
      return(c(self$s ,self$right_child$get_s()))
    }
    return( c(self$left_child$get_s(), self$s, self$right_child$get_s()) )
  },

  get_y = function(){
    if(class(self$left_child) != "Node" && class(self$right_child) != "Node"){
      return(self$y)
    }
    if(class(self$left_child) == "Node" && class(self$right_child) != "Node"){
      return(c(self$left_child$get_y(), self$y))
    }
    if(class(self$left_child) != "Node" && class(self$right_child) == "Node"){
      return(c(self$y ,self$right_child$get_y()))
    }
    return( c(self$left_child$get_y(), self$right_child$get_y()) )
  },

  is_leef = function(){
    return( self$get_depth() == 1 )
  }
))

a <- Node$new()
a$add_left_child(Node$new())
a$add_right_child(Node$new())
a$left_child$add_left_child(Node$new())
a$left_child$add_right_child(Node$new())
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


#---------------------------------------


# simulate some data
set.seed(0)
n <- 200
x <- runif(n)
y <- 1 + 2*x + rnorm(n, mean = 0, sd = 0.5)
y <- sin(2*pi*x) + rnorm(n, mean = 0, sd = 0.2**2)
plot(x,y)

create_regression_tree <- function(node, x, y, depth){
  if(depth > 0){
    s = mean(x)
    node$s = s
    node$add_left_child(Node$new())
    node$add_right_child(Node$new())
    create_regression_tree(node$get_left_child(), x[x<s], y[x<s], depth - 1)
    create_regression_tree(node$get_right_child(), x[x>=s], y[x>=s], depth - 1)
  }
  else{
    node$y = mean(y)
  }
}

b <- Node$new()
create_regression_tree(b, x, y, 3)
b_regression_tree <- Tree$new("binary regression tree", b)
b_regression_tree
b
