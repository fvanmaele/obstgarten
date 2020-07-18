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

