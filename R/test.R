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

