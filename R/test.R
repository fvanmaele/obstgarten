L <- function(y, s){
  return((y-s)>1e-5)
}

createTree <- function(X, Y, depth){
  T <- data.tree::Node$new("0", s = mean(X), depth = 0)
  for(i in 1:depth){
    data.tree::T$AddChild(sprintf("%d", i), s = mean(X), depth = i)

  }
}
