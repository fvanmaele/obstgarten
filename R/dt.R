#' Implementation of isCart() for data.tree::Node.
#'
#' @param Tree the tree to check.
#' @param mode "regression for a regression tree, "classification" for a
#'   classification tree.
#' @return TRUE if Tree is a regression or classification tree, FALSE otherwise.
isCart.Node <- function(Tree, mode) {
  stopifnot(mode == "regression" || mode == "classification")
  # visit all nodes and check for attributes and amount of children
  n <- length(Tree$children)

  if (n == 2 && all(is.numeric(Tree$s), is.integer(Tree$j))) {
    # inner node: split index and split point (ok) -> visit children
    return(all(sapply(Tree$children, isCart.Node)))
  } else if (n == 2) {
    return(FALSE) # inner node: invalid attributes
  } else if (n == 0) { # leaf: value y
    if (mode == "regression") {
      return(is.numeric(Tree$y))
    } else if (mode == "classification") {
      return(is.integer(Tree$y))
    } # no other modes
  } else { # n-ary tree, n \in 1,3,..
    return(FALSE)
  }
}

#' Implementation of plotCart() for data.tree
plotCart.Node <- function(Tree) {
  # TODO
}

#' Implementation of pruneBranch() for data.tree
#'
#' @param Tree the tree to prune.
#' @param Node the first node of the branch to remove.
#' @return A list containing the leaves of the pruned subtree.
pruneBranch.Node <- function(Tree, Node) {
  # TODO
}

evalCart.Node <- function(Tree, x) {
  #TODO
}

addSiblings.Node <- function(Tree, Child0, Child1) {
  stopifnot(class(Child0) == "Node")
  stopifnot(class(Child1) == "Node")
  # TODO
}
