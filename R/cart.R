#' Check if a tree is a regression or a classification tree.
#'
#' This means:
#' * The tree is a binary tree.
#' * Every inner node (a node with 2 child nodes) has a split index (integer)
#'   and a split point (numeric) attribute.
#' * Every leaf has a value y as attribute. For a regression tree, y is numeric;
#'   for a classification tree, y is an integer.
#'
#' @param Tree the tree to check. isCart() is generic and can be implemented for
#'   any graph data structure (e.g. igraph).
#' @param mode "regression" for a regression tree, "classification" for a
#'   classification tree.
#' @return TRUE if Tree is a regression or classification tree, FALSE otherwise.
#' @seealso [isRegressionTree()], [isClassificationTree()]
isCart <- function(Tree, ...) {
  UseMethod("isCart")
}

isCart.Baum <- function(Tree, mode) {
  # TODO
}

#' Use isCart() to check if a given tree is a regression tree
#'
#' @param Tree the tree to check.
#' @return TRUE for a regression tree, FALSE otherwise.
isRegressionTree <- function(Tree) {
  return(isCart(Tree, mode = "regression"))
}

#' Use isCart() to check if a given tree is a classification tree
#'
#' @param Tree the tree to check.
#' @return TRUE for a classification tree, FALSE otherwise.
isClassificationTree <- function(Tree) {
  return(isCart(Tree, mode = "classification"))
}

#' Visualize a regression or classification tree
#'
#' The visualisation includes node attributes (compare Richter19, 6.1)
#' @param Tree the tree to check.
plotCart <- function(Tree) {
  UseMethod("plotCart")
}

plotCart.Baum <- function(Tree) {
  #TODO
}

#' Return a pruned subtree of a given tree,
#'
#' A pruned subtree is a subtree with the same root as the tree it is taken from.
#' @param Tree the tree to prune.
#' @param Node the first node of the branch to remove.
#' @return A list containing the leaves of the pruned subtree.
pruneBranch <- function(Tree, ...) {
  UseMethod("pruneBranch")
}

pruneBranch.Baum <- function(Tree, Node) {
  #TODO
}

#' Evaluate a decision rule
#'
#' The decision rule on a CART is given by:
#' \deqn{f(x) = \sum_{m=1}{#T} y_m I_{A_{m}(x)}}
#' where \eqn{x\in\prod([a_i,b_i])} and \eqn{A_m} is the partition induced by
#' the CART.
#'
#' @param Tree the CART to evaluate.
#' @param x a numeric vector representing a given data point x.
#' @return
evalCart <- function(Tree, ...) {
  UseMethod("evalCart")
}

evalCart.Baum <- function(Tree, x) {
  #TODO
}

# TODO: Node or Tree?
addSiblings <- function(Tree, ...) {
  UseMethod("addSiblings")
}

addSiblings.Baum <- function(Tree, Node, Child0, Child1) {
  stopifnot(class(Node) == "Gabel")
  stopifnot(class(Child0) == "Gabel")
  stopifnot(class(Child1) == "Gabel")
  #Node$setChildren(Child0, Child1)
}

