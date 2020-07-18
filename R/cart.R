#' Check if a tree is a regression or a classification tree.
#'
#' This means:
#' * The tree is a binary tree.
#' * Every inner node (a node with 2 child nodes) has a split index and a split
#'   point attribute.
#' * Every leaf has a value y as attribute. For a regression tree, y is numeric;
#'   for a classification tree, y is an integer.
#'
#' @param Tree the tree to check. isCart() is generic and can be implemented for any graph data structure (e.g. igraph).
#' @return 1 for a regression tree, 2 for a classification tree, and 0 otherwise.
#' @seealso [isRegressionTree()], [isClassificationTree()]
isCart <- function(Tree) {
  UseMethod("isCart")
}

#' Implementation of isCart() for data.tree (data.tree package).
isCart.data.tree <- function(Tree) {
  #TODO
}

isCart.default <- function(Tree) {
  stop("argument Tree does not belong to a supported tree class")
}

#' Use isCart() to check if a given tree is a regression tree
#'
#' @param Tree the tree to check.
#' @return TRUE if Tree is a regression tree, FALSE otherwise.
isRegressionTree <- function(Tree) {
  return(isCart(Tree) == 1)
}

#' Use isCart() to check if a given tree is a classification tree
#'
#' @param Tree the tree to check.
#' @return TRUE if Tree is a regression tree, FALSE otherwise.
isClassificationTree <- function(Tree) {
  return(isCart(Tree) == 2)
}

#' Visualize a regression or classification tree
#'
#' The visualisation includes node attributes (compare Richter19, 6.1)
#' @param Tree the tree to check.
plotCart <- function(Tree) {
  UseMethod("plotCart")
}

#' Implementation of plotCart() for data.tree
plotCart.data.tree <- function(Tree) {
  #TODO
}

plotCart.default <- function(Tree) {
  stop("argument Tree does not belong to a supported tree class")
}

#' Return a pruned subtree of a given tree,
#'
#' A pruned subtree is a subtree with the same root as the tree it is taken from.
#' @param Tree the tree to prune.
#' @param Node the first node of the branch to remove.
#' @return A list containing the leaves of the pruned subtree.
pruneBranch <- function(Tree, Node) {
  UseMethod("pruneBranch")
}

#' Implementation of pruneBranch() for data.tree
#'
#' @param Tree the tree to prune.
#' @param Node the first node of the branch to remove.
#' @return A list containing the leaves of the pruned subtree.
pruneBranch.data.tree <- function(Tree, Node) {
  #TODO
}

pruneBranch.default <- function(Tree, Node) {
  # Note: polymorphism on first argument class(Tree)
  stop("argument Tree does not belong to a supported tree class")
}

#' Evaluates a decision rule
#'
#' The decision rule on a CART is given by:
#' \deqn{f(x) = \sum_{m=1}{#T} y_m I_{A_{m}(x)}}
#' where \eqn{x\in\prod([a_i,b_i])} and \eqn{A_m} is the partition induced by
#' the CART.
#'
#' @param Tree the CART to evaluate.
#' @param x a numeric vector representing a given data point x.
#' @return
evalCart <- function(Tree, x) {
  UseMethod("evalCart")
}

evalCart.data.tree <- function(Tree, x) {
  #TODO
}

evalCart.default <- function(Tree, x) {
  stop("argument Tree does not belong to a supported tree class")
}

addSiblings <- function(Node, Child0, Child1) {
  UseMethod("addSiblings")
}

addSiblings.Node <- function(Node, Child0, Child1) {
  stopifnot(class(Node) == class(Child0))
  stopifnot(class(node) == class(Child1))
  # TODO
}

addSiblings.default <- function(Node, Child0, Child1) {
  stop("argument Node does not belong to a supported node class")
}
