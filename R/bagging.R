source("R/data.R")

#' method tree.predict YET TO BE IMPLEMENTED.
#'
#' @description
#' Performs bootstrap aggregation with a tree on a dataset x and returns predictions.
#'
#' @param B integer: number of bootstrap samples
#' @param x data.frame: of dimensions Number of Samples x Features
#' @param tree Tree-class on which bagging algorithm should be used on
#' @param regression logical specify whether tree is a classification or a
#' regression tree. default=TRUE, TRUE for regression, FALSE for classification
#'
#' @return vector: of size Number of Samples containing bagged predictions to dataset
#' character vector for classification case and double vector for regression case
bagging <- function(B, x, tree, regression=TRUE) {
  nb_samples <- dim(x)[1]
  predictions <- matrix(rep(0., nb_samples * B), nrow=nb_samples, ncol=B)

  majorityVote <- function(vector) {
    return (names(which.max(table(vector))))
  }

  for (i in 1:B) {
    x_b <- x[sample(1:nb_samples, size=nb_samples, replace=TRUE), ]
    predictions[, i] <- tree.predict(x_b) # method yet to be implemented for trees
  }

  if (regression) return((1/B) * apply(predictions, MARGIN=1, sum))
  else  return(apply(predictions, MARGIN=1, majorityVote))
}


# l <- c(1, 1, 1, 2, 3, 4, 5, 2)
# names(which.max(table(l)))
#
# predict <- function(x) {
#   return (apply(x, MARGIN=1, mean))
# }
#
# iris <- load_iris()
# iris <- iris[, 1:4]
# iris
#
# bagging(10, iris, "helo", regression=FALSE)
