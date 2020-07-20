source("R/data.R")

#' ATTENTION!! method predict YET TO BE IMPLEMENTED.
#' ATTENTION!! method cart YET TO BE IMPLEMENTED.
#'
#' @description
#' Performs bagging on a dataset x and returns predictions.
#'
#' @param B integer: number of bootstrap samples
#' @param x data.frame: of dimensions Number of Samples x Features
#' @param regression logical specify whether tree is a classification or a
#' regression tree. default=TRUE, TRUE for regression, FALSE for classification
#'
#' @return vector: of size Number of Samples containing bagged predictions to dataset
#' character vector for classification case and double vector for regression case
bagging <- function(B, x, regression=TRUE) {
  nb_samples <- dim(x)[1]
  predictions <- matrix(rep(0., nb_samples * B), nrow=nb_samples, ncol=B)
  trees <- list()

  majorityVote <- function(vector) {
    return (names(which.max(table(vector))))
  }

  for (i in 1:B) {
    x_b <- x[sample(1:nb_samples, size=nb_samples, replace=TRUE), ]
    trees[i] <- cart(x_b, regression=regression) # method yet to be implemented, return cart for x_b
    predictions[, i] <- predict(data=x_b, tree=trees[i]) # method yet to be implemented for trees
  }

  if (regression) return((1/B) * apply(predictions, MARGIN=1, sum))
  else return(apply(predictions, MARGIN=1, majorityVote))

}
