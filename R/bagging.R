#' ATTENTION!! method predict YET TO BE IMPLEMENTED.
#' ATTENTION!! method cart YET TO BE IMPLEMENTED.
#'
#' @description
#' Performs bagging on a dataset and returns predictions.
#'
#' @param B integer: number of bootstrap samples
#' @param x_train data.frame: training data of dimensions Number of Samples x Features
#' @param x_test data.frame: test data of dimensions Number of Samples x Features
#' @param regression logical: specify whether tree is a classification or a
#' regression tree. default=TRUE, TRUE for regression, FALSE for classification
#'
#' @return vector: of size Number of Samples containing bagged predictions to dataset
#' character vector for classification case and double vector for regression case
bagging <- function(B, x_train, x_test, regression=TRUE) {
  nb_samples <- dim(x_train)[1]
  nb_test_samples <- dim(x_test)[1]
  predictions <- matrix(rep(0., nb_test_samples * B), nrow=nb_test_samples, ncol=B)
  trees <- list()

  majorityVote <- function(vector) {
    return (names(which.max(table(vector))))
  }

  for (i in 1:B) {
    # drawing bootstrap sample from training data with replacement
    x_b <- x_train[sample(1:nb_samples, size=nb_samples, replace=TRUE), ]

    # (over-)fitting tree to bootstrap sample via CART algorithm
    trees[i] <- cart(x_b, regression=regression) # method yet to be implemented, return cart for x_b

    # predicting with tree
    predictions[, i] <- predict(data=x_test, tree=trees[i]) # method yet to be implemented for trees
  }

  # returning predictions for test set via mean in regression case and via majority vote in classification case
  if (regression) return((1/B) * apply(predictions, MARGIN=1, sum))
  else return(apply(predictions, MARGIN=1, majorityVote))

}

