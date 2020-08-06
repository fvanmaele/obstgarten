library(obstgarten)

#' ATTENTION!! method not finished for classification case.
#'
#' @description
#' Performs bagging on a dataset and returns predictions.
#'
#' @param B integer: number of bootstrap samples
#' @param x_train data.frame: training data with labels of dimensions Number of Samples x Features + 1
#' @param x_test data.frame: test data with labels of dimensions Number of Samples x Features + 1
#' @param regression logical: specify whether tree is a classification or a
#' regression tree. default=TRUE, TRUE for regression, FALSE for classification
#'
#' @return vector: of size Number of Samples containing bagged predictions to dataset
#' character vector for classification case and double vector for regression case
bagging <- function(B, x_train, x_test, regression=TRUE) {
  dimnames(x_test) <- list(NULL, c(1, "y"))
  nb_samples <- dim(x_train)[1]
  nb_test_samples <- dim(x_test)[1]
  predictions <- matrix(rep(0., nb_test_samples * B), nrow=nb_test_samples, ncol=B)
  trees <- list()

  majorityVote <- function(vector) {
    return (names(which.max(table(vector))))
  }

  if (regression) {

  for (i in 1:B) {
    # drawing bootstrap sample from training data with replacement
    x_b <- x_train[sample(1:nb_samples, size=nb_samples, replace=TRUE), ]

    # (over-)fitting tree to bootstrap sample via CART algorithm
    dimnames(x_b) <- list(NULL, c(1, "y"))
    trees[[i]] <- cart_greedy(x_b, depth=5, threshold=1) # return cart for x_b
    trees[[i]]$validate()

    predict <- function(x) {
      return(cart_predict(x=x, node=trees[[i]]$root))
    }

    predictions[, i] <- apply(x_test[, 1, drop=FALSE], MARGIN=1, predict) # predicting with current tree

  }

  }

  else {
    stop("classification case not yet implemented!")
  }

  # returning predictions for test set via mean in regression case and via majority vote in classification case
  if (regression) return((1/B) * apply(predictions, MARGIN=1, sum))
  else return(apply(predictions, MARGIN=1, majorityVote))

}

# M <- generate_sin_data(n, sigma=0.2)
# dimnames(M) <- list(NULL, c(1, "y"))
# y <- bagging(10, x_train = M, x_test = M)

# plot(M[, 1], M[, 2])
# plot(M[, 1], y)


