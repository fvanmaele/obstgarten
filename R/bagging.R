# library(obstgarten)
library(parallel)

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
#' @param use_parallel: Whether to use parallel computation or not
#' @param random_forest logical: TRUE: random forest, FALSE: bagging
#'
#' @return vector: of size Number of Samples containing bagged predictions to dataset
#' character vector for classification case and double vector for regression case
bagging <- function(B, x_train, x_test, m=NULL, regression=TRUE, use_parallel=FALSE, random_forest = FALSE) {
  stopifnot("B needs to be an integer." = is.integer(B))
  stopifnot("regression needs to be logical" = is.logical(regression))
  stopifnot("random_forest needs to be logical" = is.logical(random_forest))
  stopifnot("x_train and x_test need to be df with more than one col and row"=
              ((is.data.frame(x_train) | is.matrix(x_train)) & ncol(x_train) > 1 & nrow(x_train) > 1
               & (is.data.frame(x_test) | is.matrix(x_test)) & ncol(x_test) > 1 & nrow(x_test) > 1))

  if (regression == TRUE) mode <- "regression"
  else mode <- "classification"

  # dimnames(x_test) <- list(NULL, c(1, "y"))
  nb_samples <- dim(x_train)[1]
  nb_test_samples <- dim(x_test)[1]
  predictions <- matrix(rep(0., nb_test_samples * B), nrow=nb_test_samples, ncol=B)
  trees <- list()

  majorityVote <- function(vector) {
    return (names(which.max(table(vector))))
  }

  # train cart
  fit_tree <- function(x_b, random, m) {
    # (over-)fitting tree to bootstrap sample via CART algorithm
    # dimnames(x_b) <- list(NULL, c(1, "y"))
    trees[[i]] <- cart_greedy(x_b, depth=5, mode = mode, threshold=1, random = random, m = m, allow_duplicates = TRUE) # return cart for x_b
    trees[[i]]$validate()

    predict <- function(x) {
      return(cart_predict(x, node=trees[[i]]$root))
    }

    if (regression) return(apply(x_test[, -ncol(x_test), drop=FALSE], MARGIN=1, predict))
    else return(round(apply(x_test[, -ncol(x_test), drop=FALSE], MARGIN=1, predict)))
  }

  # sample bootstraps
  X_B <- list()
  for (i in 1:B) {
    X_B[[i]] <- x_train[sample(1:nb_samples, size=nb_samples, replace=TRUE), ]
  }

  if (is.null(m)) {
    m <- 0
    if(random_forest){
      if(regression){
        m <- floor((ncol(x_train)-1)/3)
        if(m < 1){
          m <- 1
        }
      }
      else{
        m <- floor(sqrt(ncol(x_train)-1))
        if(m < 1){
          m <- 1
        }
      }
    }
  }

  if (use_parallel) {
    # set up parallel
    nb_cores <- detectCores() - 1
    cluster_pred <- makeCluster(nb_cores)
    clusterEvalQ(cluster_pred, {
      library(obstgarten)})

    predictions <- matrix(unlist(parLapply(cluster_pred, X_B, fit_tree, random_forest, m)), nrow=dim(x_test)[[1]], ncol=B)

    stopCluster(cluster_pred) # close cluster
  }
  else {
    predictions <- matrix(unlist(lapply(X_B, fit_tree, random_forest, m)), nrow=dim(x_test)[[1]], ncol=B)
  }

  # returning predictions for test set via mean in regression case and via majority vote in classification case
  if (regression) return((1/B) * apply(predictions, MARGIN=1, sum))
  else return(apply(predictions, MARGIN=1, majorityVote))

}


# ptm <- proc.time()
# n <- 150
# M <- generate_sin_data(n, sigma=0.2)
# dimnames(M) <- list(NULL, c(1, "y"))
# y <- bagging(100, x_train = M, x_test = M, use_parallel = TRUE)
#
# plot(M[, 1], M[, 2])
# plot(M[, 1], y)
# proc.time() - ptm
#
# ptm <- proc.time()
# n <- 150
# M <- generate_sin_data(n, sigma=0.2)
# dimnames(M) <- list(NULL, c(1, "y"))
# y <- bagging(100L, x_train = M, x_test = M, use_parallel = FALSE)

# plot(M[, 1], M[, 2])
# plot(M[, 1], y)
# proc.time() - ptm

