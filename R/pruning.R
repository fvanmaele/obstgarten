#' Prediction for pruned CARTs
#'
#' @description
#' returns y of leaf nodes, but considers only parts of the CART not masked
#'
#' @param x (`vector`) one col of XY except y
#' @param node (`Gabel`) first call with root
#' @param mask (`vector(logical)`) mask for the CART in dfs-order
#'
#' @return y prediction node$y of appropriate sub-node
#'
#' @export
#'
cart_predict_pruned <- function(x, node, mask) {
  stopifnot(length(x) == (ncol(node$points) - 1))
  stopifnot(is.vector(mask, mode = "logical"))
  stopifnot(mask[[node$label]])

  if ((is.null(node$childL) && is.null(node$childR))) {
    # leaf node found -> return value
    return(node$y)
  } else if (!mask[[node$childL$label]] &&
             !mask[[node$childR$label]]) {
    # "virtual" leaf node found -> return value
    if (!is.na(node$y))
      return(0) # problem at root. TODO: Should be mean or majority or better not happening at all
    return(node$y)
  } else if (x[[node$j]] < node$s) {
    return(cart_predict_pruned(x, node$childL, mask)) #recurse to left subnode
  } else {
    return(cart_predict_pruned(x, node$childR, mask)) #recurse to right subnode
  }
}

#' Prune a CART
#'
#' @description
#' returns a mask of the CART with subtree pruned at given node
#'
#' @param node (`Gabel`) node below whitch the mask will be set to FALSE
#' @param mask (`vector(logical)`) mask for the CART in dfs-order
#'
#' @return mask (`vector(logical)`) mask for the pruned CART in dfs-order
#'
#' @export
#'
cart_prune <- function(node, mask) {
  stopifnot(is.vector(mask, mode = "logical"))
  stopifnot(mask[[node$label]])

  if ((is.null(node$childL) &&
       is.null(node$childR))) {
    # not in mask or already pruned
    return(mask)
  } else if (!mask[[node$childL$label]] &&
             !mask[[node$childR$label]]) {
    # already virtually pruned
    return(mask)
  } else {
    mask <- cart_prune(node$childL, mask) # recurse to left child
    mask[[node$childL$label]] <- FALSE

    mask <- cart_prune(node$childR, mask) # recurse to right child
    mask[[node$childR$label]] <- FALSE

    return(mask)
  }
}

#' Cost-Complexity pruning of greedy CART
#'
#' @description Create a regression or classification tree greedily based
#' on training data. Then prune it according to cost-complexity trade-off
#'
#' @param XY matrix with columns \eqn{1..d} (representing the training data
#'   \eqn{X_i}) and column \eqn{y} (for the values \eqn{Y_i}).
#' @param depth The amount of steps before halting the algorithm (defaults to
#'   10)
#' @param threshold The minimum amount of data points for dividing a leaf (`integer`)
#' @param mode `regression` or `classification` specifies whether to train
#'   a regression or classification tree, respectively. Default is "regression"
#' @param random generates CART for random forest (`logical`, default `FALSE`)
#' @param m The
#' default: 0 so it only has to be set at random=TRUE (`numeric`)
#' @param quantile whether to use quantiles for computing the optimal
#'   subdivision (`logical`, defaults to `FALSE`)
#' @param q_threshold minimal of data points for using quantiles (`integer`,
#'   defaults to `100L`)
#' @param q_pct amount of probabilities for `quantile()`, in pct. of the data
#'   set size. (`numeric`, defaults to `0.25`)
#' @param lambda weight for complexity-cost
#' @param use_parallel logical: whether to use parallel computation or not
#'
#' @return A regression or classification tree modeled after the training data
#'   (`Baum`), pruned according to Cost-Complexity
#'
#' @examples cart_greedy_prune(generate_sin_data(100, sigma=0.2), depth=5, threshold=1, lambda = 0.01)
#' @export
#' @import parallel
#'
cart_greedy_prune <-
  function(XY,
           depth = 10L,
           mode = "regression",
           threshold = 1L,
           random = FALSE,
           m = 0L,
           quantile = FALSE,
           q_threshold = 100L,
           q_pct = 0.25,
           lambda = 0.01,
           # TODO CrossValidation
           use_parallel = FALSE) {
    #generate CART based on greedy algorithm
    Cart <-
      cart_greedy(
        XY,
        depth = depth,
        mode = mode,
        threshold = threshold,
        random = random,
        m = m,
        quantile = quantile,
        q_threshold = q_threshold,
        q_pct = q_pct
      )
    stopifnot("lambda is not numeric and greater or equal to 0" = is.numeric(lambda) &
                lambda >= 0L)
    # set up parallel
    if (Sys.info()["sysname"] == "Windows") {
      message("parallel not supported on windows, setting parallel to FALSE")
      use_parallel <- FALSE
    }
    if (use_parallel)
      nbCores <- detectCores() - 1

    # internal helper for generating a mask of leafes
    mLeafes <- function(tree, mask) {
      f <- function(node) {
        if (!mask[[node$label]])
          return(FALSE)
        if ((is.null(node$childL) && is.null(node$childR))) {
          return(TRUE)
        } else if ((!mask[[node$childL$label]] &&
                    !mask[[node$childR$label]])) {
          return(TRUE)
        } else if (mask[[node$childL$label]] &&
                   mask[[node$childR$label]]) {
          return(FALSE)
        } else {
          stop("malformed tree")
        }
      }
      sapply(tree$nodes, f, simplify = TRUE)
    }

    # internal helper for generating a list of leafes
    Leafes <- function(tree, mask) {
      tree$nodes[mLeafes(tree, mask)]
    }
    # internal helper for generating a mask of inner Nodes
    mInnerNodes <- function(tree, mask) {
      !mLeafes(tree, mask) & mask
    }

    # internal helper for generating a list of inner Nodes
    innerNodes <- function(tree, mask) {
      tree$nodes[mInnerNodes(tree, mask)]
    }

    # internal helper for calculating the (virtual) depth of the given masked tree
    mDepth <- function(tree, mask) {
      max(sapply(tree$nodes[mask], function(node) {
        `$`(node, "depth")
      }))
    }

    # internal helper for counting the number of leafes of given masked tree
    complexity <- function(tree, mask) {
      return(sum(mLeafes(tree, mask), na.rm = TRUE))
    }



    # internal helper for calculating the risk estimation according to [Richter, p. 174, Eqs.(6.6)+(6.7)]
    Risk <- function(mask)  {
      n <- nrow(XY) # count of observations
      pred <-
        apply(
          XY[, -ncol(XY), drop = FALSE],
          MARGIN = 1,
          FUN = cart_predict_pruned,
          mask = mask,
          node = Cart$root
        ) # predicting with current tree

      if (mode == "regression") {
        R <- 1 / n * sum((pred - XY[, ncol(XY), drop = FALSE]) ** 2)
      } else if (mode == "classification") {
        R <- 1 / n * sum((pred != XY[, ncol(XY), drop = FALSE]))
      } else {
        stop("Invalid mode in Risk(). Must be regression or classification")
      }
      return(R)
    } #END Risk()

    p <- 1
    mT <- list()
    mT[[1]] <-
      rep(TRUE, times = length(Cart$nodes)) # initial mask for the unpruned CART

    iNodesTp <- list() # list of inner Nodes of T^(p)
    cT <- vector() # number of leafes of T^(p)

    while (mDepth(Cart, mT[[p]]) > 0) {
      # iterate so long as virtual tree is more than just root

      iNodesTp <-
        innerNodes(Cart, mT[[p]]) #mask of Nodes without those being Leaves
      cT[p] <- complexity(Cart, mT[[p]])

      # calculate weakest link according to [Richter, p.179, eqn(6.15)]
      mT_test <- vector()
      wlp <- vector()
      if (use_parallel) {
        wlp <- mclapply(seq_along(iNodesTp), function(i) {
          mT_test <- cart_prune(iNodesTp[[i]], mT[[p]])
          (Risk(mT_test) + Risk(mT[[p]])) / (cT[p] - complexity(Cart, mT_test))
        }, mc.cores = nbCores)
      } else {
        i <- 0L
        for (node in iNodesTp) {
          i <- i + 1
          mT_test <- cart_prune(node, mT[[p]])
          wlp[i] <-
            (Risk(mT_test) + Risk(mT[[p]])) / (cT[p] - complexity(Cart, mT_test))
        }
      }

      # handle edge case where greedyCART gives back NA
      if ((length(wlp) < 2) && is.na(wlp)) {
        pivot <- 1
      } else {
        pivot <- which.min(wlp) # determine actual weakest link
      }

      mT[[p + 1]] <-
        cart_prune(iNodesTp[[pivot]], mT[[p]]) # next mask for T^(p)
      p = p + 1
    } # END while

    P <- p # save the reached p = P
    # calculate cost-complexity trade-off
    p_hat <- vector()
    if (use_parallel) {
      p_hat <- mclapply(1:P, function(p) {
        p_hat[p] <- Risk(mT[[p]]) + lambda * complexity(Cart, mT[[p]])
      }, mc.cores = nbCores)
    } else {
      for (p in 1:P) {
        p_hat[p] <- Risk(mT[[p]]) + lambda * complexity(Cart, mT[[p]])
      } # p_hat <- R_hat(T) + lambda * complexity(T)
    }


    p_hat_min <- which.min(p_hat)
    return(list(Cart, mT[[p_hat_min]])) #return list of CART and cost-complexity pruned mask
  }
