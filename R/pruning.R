#' Title
#'
#' @param x
#' @param node
#' @param mask
#'
#' @return
#' @export
#'
#' @examples
cart_predict_pruned <- function(x, node, mask) { # list or vector
  stopifnot(length(x) == (ncol(node$points)-1))
  stopifnot(is.vector(mask, mode = "logical"))

  if ( !mask[node$label] || (is.null(node$childR) && is.null(node$childL)) ) {
    # leaf node found -> return value
    return(node$y)
  } else if(!mask[[node$childR$label]] && !mask[[node$childL$label]]) {
    # "virtual" leaf node found -> return value
    return(node$y)
  } else if (x[[node$j]] < node$s) {
    return(cart_predict(x, node$childL, mask))
  } else {
    return(cart_predict(x, node$childR, mask))
  }
}

#' Title
#'
#' @param node
#' @param mask
#'
#' @return
#' @export
#'
#' @examples
cart_prune <- function(node, mask) { # list or vector
  stopifnot(is.vector(mask, mode = "logical"))

  if (!mask[node$label] || (is.null(node$childR) && is.null(node$childL)) ) {
    return(mask)
  } else if(!mask[[node$childR$label]] && !mask[[node$childL$label]]) {
    return(mask)
  } else {
    mask[[node$childL$label]] <- FALSE
    mask <- cart_prune(node$childL, mask)

    mask[[node$childR$label]] <- FALSE
    mask <- cart_prune(node$childR, mask)
    return(mask)
  }
}


# cart_greedy_prune <- function(XY, depth = 10L, mode="regression", threshold = 1L,
#                               sample = FALSE, random = FALSE, m = 0L,
#                               quantile = FALSE, q_threshold = 100L, q_pct = 0.25, lambda = "CV") {
#
#   Cart <- cart_greedy(XY, depth = depth, mode = mode, threshold = threshold,
#                       sample = sample, random = random, m = m,
#                       quantile = quantile, q_threshold = q_threshold, q_pct = q_pct)
#
#   return(Cart)
# }
#' Cost-Complexity pruning of greedy CART
#' @description Create a regression or classification tree tree greedily based
#' on training data.
#' @details
#' Unless `sample` is set to `TRUE`, it is required that there are no observations
#' \eqn{(X_{i_1}, Y_{i_1})} and \eqn{(X_{i_2}, Y_{i_2})} with \eqn{X_{i_1} =
#' X_{i_2}}, but \eqn{Y_{i_1} \neq Y_{i_2}}.
#' @param XY matrix with columns \eqn{1..d} (representing the training data
#'   \eqn{X_i}) and column \eqn{y} (for the values \eqn{Y_i}).
#' @param depth The amount of steps before halting the algorithm (defaults to
#'   10)
#' @param threshold The minimum amount of data points for dividing a leaf (`integer`)
#' @param sample Use `sample()` to (`logical`)
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
#' @param lambda Cost weight.
#' @return A regression or classification tree modeled after the training data
#'   (`Baum`), pruned according to Cost-Complexity
#' @examples
#' @export
cart_greedy_prune <-
  function(XY,
           depth = 10L,
           mode = "regression",
           threshold = 1L,
           sample = FALSE,
           random = FALSE,
           m = 0L,
           quantile = FALSE,
           q_threshold = 100L,
           q_pct = 0.25,
           lambda = 0.3) {
    Cart <-
      cart_greedy(
        XY,
        depth = depth,
        mode = mode,
        threshold = threshold,
        sample = sample,
        random = random,
        m = m,
        quantile = quantile,
        q_threshold = q_threshold,
        q_pct = q_pct
      )


    mLeafes <- function(tree, mask) {
      f <- function(node) {
        if (!mask[node$label])
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
          print(node)
          print(node$childL)
          print(node$childR)
          print(mask)
          stop("malformed tree")
        }
      }

     sapply(tree$nodes, f, simplify = TRUE)
    }

    mInnerNodes <- function(tree, mask) {
      !mLeafes(tree, mask) & mask
    }

    innerNodes <- function(tree, mask) {
       tree$nodes[mInnerNodes(tree, mask)]
    }

    Leafes <- function(tree, mask) {
      tree$nodes[mLeafes(tree, mask)]
    }
    #    isLeaf <- function(node, mask) {
    #      return((is.null(node$childR) && is.null(node$childL)) || !mask[[node$childR$label]] && !mask[[node$childL$label]])
    #    }
    mDepthL <- function(tree, Leafmask) {
      max(sapply(tree$nodes[Leafmask], function(node) {`$`(node, "depth")}))
    }


    mDepth <- function(tree, mask) {
      max(sapply(tree$nodes[mask], function(node) {`$`(node, "depth")}))
    }

    complexity <- function(tree, mask) {
      # return(length(tree$obstkorb()[tree$obstkorb() == TRUE]))
      return(sum(mLeafes(tree, mask), na.rm = TRUE))
    }

    pruneAt  <- function(node, mask) {
      if (!mask[[node$label]] || (is.null(node$childR) && is.null(node$childL)) ||
          !mask[[node$childR$label]] && !mask[[node$childL$label]]) {
        return(mask)
      } else {
        mask[[node$childL$label]] <- FALSE
        mask <- pruneAt(node$childL, mask)

        mask[[node$childR$label]] <- FALSE
        mask <- pruneAt(node$childR, mask)
        return(mask)
      }
    }



    Risk <- function(mask)  {

      mtest <- mask

      predict <- function(x, b, m) {
     # print(m)
    #  print(b)
     # print(x)
      return(cart_predict_pruned(x, node = b$root, m))
    }
   #   Cart$validate()
      n <- nrow(XY) # Anzahl Beobachtungen
   #   print(n)
     # print(XY[,-ncol(XY), drop = FALSE])
      #obstCount <- complexity(tree)
      pred <-
        apply(XY[,-ncol(XY), drop = FALSE], MARGIN = 1, FUN = predict, m = mtest, b = Cart) #, par(mask = mask) predicting with current tree
      if (mode == "regression") {
        R <- 1 / n * sum((pred - XY[,ncol(XY), drop = FALSE]) ** 2)
      } else if (mode == "classification") {
        R <- 1 / n * sum((pred != XY[,ncol(XY), drop = FALSE]), na.rm= TRUE)
      } else {
        stop("Invalid mode in Risk(). Must be regression or classification")
      }
      return(R)
    } #END Risk()

    p <- 1
    mT <- list()
    mT[[1]] <- rep(TRUE, times=length(Cart$nodes))

    # mLeavesTp <- Cart$obstkorb()  #logical vector
    # mINodesTp <- mInnerNodes(Cart, maskT[1])        #mask of Nodes without those being Leaves
    iNodesTp <- innerNodes(Cart, mT[[1]])
    cT <- vector()
    cT[1] <- complexity(Cart, mT[[1]])

    while (mDepth(Cart, mT[[p]]) > 0) {
print("STARTwhile")
print(p)
print(mDepth(Cart, mT[[p]]))
#print(mT[[p]])

      # Berechne den weakest link
      mT_test <- vector()
      wlp <- vector()
      i <- 0L
      for (node in iNodesTp ) {
        i <- i + 1
        mT_test <- pruneAt(node, mT[[p]])
#print(mT_test)
        wlp[i] <- (Risk(mT_test) + Risk(mT[[p]])) / (cT[p] - complexity(Cart, mT_test))
      }
print("Finished wlp")
print(wlp)
print(cT)
      pivot <- rank(min(wlp))

print(pivot)
print(length(mLeafes(Cart$mT[[1]])))
#print(iNodesTp)
#print(mT[[p]])
      mT[[p+1]] <- pruneAt(iNodesTp[[pivot]], mT[[p]])

      iNodesTp <- innerNodes(Cart, mT[[p+1]])
      cT[p+1] <- complexity(Cart, mT[[p+1]])
      p = p + 1

    } # END while
    print("Nearly FINISHED!")
    # Berechne den optimal geschnittenen Baum
    P <- p
    p_hat <- vector()
    for (p in 1:P) {
      p_hat[p] <- Risk(mT[[p]]) + lambda * complexity(Cart, mT[[p]])
    } # p_hat <- R_hat(T) + lambda * complexity(T)

    p_hat_min <- rank(min(p_hat))

    print("FINISHED!")

    return(list(Cart, mT[[p_hat_min]]))
  }
