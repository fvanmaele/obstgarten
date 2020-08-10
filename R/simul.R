library(obstgarten)
library(tidyverse)

# plots prediction of CART generated regression tree
#'
#' @param depth Integer depths of the CART generated regression tree
#' @example pred_plot_greedy(5, sigma=0.25, n=150)
pred_plot_greedy <- function(depth, sigma=0.25, n=150, random=FALSE) {
  if (random == TRUE & depth <= 2) {
    stop("Random Forest require depth > 1!")
  }

  grid <- seq(0, 1, len=n)

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  x <- generate_sin_data(n, grid=grid, sigma=sigma)

  dimnames(x) <- list(NULL, c(1, "y"))
  tree <- cart_greedy(x, depth=depth, random=random)
  pred <- apply(x[, 1, drop=FALSE], MARGIN=1, predict) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2])) +
    ggtitle(str_c("Prediction of CART Regression Tree with Depth ", depth)) +
    xlab("") +
    ylab("")

  print(gg)

}

pred_plot_greedy(depth=3, random=TRUE)


#' plots prediction of Bagging generated regression tree with depth 5
#' and specified number of bootstrap samples B
#'
#' @param B integer number of bootstrap samples
#' @example pred_plot_bagging(100, sigma=0.25, n=150)
pred_plot_bagging <- function(B, sigma=0.25, n=150) {

  grid <- seq(0, 1, len=n)

  x <- generate_sin_data(n, grid=grid, sigma=sigma)

  pred <- bagging(B=B, x_train=x, x_test=x) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2])) +
    ggtitle(str_c("Prediction of CART Regression Tree with ", B, " Bootstrap Samples")) +
    xlab("") +
    ylab("")

  print(gg)

}

#' bias variance data for 400 reps for CARTs of different depths
#' @example generate test date for different depths values of the CART algorithm with 400 reps
#' and 150 data points bv_greedy(list(2L, 5L, 10L, 15L), n=150, reps=400, sigma=0.25)
bv_greedy <- function(depths_list, sigma=0.2, n=150, reps=400) {

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  params_list <- list()
  ret <- list()
  count <- 1

  for (depth in depths_list) {
    pred <- matrix(0., nrow=reps, ncol=n)

    params_list[[count]] <- depth
    grid <- seq(0, 1, len=n)

    for (i in 1:reps) {
      x <- generate_sin_data(n, grid=grid, sigma=sigma)
      dimnames(x) <- list(NULL, c(1, "y"))
      tree <- cart_greedy(x, depth=depth)
      pred[i, ] <- apply(x[, 1, drop=FALSE], MARGIN=1, predict) # predicting with current tree
    }

    ret[[count]] <- apply(pred, MARGIN=2, function(x) c(mean(x), sd(x)))
    count <- count + 1
  }
  bv_data <- list(params_list, ret)
  save("bv_data", file=str_c("data/simul/","bv_greedy_", format(Sys.time(), "%Y%m%d-%H%M%S")))
}


#' bias variance plot for CARTs for different depths
#'
#' @example load("data/simul/bv_greedy_20200810-101102")
#'          bv_plot(bv_data)
bv_plot <- function(data) {

  grid <- seq(0, 1, len=150)

  depth_list <- data[[1]]
  data <- data[[2]]
  pd <- position_dodge(0.1)

  plt_data <- data[[1]]
  df1 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
  plt_data <- data[[2]]
  df2 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
  plt_data <- data[[3]]
  df3 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
  plt_data <- data[[4]]
  df4 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])

  gg <- ggplot(df1, aes(x=grid, y=mean)) +
    scale_colour_manual("",
                        breaks = c("depth 2", "depth 5", "depth 10", "depth 15"),
                        values = c("depth 2"="grey", "depth 5"="green", "depth 10"="red",
                                   "depth 15"="blue")) +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "blue", data=df4, alpha=0.5, outline.type="both") +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "red", data=df3, alpha=0.5, outline.type="both") +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "green", data=df2, alpha=0.5, outline.type="both") +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "grey", alpha=0.5, outline.type="both") +
    geom_line(aes(x=grid, y=sin(2*pi*grid))) +
    geom_line(aes(x=grid, y=mean, colour="depth 15"), data=df4, alpha=1) +
    geom_line(aes(x=grid, y=mean, colour="depth 10"), data=df3, alpha=1) +
    geom_line(aes(x=grid, y=mean, colour="depth 5"), data=df2, alpha=1) +
    geom_line(alpha=1, aes(colour="depth 2")) +
    ggtitle("Prediction of different CART generated Trees") +
    ylab("") +
    xlab("")

  print(gg)

}


#' bias variance data for 400 reps for Bagging with different numbers of Bootstrap samples
#'
#' @param bs_list List of different integer bagging parameters B to be tested
#'
#' @example generate test date for different depths values of the CART algorithm with 400 reps
#' and 150 data points
#' bv_bagging(list(1L, 5L, 25L, 100L), n=150, reps=400, sigma=0.25)
bv_bagging <- function(bs_list, sigma=0.2, n=150, reps=400) {

  params_list <- list()
  ret <- list()
  count <- 1

  for (bs in bs_list) {
    pred <- matrix(0., nrow=reps, ncol=n)

    params_list[[count]] <- bs
    grid <- seq(0, 1, len=n)

    for (i in 1:reps) {
      x <- generate_sin_data(n, grid=grid, sigma=sigma)
      # predicting with current Bagging alg
      pred[i, ] <- bagging(bs, x_train=x, x_test=x, regression=TRUE, use_parallel=FALSE)
    }

    ret[[count]] <- apply(pred, MARGIN=2, function(x) c(mean(x), sd(x)))
    count <- count + 1
  }
  bv_data <- list(params_list, ret)
  save("bv_data", file=str_c("data/simul/","bv_bagging_", format(Sys.time(), "%Y%m%d-%H%M%S")))
}

#load("data/simul/bv_greedy_20200810-101102")
#bv_plot(bv_data)
