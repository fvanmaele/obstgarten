#' bias variance data for 400 reps for CARTs of different depths
#'
#' @description generate test data for different depths values of the CART algorithm with 400 reps and 150 data points
#'
#' @param depths_list List of different integer depth parameters to be tested
#' @param sigma standard deviation of irreducible error in y
#' @param n number of generated data pairs
#' @param reps rows of data
#'
#' @examples  bv_greedy(list(2L, 5L, 10L, 15L), n=150, reps=400, sigma=0.25)
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
      pred[i, ] <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree
    }

    ret[[count]] <- apply(pred, MARGIN=2, function(x) c(mean(x), sd(x)))
    count <- count + 1
  }
  bv_data <- list(params_list, ret)
  save("bv_data", file=str_c("data/simul/","bv_greedy_", format(Sys.time(), "%Y%m%d-%H%M%S")))
}


#' bias variance data for 400 reps for pruned CARTs with different pruning parameter
#'
#' @description  generate test data for different depths values of the CART algorithm with 400 reps and 150 data points
#'
#' @param lambda_list List of different double lambda parameters to be tested
#' @param sigma standard deviation of irreducible error in y
#' @param n number of generated data pairs
#' @param reps rows of data
#'
#' @examples bv_pruning(list(0., 0.001, 0.01, 0.03), n=150, reps=50, sigma=0.25)
bv_pruning <- function(lambda_list, sigma=0.2, n=150, reps=50) {

  predict <- function(x) {
    return(cart_predict_pruned(x, node=tree[[1]]$root, tree[[2]]))
  }

  params_list <- list()
  ret <- list()
  count <- 1

  for (lam in lambda_list) {
    pred <- matrix(0., nrow=reps, ncol=n)

    params_list[[count]] <- lam
    grid <- seq(0, 1, len=n)

    for (i in 1:reps) {
      x <- generate_sin_data(n, grid=grid, sigma=sigma)
      dimnames(x) <- list(NULL, c(1, "y"))
      tree <- cart_greedy_prune(x, depth=5, lambda=lam, quantile = TRUE)
      pred[i, ] <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree
      print(str_c("Finished rep ", i))
    }

    ret[[count]] <- apply(pred, MARGIN=2, function(x) c(mean(x), sd(x)))
    count <- count + 1
  }
  bv_data <- list(params_list, ret)
  save("bv_data", file=str_c("data/simul/","bv_pruning_", format(Sys.time(), "%Y%m%d-%H%M%S")))
}


#' bias variance data for 400 reps for Bagging with different numbers of Bootstrap samples
#' @param bs_list List of different integer bagging parameters B to be tested
#' @param sigma standard deviation of irreducible error in y
#' @param n number of generated data pairs
#' @param reps rows of data
#'
#' @description generate test date for different depths values of the CART algorithm with 400 reps and 150 data points
#'
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
      pred[i, ] <- bagging(bs, x_train=x, x_test=x, depth=5, regression=TRUE, use_parallel=FALSE)
    }

    ret[[count]] <- apply(pred, MARGIN=2, function(x) c(mean(x), sd(x)))
    count <- count + 1
  }
  bv_data <- list(params_list, ret)
  save("bv_data", file=str_c("data/simul/","bv_bagging_", format(Sys.time(), "%Y%m%d-%H%M%S")))
}



#' bias variance plot for CARTs for different depths
#'
#' @param data data to be plotted
#' @param plot_title title shown on the plot
#' @param bagging using bagging
#' @param pruning using pruning
#'
#' @examples load("data/simul/bv_greedy_20200810-101102"); bv_plot(bv_data);
#'
#' @import ggplot2
#' @import bbplot
bv_plot <- function(data, plot_title="Prediction CART Regression Tree", bagging=FALSE, pruning=FALSE) {

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

  if (bagging) {
    plt_data <- data[[4]]
    df1 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
    plt_data <- data[[3]]
    df2 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
    plt_data <- data[[2]]
    df3 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
    plt_data <- data[[1]]
    df4 <- data.frame(x=grid, mean=plt_data[1,], std=plt_data[2, ])
  }

  if (!bagging && !pruning) {
    gg <- ggplot(df1, aes(x=grid, y=mean)) +
      scale_colour_manual("",
                          breaks = c("depth 2", "depth 5", "depth 10", "depth 15"),
                          values = c("depth 2"="red", "depth 5"="grey", "depth 10"="blue",
                                     "depth 15"="green")) +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "green", data=df4, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "blue", data=df3, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "grey", data=df2, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "red", alpha=0.5, outline.type="both") +
      geom_line(aes(x=grid, y=sin(2*pi*grid))) +
      geom_line(aes(x=grid, y=mean, colour="depth 15"), data=df4, alpha=1) +
      geom_line(aes(x=grid, y=mean, colour="depth 10"), data=df3, alpha=1) +
      geom_line(aes(x=grid, y=mean, colour="depth 5"), data=df2, alpha=1) +
      geom_line(alpha=1, aes(colour="depth 2")) +
      ggtitle(plot_title) +
      ylab("") +
      xlab("") +
      bbc_style()
  }
  else if (bagging) {
    gg <- ggplot(df4, aes(x=grid, y=mean)) +
      scale_colour_manual("",
                          breaks = c("B = 1", "B = 5", "B = 25", "B = 100"),
                          values = c("B = 1"="grey", "B = 5"="blue", "B = 25"="green",
                                     "B = 100"="red")) +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "red", data=df1, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "green", data=df2, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "blue", data=df3, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "grey", alpha=0.5, outline.type="both") +
      geom_line(aes(x=grid, y=sin(2*pi*grid))) +
      geom_line(aes(x=grid, y=mean, colour="B = 100"), data=df1, alpha=1) +
      geom_line(aes(x=grid, y=mean, colour="B = 25"), data=df2, alpha=1) +
      geom_line(aes(x=grid, y=mean, colour="B = 5"), data=df3, alpha=1) +
      geom_line(alpha=1, aes(colour="B = 1")) +
      ggtitle(plot_title) +
      ylab("") +
      xlab("") +
      bbc_style()
  }#(0., 0.001, 0.01, 0.03)
  else if (pruning) {
    gg <- ggplot(df4, aes(x=grid, y=mean)) +
      scale_colour_manual("",
                          breaks = c("lam = 0.", "lam = 0.001", "lam = 0.01", "lam = 0.03"),
                          values = c("lam = 0."="grey", "lam = 0.001"="blue", "lam = 0.01"="green",
                                     "lam = 0.03"="red")) +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "red", data=df1, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "green", data=df2, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "blue", data=df3, alpha=0.5, outline.type="both") +
      geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "grey", alpha=0.5, outline.type="both") +
      geom_line(aes(x=grid, y=sin(2*pi*grid))) +
      geom_line(aes(x=grid, y=mean, colour="lam = 0.03"), data=df1, alpha=1) +
      geom_line(aes(x=grid, y=mean, colour="lam = 0.01"), data=df2, alpha=1) +
      geom_line(aes(x=grid, y=mean, colour="lam = 0.001"), data=df3, alpha=1) +
      geom_line(alpha=1, aes(colour="lam = 0.")) +
      ggtitle(plot_title) +
      ylab("") +
      xlab("") +
      bbc_style()
  }

  print(gg)

}

