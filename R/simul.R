library(obstgarten)
library(tidyverse)
library(patchwork)

# bias variance data for 400 reps for CARTs of different depths
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
      x <- generate_sin_data(n, grid=grid)
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

# generate test date for different depths values of the CART algorithm with 400 reps
# and 150 data points
# bv_greedy(list(5L, 10L, 15L), n=150, reps=400)

#bias variance plot for CARTs for different depths
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

  gg <- ggplot(df1, aes(x=grid, y=mean)) +
    scale_colour_manual("",
                        breaks = c("depth 5", "depth 10", "depth 15"),
                        values = c("depth 5"="green", "depth 10"="red",
                                   "depth 15"="blue")) +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "red", data=df3, alpha=0.75) +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "blue", data=df2, alpha=0.75) +
    geom_ribbon(aes(ymin = mean - std, ymax = mean + std), fill = "green", alpha=0.75) +
    geom_line(aes(x=grid, y=mean, colour="depth 15"), data=df3, alpha=1) +
    geom_line(aes(x=grid, y=mean, colour="depth 10"), data=df2, alpha=1) +
    geom_line(alpha=1, aes(colour="depth 5")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid))) +
    ggtitle("Prediction of different CART generated Trees") +
    ylab("y") +
    xlab("x")

  print(gg)

}

# load("data/simul/bv_greedy_20200809-132632")
# bv_plot(bv_data)
