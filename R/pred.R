# plots prediction of CART generated regression tree
#'
#' @param depth Integer depths of the CART generated regression tree
#' @example pred_plot_greedy(5, sigma=0.25, n=150)
#' @import ggplot2
#' @import bbplot
pred_plot_greedy <- function(depth, sigma=0.25, n=150, random_forest=FALSE, simul=FALSE) {
  if (random_forest == TRUE & depth <= 2) {
    stop("Random Forest require depth > 1!")
  }

  grid <- seq(0, 1, len=n)

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  x <- generate_sin_data(n, grid=grid, sigma=sigma)

  dimnames(x) <- list(NULL, c(1, "y"))
  tree <- cart_greedy(x, depth=depth, random=random_forest, m=1)
  pred <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  mse <- 1/n * sum((pred - sin(2*pi*grid)) ** 2)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2]), alpha=0.5) +
    annotate("text", x=0.9, y=1.1, label= str_c("MSE: ", round(mse, digits=5))) +
    ggtitle(str_c("Prediction of CART Regression Tree with Depth ", depth)) +
    xlab("") +
    ylab("") +
    bbc_style()

  if (simul) return(gg)
  else print(gg)
}


# plots prediction of CART generated decision tree
#'
#' @param depth Integer depths of the CART generated decision tree
#' @example pred_plot_greedy_class(5, sigma=0.25, n=150)
#' @import dplyr
#' @import ggplot2
pred_plot_greedy_class <- function(depth, sigma=0.25, n=150, random_forest=FALSE) {
  if (random_forest == TRUE & depth <= 2) {
    stop("Random Forest require depth > 1!")
  }

  grid <- seq(0, 1, len=n)

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  x <- generate_sin_data(n, sigma=sigma, reg = FALSE)

  tree <- cart_greedy(x, depth=depth, random=random_forest, m=1, mode = "classification")
  pred <- round(apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict)) # predicting with current tree

  print(x)

  df_plot <- rename(data.frame(x), x=x1, y=x2, z=y)

  grid <- seq(0, 1, len=n)

  gg <- ggplot(data=df_plot) +
    geom_point(aes(x=x, y=y, colour=pred)) +
    ggtitle(str_c("Prediction of CART Decision Tree with Depth ", depth)) +
    geom_line(aes(x=grid, y=(0.5*sin(2*pi*grid)) + 0.5)) +
    xlab("") +
    ylab("")

  print(gg)

}


# plots prediction of pruned CART generated regression tree
#'
#' @param depth Integer depths of the CART generated regression tree
#' @example pred_plot_greedy(5, sigma=0.25, n=150)
#' @import ggplot2
#' @import bbplot
pred_plot_pruning <- function(lambda, depth=5, sigma=0.25, n=150, random_forest=FALSE, simul=FALSE) {

  grid <- seq(0, 1, len=n)

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  x <- generate_sin_data(n, grid=grid, sigma=sigma)

  dimnames(x) <- list(NULL, c(1, "y"))
  tree <- cart_greedy_prune(x, lambda=lambda, depth=depth, random=random_forest, m=1)
  pred <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  mse <- 1/n * sum((pred - sin(2*pi*grid)) ** 2)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2]), alpha=0.5) +
    annotate("text", x=0.9, y=1.1, label= str_c("MSE: ", round(mse, digits=5))) +
    ggtitle(str_c("Prediction of Pruned CART Regression Tree (lambda= ", lambda, ")")) +
    xlab("") +
    ylab("") +
    bbc_style()

  if (simul) return(gg)
  else print(gg)
}


#' plots prediction of Bagging generated regression tree with depth 5
#' and specified number of bootstrap samples B
#'
#' @param B integer number of bootstrap samples
#' @param random_forest logical: TRUE: random forest, FALSE: bagging
#'
#' @example pred_plot_bagging(100, sigma=0.25, n=150)
#' @import ggplot2
#' @import bbplot
pred_plot_bagging <- function(depth, B, sigma=0.25, n=150, grid=NULL, random_forest=FALSE, simul=FALSE) {

  if (is.null(grid)) grid <- seq(0, 1, len=n)

  x <- generate_sin_data(n, grid=grid, sigma=sigma)
  x_test <- generate_sin_data(n, grid=grid, sigma=sigma)

  pred <- bagging(depth=depth, B=B, x_train=x, x_test=x_test, random_forest=random_forest) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  mse <- 1/n * sum((pred - sin(2*pi*grid)) ** 2)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_point(aes(x=grid, y=x_test[, 2]), alpha=0.25) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    ggtitle(str_c("1D Random Forest Regression")) +
    annotate("text", x=0.9, y=1.1, label= str_c("MSE: ", round(mse, digits=5))) +
    xlab("") +
    ylab("") +
    bbc_style()

  if (simul) return(gg)
  else print(gg)

}


# plots prediction of Bagging generated decision tree
#'
#' @param depth Integer depths of the bagging generated decision tree
#' @example pred_plot_bagging_class(B=10L, depth=5, sigma=0.25, n=150)
#' @import ggplot2
#' @import bbplot
pred_plot_bagging_class <- function(B, depth, sigma=0.25, n=150, random_forest=FALSE) {

  grid <- seq(0, 1, len=n)

  x <- generate_sin_data(n, sigma=sigma, reg = FALSE)
  x_test <- generate_sin_data(n, sigma=sigma, reg = FALSE)

  pred <- bagging(depth=depth, B=B, x_train=x, x_test=x_test, regression=FALSE, random_forest=random_forest) # predicting with current tree

  acc <- sum(x_test[, ncol(x_test)] == pred)/nrow(x)

  df_plot <- rename(data.frame(x_test), x=x1, y=x2, z=y)

  gg <- ggplot(data=df_plot) +
    geom_point(aes(x=x, y=y, colour=pred)) +
    ggtitle(str_c("2D Random Forest Classification")) +
    geom_line(aes(x=grid, y=(0.5*sin(2*pi*grid)) + 0.5)) +
    annotate("text", x=0.9, y=1.05, label= str_c("Accuracy: ", round(acc, digits=5))) +
    xlab("") +
    ylab("") +
    bbc_style() +
    theme(legend.position="none")

  print(gg)

}

# pred_plot_bagging_class(B=10L, depth = 50, n=100)

#' Method to Plot Predicted Density of a Random Forest
#' for a two dimensional case for a sine generated dataset
#' (sin(sqrt(x^2+y^2)))/(sqrt(x^2+y^2))
#' @example pred_plot_sine2D(n=1000, B=10L, depth=5, sd=0.1, k=10)
#' @import ggplot2
pred_plot_sine2D <- function(n, B, depth, sd, k=10, random_forest=TRUE) {
  data <- generate_sin_2D(n=n, sigma=sd, k=k)

  l <- round(sqrt(n))

  #creating test data
  coords <- matrix(c(rep(seq(-k,k,len=l), each=l), rep(seq(-k, k, len=l), times=l)), ncol=2)
  test_data <- data.frame(x1=coords[, 1], x2=coords[, 2], y=(sin(sqrt(coords[, 1]**2+coords[, 2]**2))/(sqrt(coords[, 1]**2+coords[, 2]**2))))

  #predicting
  pred <- bagging(depth=depth, B=B, x_train=data, x_test=test_data, random_forest = random_forest) # predicting with current tree
  mse <- 1/n * sum((pred - test_data[, ncol(test_data)]) ** 2)

  plot_df <- data.frame(x=test_data[, 1], y=test_data[, 2], pred=pred, true=test_data[, ncol(test_data)])

  gg <- ggplot(plot_df, aes(x=x, y=y, z = pred)) +
    geom_contour_filled() +
    scale_color_gradient(low='white', high='red') +
    geom_contour(aes(x=x, y=y, z = true, colour=after_stat(level)), bins=15, size = 0.5) +
    ggtitle("2D Random Forest Regression") +
    annotate("text", x=k-3, y=k+0.5, label= str_c("Total MSE: ", round(mse, digits=5))) +
    # geom_point() +
    xlab("") +
    ylab("")

  # gg <- ggplot(plot_df, aes(x=x, y=y, z = pred)) +
  #     geom_contour_filled() +
  #     xlab("") +
  #     ylab("")
  # gg2 <- ggplot(plot_df, aes(x=x, y=y, z = true)) +
  #   geom_contour_filled() +
  #   xlab("") +
  #   ylab("")

  # figure <- ggarrange(gg, gg2,
  #                     labels = c("Predicted", "True"),
  #                     ncol = 2, nrow = 1)
  #
  # figure

  print(gg)

}

# pred_plot_sine2D(n=1000, B=5L, depth=5, sd=0.1, k=10)


#' Method plotting one dimension of a multidimensional regression
#' problem of a multicariate gaussian using a random forest.
#' @import ggplot2
#' @import bbplot
pred_plot_rf <- function(n, d, sd, B, depth, m, display_d=1L, simul=FALSE) {
  data <- generate_mult_data(n=n, d=d, sigma=diag(d), mu=rep(0., d))
  x <- data[[1]]
  sigma <- data[[3]]
  mu <- data[[2]]

  # data for true gaussian function
  grid <- seq(mu[display_d] - 2*sigma[display_d, display_d], mu[display_d] + 2*sigma[display_d, display_d], len=n)
  gridmat <- matrix(0., nrow=n, ncol=d)
  gridmat[, display_d] <- grid
  true_val <- apply(gridmat, MARGIN=1, mvtnorm::dmvnorm, mu, sigma)

  # data for testing prediction
  x_test <- x
  x_test[, 1:d] <- gridmat
  x_test[, ncol(x_test)] <- true_val

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  pred <- bagging(
    B=B, x_train=x, x_test=x_test, depth=depth, m=m,
    regression=TRUE, use_parallel=FALSE, random_forest = TRUE
  )

  mse <- 1/n * sum((pred - true_val) ** 2)

  df_plot <- data.frame(grid=grid, y=pred)

  gg <- ggplot(data=df_plot) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_point(aes(x=x[, display_d], y=x[, ncol(x)]), alpha=0.25) +
    geom_line(aes(x=grid, y=pred, colour="Prediction")) +
    geom_line(aes(x=grid, y=true_val, colour="True")) +
    xlab(str_c("Dimension ", display_d)) +
    xlim(-2*sigma[display_d, display_d], +2*sigma[display_d, display_d]) +
    ggtitle("Random Forest Regression") +
    annotate("text", x=1, y=max(true_val) + 0.025, label= str_c("MSE: ", round(mse, digits=5))) +
    ylab("") +
    bbc_style()

  if (simul) return(gg)
  else print(gg)

}


#' Method to Plot the classifcation of iris test dataset
#' @import dplyr
#' @import ggplot2
#' @import bbplot
pred_plot_iris_class <- function(depth, B=100L) {

  data <- prepare_iris()
  xy_train <- data[[1]]
  xy_test <- data[[2]]

  # predicting with Random Forest
  pred <- bagging(B=B, depth=depth, x_train=xy_train, x_test=xy_test, regression=FALSE, use_parallel=FALSE,
                  random_forest = TRUE)

  acc <- sum(xy_test[, ncol(xy_test)] == pred)/nrow(xy_test)

  df_plot <- rename(data.frame(xy_test), x=Sepal.Length, y=Sepal.Width, z=y)
  df_plot$z <- as.double(pred)
  xy_train$y <- as.double(xy_train$y)
  df_plot2 <- rename(data.frame(xy_train), x=Sepal.Length, y=Sepal.Width, z=y)

  gg <- ggplot(data=df_plot) +
    geom_point(data=df_plot2, aes(x=x, y=y), alpha=0.1) +
    geom_point(aes(x=x, y=y, colour=pred)) +
    ggtitle(str_c("Iris: Random Forest Classification")) +
    annotate("text", x=max(df_plot[, 1]), y=4.1, label= str_c("Accuracy: ", round(acc, digits=5))) +
    xlab("") +
    ylab("") +
    bbc_style() +
    theme(legend.position="none")

  print(gg)

}

# pred_plot_iris_class(depth=2, B=1L)
