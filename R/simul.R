#library(obstgarten)
library(tidyverse)
library(plot3D)
library(rayshader)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

# plots prediction of CART generated regression tree
#'
#' @param depth Integer depths of the CART generated regression tree
#' @example pred_plot_greedy(5, sigma=0.25, n=150)
pred_plot_greedy <- function(depth, sigma=0.25, n=150, random_forest=FALSE) {
  if (random == TRUE & depth <= 2) {
    stop("Random Forest require depth > 1!")
  }

  grid <- seq(0, 1, len=n)

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  x <- generate_sin_data(n, grid=grid, sigma=sigma)
  #TODO data cannot be used for random_forest

  dimnames(x) <- list(NULL, c(1, "y"))
  tree <- cart_greedy(x, depth=depth, random=random_forest)
  pred <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree

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

# pred_plot_greedy(depth=3, random=TRUE)


#' plots prediction of Bagging generated regression tree with depth 5
#' and specified number of bootstrap samples B
#'
#' @param B integer number of bootstrap samples
#' @param random_forest logical: TRUE: random forest, FALSE: bagging
#'
#' @example pred_plot_bagging(100, sigma=0.25, n=150)
pred_plot_bagging <- function(B, sigma=0.25, n=150, random_forest = FALSE) {

  grid <- seq(0, 1, len=n)

  x <- generate_sin_data(n, grid=grid, sigma=sigma)
  #TODO data cannot be used for random_forest

  pred <- bagging(B=B, x_train=x, x_test=x, random_forest = random_forest) # predicting with current tree

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
      pred[i, ] <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree
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

# load("data/simul/bv_bagging_20200810-115904")
# bv_plot(bv_data)

#' Method to quantitavely compare Prediction
#' Quality of the four different methods for
#' high dimensional data.
#' @example compare_methods_PE(d=10, n=1000, B=100L, reps=400) CAUTION!!
#' TAKES VERY LONG CPU EXPENSIVE
compare_methods_PE <- function(d, n, B=100L, reps=400) {
  pe_mat <- matrix(0., nrow=reps, ncol=4)

  for (i in 1:reps) {

    training_data <- generate_mult_data(n=n, d=d)
    xy <- as.matrix(training_data[[1]])
    mu <- training_data[[2]]
    sigma <- training_data[[3]]

    testing_data <- generate_mult_data(n=n, d=d, mu=mu, sigma=sigma)
    xy_test <- as.matrix(testing_data[[1]])

    # predicting with CART
    tree <- cart_greedy(xy, depth=5, random=FALSE)
    pred <- apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
                   function(x) cart_predict(x, node=tree$root))
    # calculating prediction error
    print(sum((pred - xy_test[, ncol(xy_test)])**2))
    pe_mat[i, 1] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2)

    # predicting with CART and pruning
    # yet to implement
    pe_mat[i, 2] <- 0.

    # predicting with Bagging alg
    pred <- bagging(B=B, x_train=xy, x_test=xy_test, regression=TRUE, use_parallel=FALSE)
    pe_mat[i, 3] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2) # calculating prediction error

    # predicting with Random Forest
    pred <- bagging(B=B, x_train=xy, x_test=xy_test, random_forest=TRUE, regression=TRUE, use_parallel=FALSE)
    pe_mat[i, 4] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2) # calculating prediction error

    print(str_c("Finished ", i, "th repetition!"))
  }

  ret <- list(apply(pe_mat, MARGIN=2, mean), pe_mat)
  save("ret", file=str_c("data/simul/","pe_compare_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}


#' Method to quantitavely compare Prediction
#' Quality of the Random Forests for
#' different parameters m.
#' @example compare_m_PE(list(1L, 3L, 5L, 10L), d=20, n=1000, B=100L, reps=1)
#' CAUTION CPU EXPENSIVE TAKES LONG
compare_m_PE <- function(m_list, d, n, B, reps=400) {
  for (m in m_list) {
    if (m >= d) {
      stop("For all elements m of m_list it must hold that m < d!")
    }
  }
  params_list <- list()
  count <- 1

  pe_mat <- matrix(0., nrow=reps, ncol=length(m_list))

  for (m in m_list) {

    params_list[[count]] <- m

    for (i in 1:reps) {

      training_data <- generate_mult_data(n=n, d=d)
      xy <- as.matrix(training_data[[1]])
      mu <- training_data[[2]]
      sigma <- training_data[[3]]

      testing_data <- generate_mult_data(n=n, d=d, mu=mu, sigma=sigma)
      xy_test <- as.matrix(testing_data[[1]])

      # predicting with Random Forest
      pred <- bagging(B=B, x_train=xy, x_test=xy_test, m=m, random_forest=TRUE, regression=TRUE, use_parallel=FALSE)
      # calculating prediction error
      pe_mat[i, count] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2)

      print(str_c("Finished ", i, "th repetition!"))
    }

    count <- count + 1

  }

  ret <- list(apply(pe_mat, MARGIN=2, mean), pe_mat, params_list)
  save("ret", file=str_c("data/simul/","pe_RF_m_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}


#' Method to qualitatively compare Prediction
#' Quality of the Random Forests for
#' different parameters m.
#' @example compare_m(m_list = list(1L, 3L, 5L, 10L) ,d=20, n=100, B=100L)
compare_m <- function(m_list, d, n, B) {
  for (m in m_list) {
    if (m >= d) {
      stop("For all elements m of m_list it must hold that m < d!")
    }
  }

  training_data <- generate_mult_data(n=n, d=d)
  xy <- as.matrix(training_data[[1]])
  mu <- training_data[[2]]
  sigma <- training_data[[3]]

  testing_data <- generate_mult_data(n=n, d=d, mu=mu, sigma=sigma)
  xy_test <- as.matrix(testing_data[[1]])

  ret <- data.frame(xy_test)
  pred_mat <- matrix(0., nrow=n, ncol=length(m_list))

  count <- 1

  for (m in m_list) {

    # predicting with Random Forest
    pred <- bagging(B=B, x_train=xy, x_test=xy_test, m=m, random_forest=TRUE, regression=TRUE, use_parallel=FALSE)
    pred_mat[, count] <- pred
    count <- count + 1
  }

  ret[(ncol(ret)+1):(ncol(ret)+length(m_list))] <- pred_mat

  names(ret)[(length(names(ret))-3):length(names(ret))] <- str_c("m", 1:4)
  save("ret", file=str_c("data/simul/","compare_RF_m_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}


#' Method to qualitatively compare Prediction
#' Quality of the four different methods for
#' high dimensional data.
#' @example compare_methods(d=3, n=100, B=10L)
compare_methods <- function(d, n, B=100L) {

  training_data <- generate_mult_data(n=n, d=d)
  xy <- as.matrix(training_data[[1]])
  mu <- training_data[[2]]
  sigma <- training_data[[3]]

  testing_data <- generate_mult_data(n=n, d=d, mu=mu, sigma=sigma)
  xy_test <- as.matrix(testing_data[[1]])

  ret <- data.frame(xy_test)

  # predicting with CART
  tree <- cart_greedy(xy, depth=5, random=FALSE)
  pred <- apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
                function(x) cart_predict(x, node=tree$root))

  ret$CART <- pred

  # predicting with CART and pruning
  # yet to implement
  pred_pruning <- 0.
  ret$pruning <- pred_pruning

  # predicting with Bagging alg
  pred <- bagging(B=B, x_train=xy, x_test=xy_test, regression=TRUE, use_parallel=FALSE)
  ret$bagging <- pred

  # predicting with Random Forest
  pred <- bagging(B=B, x_train=xy, x_test=xy_test, random_forest=TRUE, regression=TRUE, use_parallel=FALSE)
  ret$rf <- pred

  save("ret", file=str_c("data/simul/","compare_plot_data_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}



#' CPU Heavy!!!
#' Method to 3D render Gaussian Multivariates estimates coming from
#' generated samples or predicted values
#' @param df data.frame with columns x, y and z. samples are in the rows
plot_3D <- function(df) {
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))
  gg <- ggplot(df, aes(x=x, y=y, z=z)) +
    geom_point(aes(x=x, y=y, color=z),size=1) +
    sc

  plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
          zoom = 0.55, phi = 30)
  render_snapshot()
}


#' Method to plot 3D comparisons of predictions
#' for different methods
#' @param path character specifying path to compare_plot_data file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#' if FALSE only 2D plots are returned
#'
#' @example plot_3D_compare("data/simul/compare_plot_data_20200812-113504", render=TRUE)
plot_3D_compare <- function(path, render=FALSE) {
  stopifnot("Path should be a character specifying path to compare_plot_data file!" =
              is.character(path))
  load(path)

  ret %>%
    rename("True"=y, "CART"=CART, "Pruning"=pruning, "Bagging"=bagging, "Random Forest" = rf) %>%
    pivot_longer(cols = c(True, CART, Pruning, Bagging, "Random Forest"), names_to = c("pred")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~pred)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}


#' Method to plot 3D comparisons of prediction DIFFERENCES
#' between ground truth and model for different methods
#' @param path character specifying path to compare_RF_m file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#'  if FALSE only 2D plots are returned
#'
#' @example plot_3D_compare_DIFF("data/simul/compare_plot_data_20200812-113504", render=FALSE)
plot_3D_compare_DIFF <- function(path, render=FALSE) {
  stopifnot("Path should be a character specifying path to compare_RF_m file!" =
              is.character(path))
  load(path)

  ret %>%
    rename("True"=y, "CART"=CART, "Pruning"=pruning, "Bagging"=bagging, "RandomForest" = rf) %>%
    mutate("Squared Diff. CART"=(True - CART)**2, "Squared Diff. Pruning"=(True - Pruning)**2,
           "Squared Diff. Bagging"=(True - Bagging)**2, "Squared Diff. Random Forest"=(True - RandomForest)**2) %>%
    select(-True, -CART, -Pruning, -Bagging, -RandomForest) %>%
    pivot_longer(cols = c("Squared Diff. CART", "Squared Diff. Pruning", "Squared Diff. Bagging"
                          , "Squared Diff. Random Forest"), names_to = c("diff")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~diff)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}





#' Method to plot 3D comparisons of prediction SQUARED DIFFERENCES
#' between ground truth and Random Forest Predictions for
#' different m
#' @param path character specifying path to compare_RF_m file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#' if FALSE only 2D plots are returned
#'
#' @example plot_3D_compare_m("data/simul/compare_RF_m_20200812-175425")
plot_3D_compare_m <- function(path, render=FALSE) {
  stopifnot("Path should be a character specifying path to compare_RF_m file!" =
              is.character(path))
  load(path)

  ret %>%
    rename("True"=y, "m=1"=m1, "m=3"=m2, "m=5"=m3, "m=10" = m4) %>%
    pivot_longer(cols = c("True", "m=1", "m=3", "m=5", "m=10"), names_to = c("pred")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~pred)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}


#' Method to plot 3D comparisons of prediction DIFFERENCES
#' between ground truth and Random Forest for different m
#' @param path character specifying path to compare_RF_m file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#'  if FALSE only 2D plots are returned
#'
#' @example plot_3D_compare_m_DIFF("data/simul/compare_RF_m_...", render=FALSE)
plot_3D_compare_m_DIFF <- function(path, render=FALSE) {
  stopifnot("Path should be a character specifying path to compare_RF_m file!" =
              is.character(path))
  load(path)

  ret %>%
    rename("True"=y, "m1"=m1, "m3"=m2, "m5"=m3, "m10" = m4) %>%
    mutate("Squared Diff. m=1"=(True - m1)**2, "Squared Diff. m=3"=(True - m3)**2,
           "Squared Diff. m=5"=(True - m5)**2, "Squared Diff. m=10"=(True - m10)**2) %>%
    select(-True, -m1, -m3, -m5, -m10) %>%
    pivot_longer(cols = c("Squared Diff. m=1", "Squared Diff. m=3", "Squared Diff. m=5"
                          , "Squared Diff. m=10"), names_to = c("diff")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~diff)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}

