#' @import grid
#' @import gridExtra
#' @import ggplot2
simul_plot_greedy <- function() {
  depth_list <- list(2L, 5L, 10L, 15L)
  plot_list <- list()
  count <- 1
  for (depth in depth_list) {
    plot_list[[count]] <- pred_plot_greedy(n = 100, depth=depth, simul=TRUE)
    count <- count + 1
  }
  grid.arrange(plot_list[[1]] + ggtitle("depth = 2") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[2]] + ggtitle("depth = 5") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[3]] + ggtitle("depth = 10") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[4]]  + ggtitle("depth = 15") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               nrow = 2,
               top = textGrob("Regression Tree Predictors",
                              gp=gpar(fontsize=14))
  )
}


#' @import grid
#' @import gridExtra
#' @import ggplot2
simul_plot_pruning <- function() {
  lambda_list <- list(0., 0.001, 0.01, 0.03)
  plot_list <- list()
  count <- 1
  for (lambda in lambda_list) {
    plot_list[[count]] <- pred_plot_pruning(n = 100, depth=5, simul=TRUE, lambda = lambda)
    count <- count + 1
  }
  grid.arrange(plot_list[[1]] + ggtitle(paste("lambda =", lambda_list[[1]])) + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[2]] + ggtitle(paste("lambda =", lambda_list[[2]])) + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[3]] + ggtitle(paste("lambda =", lambda_list[[3]])) + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[4]]  + ggtitle(paste("lambda =", lambda_list[[4]])) + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               nrow = 2,
               top = textGrob("Pruned Regression Tree Predictors",
                              gp=gpar(fontsize=14))
  )
}


#' @import grid
#' @import gridExtra
#' @import ggplot2
simul_plot_bagging <- function() {
  B_list <- list(1L, 5L, 25L, 100L)
  plot_list <- list()
  count <- 1
  for (bs in B_list) {
    plot_list[[count]] <- pred_plot_bagging(depth=5, n = 100, B=bs, simul=TRUE)
    count <- count + 1
  }
  grid.arrange(plot_list[[1]] + ggtitle("No. BS = 1") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[2]] + ggtitle("No. BS = 5") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[3]] + ggtitle("No. BS = 25") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               plot_list[[4]]  + ggtitle("No. BS = 100") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
               nrow = 2,
               top = textGrob("Bagging Predictors",
                              gp=gpar(fontsize=14))
  )
}

# simul_plot_rf <- function() {
#   B_list <- list(1L, 2L, 3L)
#   plot_list <- list()
#   count <- 1
#   for (bs in B_list) {
#     plot_list[[count]] <- pred_plot_rf(n=500, d=4, m=bs, B=10L, depth=5, display_d=1, sd=0.1, simul = TRUE)
#
#     count <- count + 1
#   }
#   grid.arrange(plot_list[[1]] + ggtitle("m = 1") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
#                plot_list[[2]] + ggtitle("m = 2") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
#                plot_list[[3]] + ggtitle("m = 3") + theme(plot.title = element_text(size=10)) + theme(legend.position="none"),
#                nrow = 2,
#                top = textGrob("Random Forest Predictors",
#                               gp=gpar(fontsize=14))
#   )
# }


#' Method to test performance of quantiles
#' compare_performance(n=1000, B=10L, depth=5, sd=0.1, k=10, random_forest=TRUE, reps=100)
#' @import stringr
compare_performance <- function(n, B, depth, sd, k=10, random_forest=TRUE, reps=100) {
  pe_mat <- matrix(0., nrow=reps, ncol=4)

  for (i in 1:reps) {

    data <- generate_sin_2D(n=n, sigma=sd, k=k)

    l <- round(sqrt(n))

    #creating test data

    coords <- matrix(c(rep(seq(-k,k,len=l), each=l), rep(seq(-k, k, len=l), times=l)), ncol=2)
    test_data <- data.frame(x1=coords[, 1], x2=coords[, 2], y=(sin(sqrt(coords[, 1]**2+coords[, 2]**2))/(sqrt(coords[, 1]**2+coords[, 2]**2))))

    #predicting without quantiles
    start_time <- Sys.time()
    pred <- bagging(depth=depth, B=B, x_train=data, x_test=test_data, random_forest = random_forest) # predicting with current tree
    pe_mat[i, 1] <- 1/n * sum((pred - test_data[, ncol(test_data)])**2)
    end_time <- Sys.time()
    pe_mat[i, 2] <- end_time - start_time

    #predicting with quantiles
    start_time <- Sys.time()
    pred <- bagging(depth=depth, B=B, x_train=data, x_test=test_data, random_forest = random_forest, quantile = TRUE) # predicting with current tree
    pe_mat[i, 3] <- 1/n * sum((pred - test_data[, ncol(test_data)])**2)
    end_time <- Sys.time()
    pe_mat[i, 4] <- end_time - start_time

    print(str_c("Finished ", i, "th repetition!"))

  }

  ret <- list(apply(pe_mat, MARGIN=2, mean), pe_mat)
  save("ret", file=str_c("data/simul/","performance_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}

# pred_plot_sine2D(n=1000, B=5L, depth=5, sd=0.1, k=10)


#' Method to quantitavely compare Prediction
#' Quality of the four different methods for
#' high dimensional data.
#' compare_methods_PE(d=4, n=1000L, B=25L, reps=100) CAUTION!!
#' TAKES VERY LONG CPU EXPENSIVE
#' @import stringr
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
    tree <- cart_greedy(xy, depth=5, random=FALSE, quantile = TRUE)
    pred <- apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
                   function(x) cart_predict(x, node=tree$root))
    # calculating prediction error
    pe_mat[i, 1] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2)

    # predicting with CART and pruning
    # yet to implement
    # tree <- cart_greedy_prune(xy, depth=5, random=FALSE, quantile = TRUE)
    # pred <- apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
    #               function(x) cart_predict(x, node=tree$root))
    # pe_mat[i, 2] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2)
    pe_mat[i, 2] <- 0.

    # predicting with Bagging alg
    pred <- bagging(B=B, x_train=xy, x_test=xy_test, regression=TRUE, use_parallel=FALSE, quantile = TRUE)
    pe_mat[i, 3] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2) # calculating prediction error

    # predicting with Random Forest
    pred <- bagging(B=B, x_train=xy, x_test=xy_test, random_forest=TRUE, regression=TRUE, use_parallel=FALSE, quantile = TRUE)
    pe_mat[i, 4] <- 1/n * sum((pred - xy_test[, ncol(xy_test)])**2) # calculating prediction error

    print(str_c("Finished ", i, "th repetition!"))
  }

  ret <- list(apply(pe_mat, MARGIN=2, mean), pe_mat)
  save("ret", file=str_c("data/simul/","pe_compare_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}


#' Method to quantitavely compare Prediction
#' Quality of the Random Forests for
#' different parameters m.
#' compare_m_PE(list(1L, 3L, 5L, 10L), d=20, n=1000, B=100L, reps=1)
#' CAUTION CPU EXPENSIVE TAKES LONG
#' @import stringr
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
#' compare_m(m_list = list(1L, 3L, 5L, 10L) ,d=20, n=100, B=100L)
#' @import stringr
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

  names(ret)[(length(names(ret))-3):length(names(ret))] <- str_c("m", 1:length(m_list))
  save("ret", file=str_c("data/simul/","compare_RF_m_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}


#' Method to qualitatively compare Prediction
#' Quality of the four different methods for
#' high dimensional data.
#' compare_methods(d=3, n=1000, B=100L)
#' @import stringr
compare_methods <- function(d, n, B=100L) {

  training_data <- generate_mult_data(n=n, d=d)
  xy <- as.matrix(training_data[[1]])
  mu <- training_data[[2]]
  sigma <- training_data[[3]]

  testing_data <- generate_mult_data(n=n, d=d, mu=mu, sigma=sigma)
  xy_test <- as.matrix(testing_data[[1]])

  ret <- data.frame(xy_test)

  # predicting with CART
  tree <- cart_greedy(xy, depth=5, random=FALSE, quantile=TRUE)
  pred <- apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
                function(x) cart_predict(x, node=tree$root))

  ret$CART <- pred

  # predicting with CART and pruning
  # yet to implement
  # tree <- cart_greedy_prune(xy, depth=5, random=FALSE)
  # pred <- apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
  #              function(x) cart_predict(x, node=tree$root))
  pred <- 0.
  ret$pruning <- pred

  # predicting with Bagging alg
  pred <- bagging(B=B, x_train=xy, x_test=xy_test, regression=TRUE, use_parallel=FALSE, quantile = TRUE)
  ret$bagging <- pred

  # predicting with Random Forest
  pred <- bagging(B=B, x_train=xy, x_test=xy_test, random_forest=TRUE, regression=TRUE, use_parallel=FALSE, quantile=TRUE)
  ret$rf <- pred

  save("ret", file=str_c("data/simul/","compare_methods_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}


#' Visualizing the Distribution of the Iris Dataset Features
#' Adapted from Antonio Lopez
#' @import ggplot2
#' @import grid
#' @import gridExtra
visualize_iris_feature_distr <- function() {
  iris <- load_iris()

  petallength <-  ggplot(iris, aes(x=Petal.Length, colour=Species, fill=Species)) +
    geom_density(alpha=.3) +
    geom_vline(aes(xintercept=mean(Petal.Length),  colour=Species),linetype="dashed",color="grey", size=1)+
    xlab("Petal Length (cm)") +
    ylab("Density") +
    theme(legend.position="none")

  sepallength <- ggplot(iris, aes(x=Sepal.Length, colour=Species, fill=Species)) +
    geom_density(alpha=.3) +
    geom_vline(aes(xintercept=mean(Sepal.Length),  colour=Species),linetype="dashed", color="grey", size=1)+
    xlab("Sepal Length (cm)") +
    ylab("Density") +
    theme(legend.position="none")

  petalwidth <- ggplot(iris, aes(x=Petal.Width, colour=Species, fill=Species)) +
    geom_density(alpha=.3) +
    geom_vline(aes(xintercept=mean(Petal.Width),  colour=Species),linetype="dashed",color="grey", size=1)+
    xlab("Petal Width (cm)") +
    ylab("Density")

  sepalwidth <- ggplot(iris, aes(x=Sepal.Width, colour=Species, fill=Species)) +
    geom_density(alpha=.3) +
    geom_vline(aes(xintercept=mean(Sepal.Width),  colour=Species), linetype="dashed",color="grey", size=1)+
    xlab("Sepal Width (cm)") +
    ylab("Density") +
    theme(legend.position="none")

  grid.arrange(sepallength + ggtitle(""),
               sepalwidth + ggtitle(""),
               petallength + ggtitle(""),
               petalwidth  + ggtitle(""),
               nrow = 2,
               top = textGrob("Iris Dataset Feature Densities",
                              gp=gpar(fontsize=14))
  )

}


#' Function that performs classification with all 4 methods on
#' @import stringr
compare_classify_iris <- function(depth=5L, B=10L, h=FALSE) {
  data <- prepare_iris()
  xy_train <- data[[1]]
  xy_test <- data[[2]]

  ret <- data.frame(xy_test)

  # predicting with CART
  tree <- cart_greedy(xy_train, depth=depth, random=FALSE, mode = "classification", quantile = FALSE)
  pred <- round(apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
                function(x) cart_predict(x, node=tree$root)))

  ret$CART <- pred

  # predicting with CART and pruning
  # tree <- cart_greedy_prune(xy_train, depth=depth, random=FALSE, mode = "classification", quantile = TRUE)
  # pred <- round(apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
  #                     function(x) cart_predict(x, node=tree$root)))
  pred <- 0.
  ret$pruning <- pred

  # predicting with Bagging alg
  pred <- bagging(B=B, x_train=xy_train, x_test=xy_test, regression=FALSE, use_parallel=FALSE,
                  random_forest = FALSE, quantile = FALSE)
  ret$bagging <- pred

  # predicting with Random Forest
  pred <- bagging(B=B, x_train=xy_train, x_test=xy_test, regression=FALSE, use_parallel=FALSE,
                  random_forest = TRUE, quantile = FALSE)
  ret$rf <- pred

  if (h) return(ret)
  else save("ret", file=str_c("data/simul/","compare_iris_", format(Sys.time(), "%Y%m%d-%H%M%S")))

}

#' Calculate Accuracy
#'
#' Method that calculated the accuracy the different methods achieved in the iris classification
#' dataset
calc_accuracy <- function(ret) {

  dat <- ret [, 5:ncol(ret)]
  acc <- rep(0., times=4)

  for (i in 1:4) {
    acc[[i]] <- sum(dat[, 1] == dat[, 1+i])/nrow(dat)
  }

  return(acc)
}

#' Calculate Mean Accuracy of Iris Dataset
calc_mean_accuracy <- function(reps=100) {
  mat <- matrix(0., nrow=reps, ncol=4)
  for (i in 1:reps) {
    mat[i, ] <- calc_accuracy(compare_classify_iris(depth=5L, B=3L, h=TRUE))
  }
  ret <- apply(mat, MARGIN=2, mean)
  print(ret)
  save("ret", file=str_c("data/simul/","iris_mean_", format(Sys.time(), "%Y%m%d-%H%M%S")))
}

# compare_methods(d=5, n=100, B=10L)

# compare_classify_iris(B=10L)
# load("data/simul/compare_iris_20200813-194215")

# data <- prepare_iris()
# xy_train <- data[[1]]
# xy_test <- data[[2]]
#
# tree <- cart_greedy(xy_train, depth=5, random=FALSE, mode = "classification")
# pred <- round(apply(xy_test[, -ncol(xy_test), drop=FALSE], MARGIN=1,
#               function(x) cart_predict(x, node=tree$root)))


