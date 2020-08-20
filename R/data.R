#' Generate data set like in Example 6.3 of Richter19.pdf
#'
#' @param n number of generated data pairs
#' @param sigma standard deviation of irreducible error in y
#' @param reg logical TRUE for regression data FALSE for classification data
#' @param grid specify data points on x axis only works for reg == TRUE
#' @return if reg TRUE: n x 2 matrix (x_i, y_i) of generated data set
#' @return if reg FALSE: n x 3 matrix (x_1i, x_2i, y_i) of generated data set
#'
#' @export
generate_sin_data <- function(n, sigma=0.2, reg=TRUE, grid=NULL) {
  if (sigma <= 0.) {
    stop("Error: sigma has to be > 0")
  }
  if (!is.null(grid) & n != length(grid)) {
    stop("Error: The grid needs to be of the same length as n")
  }

  eps <- rnorm(n, mean=0, sd=sigma)

  if (reg) {
    if (!is.null(grid)) x <- grid
    else x <- runif(n, min=0, max=1)
    y <- sin(2 * pi * x) + eps
    ret <- matrix(c(x, y), nrow=n, ncol=2)
    colnames(ret) <- c("x", "y")
    ret <- as.data.frame(ret)
    return(data.matrix(ret[order(ret[, 1]), ]))
  }
  else {
    x1 <- runif(n, min=0, max=1)
    x2 <- runif(n, min=0, max=1)
    k <- x2 - 0.5 - 0.3 * sin(2 * pi * x1)
    y <- rep(1, times=n)
    y[(k - eps) > 0.] <- 2
    ret <- matrix(c(x1, x2, y), nrow=n, ncol=3)
    colnames(ret) <- c("x1", "x2", "y")
    ret <- as.data.frame(ret)
    return(data.matrix(ret[order(ret[, 1]), ]))
  }
}

#' Generate 2D sine data set
#'
#' @param n number of generated data pairs
#' @param sigma standard deviation of irreducible error in y
#' @param k specifying field of view
#'
#' @description According to model y = (sin(sqrt(x^2+y^2)))/(sqrt(x^2+y^2)) + N(0, sigma)
#' @export
generate_sin_2D <- function(n, sigma=0.2, k=10) {

  eps <- rnorm(n, mean=0, sd=sigma)

  x <- runif(n, min=-k, max=k)
  y <- runif(n, min=-k, max=k)
  z <- (sin(sqrt(x**2+y**2)))/(sqrt(x**2+y**2)) + eps

  ret <- data.frame(x1=x, x2=y, y=z)

  return(ret)

}


#' Generate Data from a multivariate Gaussian
#' @param n dimensional vector with feature means
#' @param d dimension
#' @param sd standard deviation of irreducible error in y
#' @param sigma positive definite square covariance matrix
#' @param mu mean for the multivariate normal distribution
#'
#' @return list(df of data, feature means vector, covariance matrix)
#'
#' @examples generate_mult_data(n=1000, d=5)
#' @import mvtnorm
#' @export
generate_mult_data <- function(n, d, sd=0.01, mu=NULL, sigma=NULL) {
  if (is.null(sigma)) {
    A <- matrix(runif(d*d, min = 0, max = 1), nrow=d, ncol=d)
    sigma <- t(A) %*% A
    }
  if (is.null(mu)) {
    mu <- runif(d, min = -1, max = +1)
  }

  data <- rmvnorm(n, mean=mu, sigma=sigma)

  # print(data)

  eps <- rnorm(n, mean=0, sd=sd)

  y <- apply(data, dmvnorm, mu, sigma, MARGIN = 1) + eps

  data <- data.frame(x=data, y=y)

  return(list(data, mu, sigma))
}

# data <- generate_mult_data(n=1000, d=2, mu=NULL, sigma=NULL)
# print(data)
# gg <- ggplot(data[[1]], aes(x=x.1, y=y)) +
#   geom_point()
# print(gg)


#' Iris data set
#'
#' @description Loads and returns Iris data set for classification testing
#'
#' @return 150 x 5 data.frame
#' @export
load_iris <- function() {
  data("iris")
  return(iris)
}

#' prepare_iris
#'
#' @description Method that prepares Iris dataset for classification with Tree-Methods.
#' setosa == 1, versicolor == 2, virginica == 3
#'
#' @param training_set_ratio double between 0 and 1 specifies the training test
#' set ratio
#' @param training_split_indices Vector of Integers specifying which rows of the
#' full set should be taken to be in the training set. Default is NULL. If default
#' rows are chosen randomly
#'
#' @return Classification Training and Classification Test Set
#' @import dplyr
#' @export
prepare_iris <- function(training_set_ratio = 0.8, training_split_indices=NULL) {
  iris <- load_iris()

  iris$Species <- as.integer(iris$Species)

  # Convert Species into Integers
  iris$Species[iris$Species == "setosa"] <- 1L
  iris$Species[iris$Species == "versicolor"] <- 2L
  iris$Species[iris$Species == "virginica"] <- 3L

  rename(iris, y=Species) -> iris

  # split into training and test set
  if (is.null(training_split_indices)) {
  rand_subset <- sample(1:nrow(iris), training_set_ratio*nrow(iris))
  }
  else rand_subset <- training_split_indices
  training_set <- iris[rand_subset, ]
  test_set <- iris[-rand_subset, ]

  return(list(training_set, test_set))
}

