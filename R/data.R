#' Generate data set like in Example 6.3 of Richter19.pdf
#'
#' @param n number of generated data pairs
#' @param sigma standard deviation of irreducible error in y
#' @param reg logical TRUE for regression data FALSE for classification data
#' @param grid specify data points on x axis only works for reg == TRUE
#' @return if reg TRUE: n x 2 matrix (x_i, y_i) of generated data set
#' @return if reg FALSE: n x 3 matrix (x_1i, x_2i, y_i) of generated data set
#'
#' regression:
#' dat <- generate_sin_data(100, sigma=0.2)
#'
#' classification:
#' dat <- generate_sin_data(100, sigma=0.2, reg=FALSE)
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
#' According to model y = (sin(sqrt(x^2+y^2)))/(sqrt(x^2+y^2)) + N(0, sigma)
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
#' @param mu
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
#' Loads and returns Iris data set for classification testing
#'
#' @return 150 x 5 data.frame
#' @export
load_iris <- function() {
  data("iris")
  return(iris)
}

#' Method that prepares Iris dataset for classification with Tree-Methods.
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

#' mnist data set
#'
#' Loads and returns mnist data set
#'
#' MIT License Copyright 2008, Brendan O'Connor
#'
#' @return list containing:
#' 1) 60000 x 784 matrix of training images, each row one image,
#' 2) atomic vector of integer labels of training images,
#' 3) 10000 x 784 matrix of test images, each row ine image,
#' 4) atomic vector of integer labels of test images.
#'
#' See names() of returned list.
#' @export
load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <- load_image_file('data/mnist/train-images-idx3-ubyte')
  test <- load_image_file('data/mnist/t10k-images-idx3-ubyte')

  train$y <- load_label_file('data/mnist/train-labels-idx1-ubyte')
  test$y <- load_label_file('data/mnist/t10k-labels-idx1-ubyte')

  return(list(training_images=train$x, training_labels=train$y,
              test_images=test$x, test_labels=test$y))
}


#' Heart Disease Data Set
#'
# 'Loads heart disease dataset of the cleveland dataset and returns it as a dataframe with named columns.
#' For reference see https://archive.ics.uci.edu/ml/datasets/Heart+Disease
#'
#' The labels are given by the last column "Diagnosis_Heart_Disease" 0 indicates no heart disease
#' while 1, 2, 3, 4 indicate a heart disease
#'
#' data.frame still contains NA and ? values which need to be cleaned before
#' this can be used for testing and training
#'
#' @return 303 x 14 data.frame with named columns
#' @export
load_heart_disease <- function() {
  heart_disease_dataset <- read.csv(file="data/heart_disease_data/processed.cleveland.data", header = F)
  # names <- read.csv(file="data/heart_disease_data/heart-disease.names", header = F)
  names <- c("Age",
             "Sex",
             "Chest_Pain_Type",
             "Resting_Blood_Pressure",
             "Serum_Cholesterol",
             "Fasting_Blood_Sugar",
             "Resting_ECG",
             "Max_Heart_Rate_Achieved",
             "Exercise_Induced_Angina",
             "ST_Depression_Exercise",
             "Peak_Exercise_ST_Segment",
             "Num_Major_Vessels_Flouro",
             "Thalassemia",
             "Diagnosis_Heart_Disease")
  colnames(heart_disease_dataset) <- names
  return(heart_disease_dataset)
}
