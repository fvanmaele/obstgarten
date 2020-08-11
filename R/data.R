library(MASS)
library(matlib)

#' Generate data set like in Example 6.3 of Richter19.pdf
#'
#' @param n number of generated data pairs
#' @param sigma standard deviation of irreducible error in y
#' @param reg logical TRUE for regression data FALSE for classification data
#' @param grid specify data points on x axis only works for reg == TRUE
#' @return if reg TRUE: n x 2 matrix [x_i, y_i] of generated data set
#' @return if reg FALSE: n x 3 matrix [x_1i, x_2i, y_i] of generated data set
#' @examples
#' # regression:
#' dat <- generate_sin_data(100, sigma=0.2)
#' plot(dat[, 1], dat[, 2], xlim=c(0, 1), ylim = c(-1, 1))
#'
#' # classification:
#' dat <- generate_sin_data(100, sigma=0.2, reg=FALSE)
#' plot(dat[, 1], dat[, 2], xlim=c(0, 1), ylim = c(0, 1), col=dat[, 3])
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
    return(ret)
  }
}

#' Generate Data from a multivariate Gaussian
#' @param n dimensional vector with feature means
#' @param sigma positive definite square covariance matrix
#' @return list(df of data, feature means vector, covariance matrix)
#' @example generate_mult_data(n=1000, d=5)
generate_mult_data <- function(n, d, mu=NULL, sigma=NULL) {
  if (is.null(sigma)) {
    A <- matrix(runif(d*d, min = 0, max = 1), nrow=d, ncol=d)
    sigma <- t(A) %*% A
    }
  if (is.null(mu)) {
    mu <- runif(d, min = -1, max = +1)
  }

  data <- mvrnorm(n = n, mu=mu, Sigma=sigma, tol = 1e-06, empirical = FALSE)

  mvg <- function(x) {
    return((2*pi)**(-d/2) * det(sigma)**(-1/2) *
             exp(-1/2 * (t(x - mu) %*% inv(sigma) %*% (x - mu))))
  }

  y <- apply(data, mvg, MARGIN = 1)
  data <- data.frame(x=data, y=y)

  return(list(data, mu, sigma))
}


# test data for random forest with d = 3
generate_sin_data2 <- function(n, sigma=0.2, reg=TRUE) {
  if (sigma <= 0.) {
    stop("Error: sigma has to be > 0")
  }

  eps <- rnorm(n, mean=0, sd=sigma)

  if (reg) {
    a <- sample(-0.01:0.01, 1)
    b <- sample(-0.01:0.01, 1)
    M <- matrix(runif(n, min=0, max=1))
    M <- cbind(M, M[,1]+a, M[,1]+b)
    for (j in 1:ncol(M)) {
      for (i in 1:nrow(M)) {
        if(M[i,j] > 1){
          M[i,j] <- 1
        }
        if(M[i,j] < 0){
          M[i,j] <- 0
        }
      }
    }
    y <- c()
    for (i in 1:n) {
      y <- c(y, sin(2 * pi * mean(M[[i,1]], M[[i,2]], M[[i,3]])) + eps[i])
    }
    ret <- cbind(M, y)
    colnames(ret) <- c("x1", "x2", "x3", "y")
    return(ret)
  }
  else {
    x1 <- runif(n, min=0, max=1)
    x2 <- runif(n, min=0, max=1)
    k <- x2 - 0.5 - 0.3 * sin(2 * pi * x1)
    y <- rep(1, times=n)
    y[(k - eps) > 0.] <- 2
    ret <- matrix(c(x1, x2, y), nrow=n, ncol=3)
    colnames(ret) <- c("x1", "x2", "y")
    return(ret)
  }
}

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
