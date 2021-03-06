% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bv.R
\name{bv_bagging}
\alias{bv_bagging}
\title{bias variance data for 400 reps for Bagging with different numbers of Bootstrap samples}
\usage{
bv_bagging(bs_list, sigma = 0.2, n = 150, reps = 400)
}
\arguments{
\item{bs_list}{List of different integer bagging parameters B to be tested}

\item{sigma}{standard deviation of irreducible error in y}

\item{n}{number of generated data pairs}

\item{reps}{rows of data}
}
\description{
generate test date for different depths values of the CART algorithm with 400 reps and 150 data points
}
