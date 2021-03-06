% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bv.R
\name{bv_greedy}
\alias{bv_greedy}
\title{bias variance data for 400 reps for CARTs of different depths}
\usage{
bv_greedy(depths_list, sigma = 0.2, n = 150, reps = 400)
}
\arguments{
\item{depths_list}{List of different integer depth parameters to be tested}

\item{sigma}{standard deviation of irreducible error in y}

\item{n}{number of generated data pairs}

\item{reps}{rows of data}
}
\description{
generate test data for different depths values of the CART algorithm with 400 reps and 150 data points
}
