% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy.R
\name{cart_part}
\alias{cart_part}
\title{Cart partition}
\usage{
cart_part(s, j, A)
}
\arguments{
\item{s}{split point at which the data is separated (\code{numeric})}

\item{j}{dimension at which the data is separated (\code{numeric})}

\item{A}{data (\code{data.frame} or \code{matrix})}
}
\value{
\code{list} with the following elements:
\itemize{
\item \verb{$A1}: The rows \code{A[, j] < s}
\item \verb{$A2}: The rows \code{A[, j] >= s} (taken by complement)
}
}
\description{
partition data at a given split point and index
}
\examples{
cart_part(0.5,1,generate_sin_data(10))
}
