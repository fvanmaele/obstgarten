% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{generate_sin_2D}
\alias{generate_sin_2D}
\title{Generate 2D sine data set}
\usage{
generate_sin_2D(n, sigma = 0.2, k = 10)
}
\arguments{
\item{n}{number of generated data pairs}

\item{sigma}{standard deviation of irreducible error in y}

\item{k}{specifying field of view}
}
\description{
According to model y = (sin(sqrt(x^2+y^2)))/(sqrt(x^2+y^2)) + N(0, sigma)
}
