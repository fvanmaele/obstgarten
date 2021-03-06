% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy.R
\name{cart_greedy}
\alias{cart_greedy}
\title{Greedy algorithm for CART}
\usage{
cart_greedy(
  XY,
  depth = 10L,
  mode = "regression",
  threshold = 1L,
  random = FALSE,
  m = 0L,
  quantile = FALSE,
  q_threshold = 100L,
  q_pct = 0.25
)
}
\arguments{
\item{XY}{matrix with columns \eqn{1..d} (representing the training data
\eqn{X_i}) and column \eqn{y} (for the values \eqn{Y_i}).}

\item{depth}{The amount of steps before halting the algorithm (defaults to
10)}

\item{mode}{\code{regression} or \code{classification} specifies whether to train
a regression or classification tree, respectively. Default is "regression"}

\item{threshold}{The minimum amount of data points for dividing a leaf (\code{integer})}

\item{random}{generates CART for random forest (\code{logical}, default \code{FALSE})}

\item{m}{The
default: 0 so it only has to be set at random=TRUE (\code{numeric})}

\item{quantile}{whether to use quantiles for computing the optimal
subdivision (\code{logical}, defaults to \code{FALSE})}

\item{q_threshold}{minimal of data points for using quantiles (\code{integer},
defaults to \code{100L})}

\item{q_pct}{amount of probabilities for \code{quantile()}, in pct. of the data
set size. (\code{numeric}, defaults to \code{0.25})}
}
\value{
A regression or classification tree modeled after the training data
(\code{Baum})
}
\description{
Create a regression or classification tree tree greedily based
on training data.
}
\examples{
cart_greedy(generate_sin_data(100) , depth=5, threshold=1)
}
