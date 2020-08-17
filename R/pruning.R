#' Cost-Complexity pruning of greedy CART
#' @description Create a regression or classification tree tree greedily based
#' on training data.
#' @details
#' Unless `sample` is set to `TRUE`, it is required that there are no observations
#' \eqn{(X_{i_1}, Y_{i_1})} and \eqn{(X_{i_2}, Y_{i_2})} with \eqn{X_{i_1} =
#' X_{i_2}}, but \eqn{Y_{i_1} \neq Y_{i_2}}.
#' @param XY matrix with columns \eqn{1..d} (representing the training data
#'   \eqn{X_i}) and column \eqn{y} (for the values \eqn{Y_i}).
#' @param depth The amount of steps before halting the algorithm (defaults to
#'   10)
#' @param threshold The minimum amount of data points for dividing a leaf (`integer`)
#' @param sample Use `sample()` to (`logical`)
#' @param mode `regression` or `classification` specifies whether to train
#'   a regression or classification tree, respectively. Default is "regression"
#' @param random generates CART for random forest (`logical`, default `FALSE`)
#' @param m The
#' default: 0 so it only has to be set at random=TRUE (`numeric`)
#' @param quantile whether to use quantiles for computing the optimal
#'   subdivision (`logical`, defaults to `FALSE`)
#' @param q_threshold minimal of data points for using quantiles (`integer`,
#'   defaults to `100L`)
#' @param q_pct amount of probabilities for `quantile()`, in pct. of the data
#'   set size. (`numeric`, defaults to `0.25`)
#' @param lambda Cost weight.
#' @return A regression or classification tree modeled after the training data
#'   (`Baum`), pruned according to Cost-Complexity
#' @examples cart_greedy_prune(generate_sin_data(100))
#' @export
cart_greedy_prune <- function(XY, depth = 10L, mode="regression", threshold = 1L,
                              sample = FALSE, random = FALSE, m = 0L,
                              quantile = FALSE, q_threshold = 100L, q_pct = 0.25, lambda = "CV") {

  Cart <- cart_greedy(XY, depth = depth, mode = mode, threshold = threshold,
                      sample = sample, random = random, m = m,
                      quantile = quantile, q_threshold = q_threshold, q_pct = q_pct)

  return(Cart)
}
