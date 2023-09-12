#' Fuzzy monetary poverty estimation
#'
#' @description
#' constructs fuzzy monetary poverty estimates as of Cerioli Zani
#'
#' @param x poverty predicate
#' @param z1 parameter
#' @param z2 parameter
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param breakdown A factor of sub-domains to calculate estimates for.
#'
#' @return a list containing the membership function values and its expected value
#'
#'
fm_cerioli<- function (x, z1, z2, weight, breakdown) {
  N <- length(x)
  y <- rep(NA, N)
  y[0 <= x & x < z1] <- 1
  y[z1 <= x & x < z2] <- (z2 - x[z1 <= x & x < z2])/(z2-z1)
  y[x >= z2] <- 0
  if (!is.null(breakdown)) {
    estimate <- sapply(split(data.frame(y, weight, breakdown), f = ~ breakdown),
                       function(X) weighted.mean(X[["y"]], w = X[["weight"]]))
  }
  else {
    estimate <- weighted.mean(x = y, w = weight)
  }
  return(list(mu = y, estimate = estimate))
}
