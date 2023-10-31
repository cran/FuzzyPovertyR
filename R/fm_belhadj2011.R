#' Fuzzy monetary poverty estimation
#'
#' @description
#' constructs fuzzy monetary poverty estimates as of Belhadj (2011)
#'
#' @param x poverty predicate
#' @param z_min parameter
#' @param z_max parameter
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param breakdown A factor of sub-domains to calculate estimates for.
#'
#' @return a list containing the membership function values and its expected value
#'
#'
fm_belhadj2011 <- function (x, z_min, z_max, weight, breakdown) {
  if(z_min < min(x) | z_min > max(x))stop("The value of z_min has to be between the minimum and the maximum of the predicate")
  if(z_max < min(x) | z_max > max(x))stop("The value of z_max has to be between the minimum and the maximum of the predicate")
  if(z_max<z_min)stop("The value of z_max has to be > z_min")

  N <- length(x)
  y <- rep(NA, N)
  y[0 <= x & x < z_min] <- 1
  y[z_min <= x & x < z_max] <- (z_max - x[z_min <= x & x < z_max])/(z_max-z_min)
  y[x >= z_max] <- 0
  if (!is.null(breakdown)) {
    estimate <- sapply(split(data.frame(y, weight, breakdown), f = ~ breakdown),
                       function(X) weighted.mean(X[["y"]], w = X[["weight"]]))
  }
  else {
    estimate <- weighted.mean(x = y, w = weight)
  }
  return(list(mu = y, estimate = estimate))
}
