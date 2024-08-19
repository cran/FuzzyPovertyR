# Fuzzy monetary poverty estimation
#
# @description
# constructs fuzzy monetary poverty estimates as of Cerioli Zani
#
# @param x poverty predicate
# @param z1 parameter
# @param z2 parameter
# @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
# @param breakdown A factor of sub-domains to calculate estimates for.
# @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#
# @return a list containing the membership function values and its expected value
#
#
fm_cerioli<- function (x, z1, z2, weight, breakdown, ID) {
  if(z1 < min(x) | z1 > max(x))stop("The value of z1 has to be between the minimum and the maximum of the predicate")
  if(z2 < min(x) | z2 > max(x))stop("The value of z2 has to be between the minimum and the maximum of the predicate")
  if(z2<z1)stop("The value of z2 has to be > z1")

  N <- length(x)
  if(is.null(ID)) ID <- seq_len(N)

  y <- rep(NA_real_, N)
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
  fm_data <- data.frame(ID = ID, predicate = x, weight = weight, mu = y)
  fm_data <- fm_data[order(fm_data$mu),]
  return(list(results = fm_data,
              estimate = estimate,
              parameters = list(z1 = z1, z2 = z2),
              fm = "cerioli"))
}
