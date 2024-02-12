#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param z2 Parameter
#' @param z Parameter
#' @param b Parameter
#'
#' @return The fuzzy membership function as of Belhadj (2015).
#'
#'
belhadj2015 <- function(x, z1, z2, z, b){
  if(z1 < min(x) | z1 > max(x)) stop("The value of z1 has to be between the minimum and the maximum of the predicate")
  if(z2 < min(x) | z2 > max(x)) stop("The value of z2 has to be between the minimum and the maximum of the predicate")
  if(z2<z1) stop("The value of z2 has to be > z1")
  if(b<1) stop("The value of b has to be >=1")

  y <- x
  y[x<z1] <- 1
  y[x>=z2] <- 0
  y[z1<=x & x<z] <- ub1(x[z1<=x & x<z], z1, b)
  y[z<=x & x<z2] <- ub2(x[z<=x & x<z2], z2, b)
  return(y)
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param b Parameter
#'
#' @return returns one of the two membership functions as of Belhadj (2015).
#'
ub1 <- function(x, z1, b){
  return( 1-0.5*((x-z1)/z1)^b)
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z2 Parameter
#' @param b Parameter
#'
#' @return returns one of the two membership functions as of Belhadj (2015).
#'
ub2 <- function(x, z2, b){
  return( 0.5*((z2-x)/z2)^b )
}

# ddx_ub2 <- function(x, z2, b){
#   return((1/(2*z2))*b*(b-1)*(1-x/z2)^(b-2))
# }

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param z2 Parameter
#' @param b Parameter
#'
#' @return the difference between membership functions
#'
z_fun <- function(x, z1, z2, b){
  return( ub1(x, z1, b) - ub2(x, z2, b))
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param z2 Parameter
#' @param b Parameter
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#' @param weight A numeric vector of sampling weights
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#'
#' @return a list containing the fuzzy membership function and the value of z found as of Belhadj(2015).
#'
#'
fm_belhadj2015 <- function(x, z1, z2, b, breakdown, weight, ID){
  # uniroot(ddx_ub2, z2, b, interval = c(0, 100))
  N <- length(x)
  if(is.null(ID)) ID <- seq_len(N)
  z <- uniroot(z_fun, z1, z2, b, interval = c(z1, z2), extendInt = "yes")$root
  mu <- belhadj2015(x, z1, z2, z, b)
  if(!is.null(breakdown)) {
    # estimate <- tapply( (mu*weight)/sum(weight), INDEX = breakdown, mean)
    estimate <- sapply(split(data.frame(mu, weight, breakdown), f = ~ breakdown), function(X) weighted.mean(X[["mu"]], w = X[["weight"]]))
  } else {
    estimate <- weighted.mean(x = mu, w = weight)
  }
  fm_data <- data.frame(ID = ID, predicate = x, weight = weight, mu = mu)
  fm_data <- fm_data[order(fm_data$mu),]
  return(list(results = fm_data,
              estimate = estimate,
              parameters = list(z1 = z1, z2 = z2, z = z, b = b),
              fm = "belhadj2015"))
}






