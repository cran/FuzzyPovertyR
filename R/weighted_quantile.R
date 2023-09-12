#' Calculating a weighted quantile.
#'
#' @description This function calculates a weighted quantile
#'
#' @param x a numeric vector representing a statistical variable
#' @param w a numeric vector containing sampling weights
#' @param p the order of quantile
#'
#' @return a weighted quantile

weighted_quantile <- function(x, w, p) {

  na.idx <- is.na(x)
  x <- x[!na.idx]
  w <- w[!na.idx]

  n <- length(x)
  sum.w <- sum(w)
  cond <- p*sum.w
  order.idx <- order(x)
  x.ord <- x[order.idx]
  w.ord <- w[order.idx]

  w.cd <- cumsum(w.ord)
  test <- (w.cd == cond)
  if(any(test)){
    low.idx <- which(test)
    up.idx <- low.idx+1
    q_p <- 0.5*(x.ord[low.idx]+x.ord[up.idx])

  } else {
    low.idx <- sum(w.cd < cond)
    up.idx <- low.idx+1
    q_p <- x.ord[up.idx]

  }
  return(q_p)
}


