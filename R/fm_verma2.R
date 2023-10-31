#' Fuzzy monetary poverty estimation
#'
#' @description
#' `fm_construct` constructs fuzzy monetary poverty estimates.
#'
#' @details
#' It implements the fuzzy set approach to monetary poverty measurement where
#' the usual dichotomy poor (1) not-poor(0) is replaced with a continuum score in $(0,1)$
#'
#' @param predicate A numeric vector of a predicate variable (i.e. equivalised income or expenditure)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param HCR The value of the head count ratio (this is not used in the case that alpha is supplied by the user).
#' @param interval A numeric vector of length two to look for the value of alpha (if not supplied).
#' @param alpha The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#' @param verbose Logical. whether to print the proceeding of the procedure.
#'
#' @return
#' A list containing the (fuzzy) membership function for each individual in the sample, the estimated expected value of the function, the alpha parameter.
#'
#'
fm_verma2 <- function (predicate, weight, ID, HCR, interval, alpha, breakdown, verbose) {
  if(!is.null(alpha)) if(alpha < 1) stop("The value of alpha has to be >=1")
  N <- length(predicate)
  if (is.null(ID)) ID <- seq_len(N)
  fm_data <- data.frame(ID = ID, predicate = predicate, weight = weight)
  if(!is.null(breakdown)) fm_data <- data.frame(fm_data, breakdown)
  fm_data <- fm_data %>% dplyr::arrange(predicate)

  predicate.ord <- fm_data[["predicate"]]
  weight.ord <- fm_data[["weight"]]
  if (is.null(alpha)) {
    if(verbose) cat("Solving non linear equation: E[u] = HCR\n")
    alpha <- uniroot(fm_objective,
                     interval = interval,
                     predicate.ord = predicate.ord,
                     weight.ord = weight.ord,
                     HCR = HCR,
                     fm = "verma2",
                     verbose)$root
    if(verbose) cat("Done.\n")
  }
  fm_data$mu <- fm_mu2(predicate.ord, weight.ord, alpha)
  estimate <- weighted.mean(fm_data$mu, fm_data$weight)
  if (!is.null(breakdown)) {
    fm_data <- split(data.frame(fm_data), f = ~ fm_data$breakdown)
    estimate <- sapply(fm_data, function(x) weighted.mean(x$mu, x$weight))
  }
  out <- list(results = fm_data, estimate = estimate, alpha = alpha)
  return(out)
}

#' Fuzzy predicate poverty estimation
#'
#' @param predicate.ord A ordered numeric vector of a predicate variable (i.e. equivalised income or expenditure)
#' @param weight.ord A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param alpha The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#'
#' @return A numeric vector containing the estimated membership function.
#'
fm_mu2 <- function (predicate.ord, weight.ord, alpha) {
  N = length(predicate.ord)
  tot2 = sum((predicate.ord * weight.ord)[2:N])
  L <- rep(0, N)
  cond <- predicate.ord > predicate.ord[1]
  tot2 <- sum((predicate.ord * weight.ord) * cond)
  i <- 1
  while (i < N) {
    flag_i <- predicate.ord > predicate.ord[i]
    L[i] <- sum((predicate.ord * weight.ord)[flag_i])/tot2
    i <- i + 1
  }
  u <- L^(alpha)
  return(u)
}
