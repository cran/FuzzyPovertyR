#' Fuzzy monetary poverty estimation
#'
#' @description
#' constructs fuzzy monetary poverty estimates.
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
#' @return The membership function of the Total Fuzzy and Relative indicator.
#'

fm_TFR = function (predicate, weight, ID, HCR, interval, alpha, breakdown, verbose) {
  N <- length(predicate)
  if (is.null(ID)) ID <- seq_len(N)
  fm_data <- data.frame(ID = ID, predicate = predicate, weight = weight)
  if(!is.null(breakdown)) fm_data <- data.frame(fm_data, breakdown = breakdown)

  fm_data <- fm_data %>% dplyr::arrange(predicate)

  predicate.ord <- fm_data[["predicate"]]
  weight.ord <- fm_data[["weight"]]
  if(is.null(alpha)) {
    if(verbose) cat("Solving non linear equation: E[u] = HCR\n")
    alpha <- uniroot(fm_objective,
                     interval = interval,
                     predicate.ord = predicate.ord,
                     weight.ord = weight.ord,
                     HCR = HCR,
                     fm = "TFR",
                     verbose)$root
    if(verbose) cat("Done.\n")
  }
  fm_data$mu <- fm_mu_TFR(predicate.ord, weight.ord, alpha)
  estimate <- weighted.mean(fm_data$mu, fm_data$weight)
  if (!is.null(breakdown)) {
    fm_data <- split(data.frame(fm_data), f = ~ fm_data$breakdown)
    estimate <- sapply(fm_data, function(x) weighted.mean(x$mu, x$weight))
  }
  out <- list(results = fm_data, estimate = estimate, alpha = alpha)
  return(out)
}


#' Fuzzy monetary poverty estimation
#'
#' @param predicate.ord A ordered numeric vector of a predicate variable (i.e. equivalised income or expenditure)
#' @param weight.ord A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param alpha The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#'
#' @return A numeric vector containing the estimated membership function.
#'
fm_mu_TFR = function (predicate.ord, weight.ord, alpha) {
  N = length(predicate.ord)
  tot1 = sum(predicate.ord[2:N])
  F <- rep(0, N)
  cond <- predicate.ord > predicate.ord[1]
  tot1 <- sum(weight.ord * cond)
  i <- 1
  while (i < N) {
    flag_i <- predicate.ord > predicate.ord[i]
    F[i] <- sum(weight.ord * flag_i)/tot1
    i <- i + 1
  }
  u <- as.numeric(F^(alpha) )
  return(u)
}
