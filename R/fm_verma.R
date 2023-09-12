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
fm_verma <- function(predicate, weight, ID, HCR, interval, alpha, breakdown, verbose){ # cambiare ordine dei parametri
  N <- length(predicate)
  if(is.null(ID)) ID <- seq_len(N)
  if(!is.null(breakdown)) {
    fm_data <-  data.frame(ID = ID, predicate = predicate, weight = weight, breakdown = breakdown) %>% arrange(predicate)
  } else {
    fm_data <-  data.frame(ID = ID, predicate = predicate, weight = weight) %>% arrange(predicate)
  }

  predicate.ord <- fm_data[['predicate']]
  weight.ord <- fm_data[['weight']] # actually ordered according to predicate

  if(is.null(alpha)){
    if(verbose) cat('Solving non linear equation: E[u] = HCR\n')
    alpha <- uniroot(fm_objective,
                     interval = interval,
                     predicate.ord = predicate.ord,
                     weight.ord = weight.ord,
                     HCR = HCR,
                     fm = "verma",
                     verbose)$root
    if(verbose) cat('Done.\n')
  }

  fm_data$mu <- fm_mu(predicate.ord, weight.ord, alpha)
  estimate <- weighted.mean(fm_data$mu, fm_data$weight)

  if(!is.null(breakdown)){

    fm_data.list <- split(fm_data, f = ~ breakdown)
    estimate <- sapply(fm_data.list, function(x) weighted.mean(x$mu, x$weight))

    # fa funzioni di appartenenza per ogni breakdown. per altro paper ;T

    # fm_data.list <- split(fm_data, breakdown) %>% lapply(function (x) x %>% arrange(predicate))
    # mu.list <- lapply(fm_data.list, function(x) fm_mu(x$predicate, x$weight, alpha))
    # fm_data.list <- Map(cbind, fm_data.list, mu.list); for(i in 1:length(fm_data.list)) colnames(fm_data.list[[i]]) <- c('ID', 'predicate','weight','membership')
    # fm_estimates <- sapply(fm_data.list, function(x) weighted.mean(x$membership, x$weight))
    # return(list(results = fm_data.list, estimates = fm_estimates, alpha = alpha))

  }
  out <- list(results = fm_data, estimate = estimate, alpha = alpha)
  return(out)

}
