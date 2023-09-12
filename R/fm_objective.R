#' Fuzzy monetary poverty estimation.
#'
#' @param predicate.ord A sorted vector of a predicate variable (in ascending order).
#' @param weight.ord A sorted vector of weights (in the same order of s.ord)
#' @param alpha The value of the exponent parameter to use in the non-linear equation as of Betti et. al, 2018.
#' @param HCR The head count ratio.
#' @param fm the type of membership function to use
#' @param verbose prints the proceeding of the routine.
#'
#' @return The value of the objective function
#'
fm_objective <- function(predicate.ord, weight.ord, alpha, HCR, fm, verbose){
  switch(fm,
         verma = {FM <- fm_mu(predicate.ord, weight.ord, alpha)},
         verma2 = {FM <- fm_mu2(predicate.ord, weight.ord, alpha)},
         TFR = {FM <- fm_mu_TFR(predicate.ord, weight.ord, alpha)})
  if(verbose) cat('trying with alpha: ', round(alpha, 3) , ' Expected Value: ', round(weighted.mean(x = FM, w = weight.ord), 3), "\n")
  return( weighted.mean(x = FM, w = weight.ord) - HCR )
}
