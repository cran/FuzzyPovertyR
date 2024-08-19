# Fuzzy monetary poverty estimation
#
# @description
# Solves the non-linear equation in Betti et. al, 2018.
#
# @details
# Calculates the exponent parameter alpha of the non-linear equation of Betti et al, 2018 so that
# the expected value of the fuzzy membership function equated the head count ratio.
#
# @param predicate.ord a sorted vector of a predicate variable
# @param weight.ord a sorted vector of weights (in the same order of predicate.ord)
# @param interval The interval to look for the solution of the equation.
# @param verbose Logical. whether to print the proceeding of the procedure.
#
# @return the obtained exponent

fm_equate <- function(predicate.ord, weight.ord, interval, verbose){

  alpha <- uniroot(fm_objective,
                   interval = interval,
                   predicate.ord = predicate.ord,
                   weight.ord = weight.ord,
                   HCR = HCR,
                   verbose)$root
  if(verbose) cat('Done.\n')
  return(alpha)
}
