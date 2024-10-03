# Fuzzy monetary poverty estimation
#
# @description This function calculates the fuzzy membership function as defined in Betti et. al, 2018.
#
# @param predicate.ord A sorted vector of a predicate variable (in ascending order).
# @param weight.ord A sorted vector of weights (in the same order of s.ord)
# @param alpha The value of the exponent parameter to use in the non-linear equation as of Betti et. al, 2018.
#
# @return A numeric vector containing the estimated membership function.
#
fm_mu <- function(predicate.ord, weight.ord, alpha){

  N = length(predicate.ord)
  tot1 = sum(predicate.ord[2:N])
  tot2 = sum( (predicate.ord*weight.ord)[2:N] )

  F <- rep(0,N)
  L <- rep(0,N)

  cond <- predicate.ord > predicate.ord[1]
  tot1 <- sum( weight.ord*cond )
  tot2 <- sum( (predicate.ord*weight.ord)*cond )

  i <-  1
  while(i < N) {
    flag_i <- predicate.ord > predicate.ord[i]
    F[i] <- sum( weight.ord*flag_i ) / tot1
    L[i] <- sum( (predicate.ord*weight.ord)[ flag_i ]) / tot2
    i <- i + 1
  }

  u <- F^(alpha-1)*L

  return(u)
}
