# Fuzzy monetary poverty estimation
#
# @description This function calculates the fuzzy membership function as defined in Betti et. al, 2018.
#
# @param predicate.ord A sorted vector of a predicate variable (in ascending order).
# @param weight.ord A sorted vector of weights (in the same order of s.ord)
#
# @return A numeric vector containing the estimated membership function.
#
fm_FL <- function(predicate.ord, weight.ord){

  N = length(predicate.ord)
  tot1 = sum(predicate.ord[2:N])
  tot2 = sum( (predicate.ord*weight.ord)[2:N] )

  F1 <- rep(0,N)
  L1 <- rep(0,N)

  cond <- predicate.ord > predicate.ord[1]
  tot1 <- sum( weight.ord*cond )
  tot2 <- sum( (predicate.ord*weight.ord)*cond )

  i <-  1
  while(i < N) {
    flag_i <- predicate.ord > predicate.ord[i]
    F1[i] <- sum( weight.ord*flag_i ) / tot1
    L1[i] <- sum( (predicate.ord*weight.ord)[ flag_i ]) / tot2
    i <- i + 1
  }

  return(list(WECDF = F1, Lorenz = L1))
}
