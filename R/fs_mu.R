# Fuzzy supplementary poverty estimation.
#
# @param s.ord A vector of ordered deprivation scores.
# @param w.ord A vector of ordered sampling weights. In the same order of `s.ord`.
# @param alpha The alpha parameter.
#
# @return the fuzzy membership function.
#
fs_mu <- function(s.ord, w.ord, alpha){

  #@ s.ord. a sorted vector of deprivation scores.
  #@ w.ord a sorted vector of weights (in the same order of s.ord)
  N = length(s.ord)
  tot1 = sum(s.ord[2:N])
  tot2 = sum( (s.ord*w.ord)[2:N] )

  F <- rep(0,N)
  L <- rep(0,N)

  cond <- s.ord > s.ord[1]
  tot1 <- sum( w.ord*cond )
  tot2 <- sum( (s.ord*w.ord)*cond )

  i <-  1
  while(i < N) {
    flag_i <- s.ord > s.ord[i]
    F[i] <- sum( w.ord*flag_i ) / tot1
    L[i] <- sum( (s.ord*w.ord)[ flag_i ]) / tot2
    i <- i + 1
  }

  mu <- F^(alpha-1)*L
  return(mu)
}
