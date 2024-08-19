# Fuzzy supplementary poverty estimation.
#
# @param j The number identifying one of the latent dimensions discovered
# @param step2 The result obtained from Step2 of the fuzzy supplementary procedure.
# @param dimensions A numeric vector enumerating the dimensions discovered
# @param rho The critical value to be used for calculation of weights in the kendall correlation matrix.
# @param ... other parameters
#
# @return The weight obtained for item j in dimension h as of Formula 12 of Betti and Verma 2008.
#
# @references
# Betti, G., & Verma, V. (2008). Fuzzy measures of the incidence of relative poverty and deprivation: a multi-dimensional perspective. Statistical Methods and Applications, 17(2), 225-250.
#
wb.jh <- function(j, step2, dimensions, rho, ...){
  col_sel <- (dimensions==j)
  X <- step2[,col_sel]
  cor.mat.h = cor( X , method = "kendall" ); cor.mat.h[is.na(cor.mat.h)] <- 0L # to avoid strange rhos in bootstrap (-inf) and rhos greater than 1
  if(is.null(rho)) rho <- max( diff( sort( c(1, cor.mat.h[upper.tri(cor.mat.h)] ), decreasing = F) ) ) # critical value
  # print(cor.mat.h); print(rho)
  wb_jh.denom.first <- apply(cor.mat.h, 2, function(x) 1/(1 + sum( x[x < rho], na.rm = T )) ) # remove na for NA in cor matrix
  wb_jh.denom.sec <- apply(cor.mat.h, 2, function(x) 1/sum(x[x > rho], na.rm = T ) ) # remove na for NA in cor matrix
  wb_jh <- (wb_jh.denom.first*wb_jh.denom.sec) # returns the weights for items j_1, j_2,..., j_h in the h dimension.

  return(wb_jh)
}
