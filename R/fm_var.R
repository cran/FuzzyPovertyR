#' Fuzzy monetary poverty estimation
#'
#' @description This function estimates the variance of the fuzzy monetary poverty index
#'
#' @param predicate A numeric vector representing the poverty predicate (i.e. income or expenditure)
#' @param weight A numeric vector of sampling weights of the same length of predicate. if NULL weights will set equal to n (n = sample size)
#' @param fm The membership function (default is "verma". Other options are "ZBM", "belhadj2015", "belhadj2011", "chakravarty", "cerioli", "verma1999" and "TFR". See Betti et. al., 2023)
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence
#' @param type The variance estimation method chosen. One between `bootstrap_naive` (default), `bootstrap_calibrated` or `jackknife`
#' @param R The number of bootstrap replicates. Default is 500
#' @param M The size of bootstrap samples. Default is `nrow(data)`
#' @param stratum The vector identifying the stratum (if 'jackknife' is chosen as variance estimation technique)
#' @param psu The vector identifying the psu (if 'jackknife' is chosen as variance estimation technique)
#' @param f The finite population correction fraction (if 'jackknife' is chosen as variance estimation technique)
#' @param verbose Logical. whether to print the proceeding of the variance estimation procedure
#' @param HCR If fm="verma" or fm="verma1999" or fm="TFR" . The value of the head count ratio used to compute alpha so that the membership function equals the HCR
#' @param interval If fm="verma" or fm="verma1999" or fm="TFR". A numeric vector of length two to look for the value of alpha (if not supplied)
#' @param alpha The value of the exponent in equations of "verma", "verma1999" and "TFR". If NULL it is calculated so that it equates the expectation of the membership function to HCR.
#' @param hh.size If fm="ZBM". A numeric vector of household size
#' @param z_min A parameter of the membership function if fm="belhadj2011", i.e. the z_min: $mu=1 for 0 <y_i<z_min$ (see: See Betti et. al, 2023)
#' @param z_max A parameter of the membership function if fm="belhadj2011", i.e. the z_max: $mu=0 for y_i>z_max$ (see: See Betti et. al, 2023)
#' @param z1 A parameter of the membership function if fm="belhadj2015" or fm="cerioli". For  "belhadj2015" z1: $mu=1 for y_i<z1$ while for  "cerioli" $mu=1 for 0 <y_i<z1$ (see: See Betti et. al, 2023)
#' @param z2 A parameter of the membership function if fm="belhadj2015" or fm="cerioli". For  "belhadj2015" z2: $mu=0 for y_i>z2$   while for  "cerioli" the z1: $mu=0 for y_i>z2$ (see: See Betti et. al, 2023)
#' @param b A parameter of the membership function if fm="belhadj2015". The shape parameter (if b=1 the mf is linear between z1 and z2)
#' @param z A parameter of the membership function if fm="chakravarty", i.e. $mu=0 for y_i>=z$ (see: See Betti et. al, 2023)
#' @param Xs A matrix (i x j) of calibration variables. i number of units, j number of variables
#' @param total A Vector of population totals of dimension 1 x j
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha). If numeric will be coerced to a factor
#' @param data An optional data frame containing the variables to be used
#'
#' @import sampling
#' @return An object of class FuzzyMonetary containing the estimate of variance with the method selected. if breakdown is not NULL, the variance is estimated for each sub-domain.
#' @export
#' @examples
#' #The following examples are based on the dataset eusilc
#' #included in the package.
#'
#' #Example 1 using bootstrap and breakdown
#'
#' #fm = "verma"
#'
#' fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090,
#'        fm = "verma", breakdown = eusilc$db040, type = "bootstrap_calibrated",
#'        alpha = 4, Xs = eusilc[,4:6], total = c(20, 30, 40))
#'
#' #fm = "belhadj2015"
#'
#' fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090,
#'        fm = "belhadj2015", breakdown = eusilc$db040, type = "bootstrap_naive",
#'        z1 = 100, z2 = 15000, b = 2)
#'
#'
#' #Example 2 using jackknife without breakdown
#'
#' #fm = "verma1999"
#'
#' fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090,
#'        fm = "verma1999",  type = "jackknife",
#'        stratum = eusilc$stratum , psu = eusilc$psu,
#'        alpha = 4)
#'
#' #fm = "cerioli"
#'
#' fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090,
#'        fm = "cerioli",  type = "jackknife",
#'        stratum = eusilc$stratum , psu = eusilc$psu,
#'        z1 = 1000, z2 = 12000)

#' @references
#'
#' Belhadj, B. (2011). A new fuzzy unidimensional poverty index from an information theory perspective. Empirical Economics, 40(1):687–704.
#'
#' Belhadj, B. (2015). Employment measure in developing countries via minimum wage and poverty new fuzzy approach. Opsearch, 52(1):329–339.
#'
#' Betti, G., Cheli, B., Lemmi, A., and Verma, V. (2006). Multidimensional and longitudinal poverty: an integrated fuzzy approach. In Betti, G. and Lemmi, A., editors, Fuzzy set approach to multidimensional poverty measurement, pages 115–137. Springer, Boston, USA.
#'
#' Betti, G., D’Agostino, A., Lemmi, A., & Neri, L. (2023). The fuzzy approach to poverty measurement. In Research Handbook on Measuring Poverty and Deprivation Edited by Silber, J. (pp. 489-500). Edward Elgar Publishing.
#'
#' Betti, G. and Verma, V. (1999). Measuring the degree of poverty in a dynamic and comparative context: a multi-dimensional approach using fuzzy set theory. In Proceedings, iccs-vi, volume 11, pages 289–300.
#'
#' Cerioli, A. and Zani, S. (1990). A fuzzy approach to the measurement of poverty. In Income and Wealth Distribution, Inequality and Poverty: Proceedings of the Second International Conference on Income Distribution by Size: Generation, Distribution, Measurement and Applications., 272–284. Springer, Boston, USA.
#'
#' Chakravarty, S. R. (2006). An Axiomatic Approach to Multidimensional Poverty Measurement via Fuzzy Sets. Fuzzy Set Approach to Multidimensional Poverty Measurement, 49-72.
#'
#' Cheli, B. and Lemmi, A. (1995). A ’totally’ fuzzy and relative approach to the multidimensional analysis of poverty. 24(1):115–134.
#'
#' Zedini, A. and Belhadj, B. (2015). A new approach to unidimensional poverty analysis: Application to the Tunisian case. Review of Income and Wealth, 61(3):465–476.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.

fm_var <- function(predicate, weight,
                   fm, ID = NULL,
                   type = 'bootstrap_naive',
                   R = 100, M = NULL,
                   stratum, psu, f = 0.01,
                   verbose = FALSE,
                   HCR, interval = c(1,10), alpha = NULL,
                   hh.size, #k=3,
                   z_min, z_max,
                   z1, z2, b,
                   z,
                   Xs, total,
                   breakdown = NULL,
                   data = NULL) {
  if(!is.null(data)){
    predicate <- data[[predicate]]
    weight <- data[[weight]]
    breakdown <- data[[breakdown]]
    hh.size <- data[[hh.size]]
    ID <- data[[ID]]
  }
  if(!(type %in% c("bootstrap_naive","bootstrap_calibrated", "jackknife"))) stop("Select a variance estimation method from the list: bootstrap_naive, bootstrap_calibrated, jackknife ")

  N <- length(predicate)
  if(is.null(weight)) weight <- rep(N, N)
  if(is.null(ID)) ID <- seq_len(N)
  if(is.null(M)) M <- N
  if(!is.null(breakdown)) breakdown <- as.factor(breakdown)
  if(fm=="ZBM") k <- 3
  switch(type, # creare funzione bootstrap e funzione jackknife da chiamare qui invece che codificarle
         bootstrap_naive = {
           BootDistr <- sapply(1:R, function(r) {
             if(verbose == TRUE) {
               if(R%%100==0) cat('Bootstrap Replicate : ', r, 'of', R, '\n')
             }
             bootidx <- sample(1:N, size = M, replace = T)
             ID.boot <- ID[bootidx]
             predicate.boot <- predicate[bootidx]
             weight.boot <- weight[bootidx]
             if(!is.null(breakdown)) breakdown <- breakdown[bootidx]
             if(fm=="ZBM") {
               hh.size.boot <- hh.size[bootidx]
               try(fm_construct(predicate.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, z_min, z_max, z1, z2, b, z, breakdown, data = NULL)$estimate)
             } else {
               try(fm_construct(predicate.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, z_min, z_max, z1, z2, b, z, breakdown, data = NULL)$estimate)
             }

           }, simplify = "array")
           if(!is.null(breakdown)){
             if (fm=="ZBM") {
               var.hat <- apply(BootDistr, 1:2, var, na.rm = TRUE)
             } else {
               var.hat <- apply(BootDistr, 1, var, na.rm = TRUE)
             }

           } else {
             var.hat <- var(BootDistr, na.rm = TRUE)
           }
         },
         bootstrap_calibrated = {
           BootDistr <- sapply(1:R, function(r) {
             if(verbose == TRUE) {
               if(R%%100==0) cat('Bootstrap Replicate : ', r, 'of', R, '\n')
             }
             bootidx <- sample(1:N, size = M, replace = T)
             ID.boot <- ID[bootidx]
             predicate.boot <- predicate[bootidx]
             weight.boot <- sampling::calib(Xs = Xs, d = weight[bootidx], total = total, method = "linear")
             if(!is.null(breakdown)) breakdown <- breakdown[bootidx]
             if(fm=="ZBM") {
               hh.size.boot <- hh.size[bootidx]
               try(fm_construct(predicate.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, z_min, z_max, z1, z2, b, z, breakdown, data = NULL)$estimate)
             } else {
               try(fm_construct(predicate.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, z_min, z_max, z1, z2, b, z, breakdown, data = NULL)$estimate)
             }

           }, simplify = "array")
           if(!is.null(breakdown)){
             if (fm=="ZBM") {
               var.hat <- apply(BootDistr, 1:2, var, na.rm = TRUE)
             } else {
               var.hat <- apply(BootDistr, 1, var, na.rm = TRUE)
             }

           } else {
             var.hat <- var(BootDistr, na.rm = TRUE)
           }
         },
         jackknife = {
           tab <- data.frame(table(stratum, psu))
           a <- tapply(tab$Freq, tab$stratum, sum)
           if(any(a<2)) stop("There should be at least 2 PSUs in each stratum")
           strata <- unique(stratum)
           H <- length(strata) # can be also a character string then
           w_jh <- tapply(weight, list(stratum, psu), sum, na.rm = T) # sum of the weights inside the strata
           w_h <- rowSums(w_jh, na.rm = T) # sum of the weights inside the strata
           z_h <- vector(mode = 'list', length = H)
           var_h <- rep(0, H)
           if(!is.null(breakdown)) {
             J <- length(unique(breakdown))
             var_h <- matrix(0, nrow = H, ncol = J, dimnames = list(NULL, levels(as.factor(breakdown))))
             if(fm=="ZBM") var_h <- array(NA, dim = c(H, J, k), dimnames = list(NULL, levels(as.factor(breakdown)), 1:k))

           }
           for(h in 1:H){
             if(verbose == TRUE) cat('doing for stratum',h,'of',H,'\n')
             stratum_h <- strata[h]
             psu_h <- tab$psu[tab$stratum==stratum_h & tab$Freq > 0] # psu-s in statum h
             a_h <- length(psu_h)
             z_hi <- g_hi <- rep(0, a_h)
             if(!is.null(breakdown)) z_hi <- vector(mode = 'list', length = a_h)
             if(!is.null(breakdown) & fm == "ZBM") z_hi <- array(0, dim = c(J, k, a_h), dimnames = list(levels(breakdown), NULL, NULL))
             for(i in 1:a_h){
               if(verbose == T) cat('doing for psu', i, 'of',a_h,'\n')
               psu_jh <- psu_h[i]
               delete.idx <- !(stratum==stratum_h & psu==psu_jh) # eliminating observations in psu
               # change the weights
               case1.idx <- (stratum!=stratum_h)
               case2.idx <- (stratum==stratum_h & psu!=psu_jh)
               # case3 set to 0
               w <- rep(0,N)
               w[case1.idx] <- weight[case1.idx]
               g_hi[i] <- w_h[stratum_h]/(w_h[stratum_h] - w_jh[stratum_h, psu_jh])
               w[case2.idx] <- (weight*g_hi[i])[case2.idx]

               if(!is.null(breakdown)){
                 if(fm=="ZBM") {
                   z_hi[,,i] <- fm_construct(predicate[delete.idx], w[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size[delete.idx], z_min, z_max, z1, z2, b, z, breakdown[delete.idx], data)$estimate
                 } else{
                   z_hi[[i]] <- fm_construct(predicate[delete.idx], w[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size[delete.idx], z_min, z_max, z1, z2, b, z, breakdown[delete.idx], data)$estimate
                 }
               } else {
                 z_hi[i] <- fm_construct(predicate[delete.idx], w[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size[delete.idx], z_min, z_max, z1, z2, b, z)$estimate
               }
             }

             if(!is.null(breakdown)){

               if(fm=="ZBM"){
                 z_hi.bar <- array(apply(z_hi, 1:2, mean, na.rm = T), dim = c(J, k, a_h), dimnames = list(levels(breakdown), NULL ))
                 squared.dev <- (z_hi - z_hi.bar)^2
                 var_h[h,,] <- Reduce('+', lapply(1:a_h, function(i) g_hi[i]*squared.dev[,,i])) # oppure usare modified sum
               } else {
                 z_hi.breakdown <- do.call(rbind, z_hi)
                 means.breakdown <- rep(1, a_h)%*%t(apply(z_hi.breakdown,2,mean))
                 var_h.breakdown <- (1-f)*t(g_hi)%*%(z_hi.breakdown - means.breakdown)^2
                 var_h[h,] <- var_h.breakdown
               }

             } else { # if not breakdown
               var_h[h] <- (1-f)*sum(g_hi*(z_hi - mean(z_hi))^2)
             }
           }

           if(!is.null(breakdown)) {
             if(fm=="ZBM"){
               var.hat <- apply(var_h, 2:3, sum, na.rm = T)
             } else {
               var.hat <- apply(var_h, 2, sum, na.rm = T)
             }
           } else {
             var.hat <- sum(var_h, na.rm = T)

           }
         })


  # if(type=="bootstrap") var.hat <- list(variance = var.hat, quantiles = quantile(BootDistr, probs = c(.025, 0.5, 0.975)), type = type)
  if(!is.null(breakdown)) {
    var.hat <- list(variance = var.hat, size = table(breakdown), type = type)
  } else {
    var.hat <- list(variance = var.hat, type = type)
  }
  var.hat <- FuzzyPoverty(var.hat)
  return(var.hat)
}
