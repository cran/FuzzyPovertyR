#' Fuzzy monetary poverty estimation
#'
#' @description This function estimates the variance of the fuzzy monetary poverty index
#'
#' @param predicate A numeric vector representing the poverty predicate (i.e. income or expenditure)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param fm the type of membership function to use
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha). Ff numeric will be coherced to a factor.
#' @param type The variance estimation method chosen. One between `bootstrap` (default) or `jackknife`.
#' @param R The number of bootstrap replicates. Default is 500.
#' @param M The size of bootstrap samples. Default is `nrow(data)`.
#' @param stratum The vector identifying the stratum (if 'jackknife' is chosen as variance estimation technique).
#' @param psu The vector identifying the psu (if 'jacknife' is chosen as variance estimation technique).
#' @param f The finite population correction fraction (if 'jackknife' is chosen as variance estimation technique).
#' @param verbose Logical. whether to print the proceeding of the variance estimation procedure.
#' @param HCR If fm="verma" or fm="verma1999" or fm="TFR" . The value of the head count ratio.
#' @param interval If fm="verma" or fm="verma1999" or fm="TFR". A numeric vector of length two to look for the value of alpha (if not supplied).
#' @param alpha If fm="verma" or fm="verma1999" or fm="TFR". The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#' @param hh.size If fm="ZBM". A numeric vector of household size.
#' @param k If fm="ZBM". The number of change points locations to estimate.
#' @param z_min A parameter of the membership function if fm="belhadj2011"
#' @param z_max A parameter of the membership function if fm="belhadj2011"
#' @param z1 A parameter of the membership function if fm="belhadj2015" or fm="cerioli"
#' @param z2 A parameter of the membership function if fm="belhadj2015" or fm="cerioli"
#' @param b A parameter of the membership function if fm="belhadj2015". The shape parameter (if b=1 the mf is linear between z1 and z2)
#' @param z A parameter of the membership function if fm="chakravarty".
#' @param data an optional data frame containing the variables to be used.
#'
#' @return The estimate of variance with the method selected. if breakdown is not NULL, the variance is estimated for each sub-domain.
#' @export
#' @examples
#' data(eusilc)
#' HCR <- 0.14
#' hh.size <- rep(1, 1000)
#' fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090,
#' fm = "verma", breakdown = eusilc$db040, type = "bootstrap", HCR = .14, alpha = 9)
#'
fm_var <- function(predicate, weight, fm, ID = NULL,
                   breakdown = NULL, type = 'bootstrap',
                   R = 100, M = NULL,
                   stratum, psu, f = 0.01,
                   verbose = FALSE,
                   HCR, interval = c(1,10), alpha = NULL,
                   hh.size, k=3,
                   z_min, z_max,
                   z1, z2, b,
                   z,
                   data = NULL) {
  if(!is.null(data)){
    predicate <- data[[predicate]]
    weight <- data[[weight]]
    breakdown <- data[[breakdown]]
    hh.size <- data[[hh.size]]
    ID <- data[[ID]]
  }
  if(!(type %in% c("bootstrap", "jackknife"))) stop("Select a variance estimation method from the list:  bootstrap, jackknife ")
  N <- length(predicate)
  if(is.null(weight)) weight <- rep(N, N)
  if(is.null(ID)) ID <- seq_len(N)
  if(is.null(M)) M <- N
  if(!is.null(breakdown)) breakdown <- as.factor(breakdown)
  switch(type, # creare funzione bootstrap e funzione jacknife da chiamare qui invece che codificarle
         bootstrap = {
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
               try(fm_construct(predicate.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, k, z_min, z_max, z1, z2, b, z, breakdown)$estimate)
             } else {
               try(fm_construct(predicate.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, k, z_min, z_max, z1, z2, b, z, breakdown)$estimate)
             }

           }, simplify = "array")
           if(!is.null(breakdown)){
               if (fm=="ZBM") {
               var.hat <- apply(BootDistr, 1:2, var, na.rm = TRUE)
               } else {
               var.hat <- apply(BootDistr, 1, var, na.rm = TRUE)
             }

           } else {
             var.hat <- var(BootDistr)
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
                   z_hi[,,i] <- fm_construct(predicate[delete.idx], w[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size[delete.idx], k , z_min, z_max, z1, z2, b, z, breakdown[delete.idx])$estimate
                 } else{
                   z_hi[[i]] <- fm_construct(predicate[delete.idx], w[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size[delete.idx], k , z_min, z_max, z1, z2, b, z, breakdown[delete.idx])$estimate
                 }
               } else {
                 z_hi[i] <- fm_construct(predicate[delete.idx], w[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size[delete.idx], k , z_min, z_max, z1, z2, b, z)$estimate
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
  var.hat
}


