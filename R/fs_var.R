##################
#--- Variance ---#
#' Fuzzy supplementary poverty estimation.
#'
#' @param data A matrix or data frame of items
#' @param weight A numeric vector of sampling weights of length nrow(step1). if NULL weights will set equal to n (n = sample size)
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence
#' @param dimensions A numeric vector (of length  `ncol(data)`) of assignments of items in data to dimensions
#' @param HCR The value of the head count ratio used to compute alpha so that the expected value of the membership function equals HCR
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha). If numeric will be coerced to a factor
#' @param alpha The value of the exponent in equations of "verma", "verma1999" and "TFR". If NULL it is calculated so that it equates the expectation of the membership function to HCR.
#' @param rho Optional critical value to be used for calculation of weights in the Kendall correlation matrix. If NULL rho is set equal to the point of largest gap between the ordered set of correlation values encountered (see Betti and Verma, 2008)
#' @param type The variance estimation method chosen. One between `bootstrap_naive` (default), `bootstrap_calibrated` or `jackknife`
#' @param R The number of bootstrap replicates. Default is 500
#' @param M The size of bootstrap samples. Default is `nrow(data)`
#' @param stratum The vector identifying the stratum (if 'jackknife' is chosen as variance estimation technique)
#' @param psu The vector identifying the psu (if 'jackknife' is chosen as variance estimation technique)
#' @param f The finite population correction fraction (if 'jackknife' is chosen as variance estimation technique
#' @param Xs A matrix (i x j) of calibration variables. i number of units, j number of variables
#' @param total A Vector of population totals of dimension 1 x j
#' @param fixed Whether the membership function needs to be re-calculated at each bootstrap or jackknife replicate (default is FALSE)
#' @param verbose Logical. whether to print the proceeding of the variance estimation procedure
#'
#' @import sampling
#' @return An object of class FuzzySupplementary containing the estimated variance.
#' @export
#'
#' @examples
#'
#' #This example is based on the dataset eusilc included in the package
#' #The  variance of the FS index is compute without breakdown
#' #and using an alpha = 2
#'
#' #############
#' ##Bootstrap##
#' #############
#'
#' fs_var(data = eusilc[,4:23], weight = eusilc$DB090, ID = NULL,
#'        dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5),
#'        breakdown = NULL, alpha = 2,
#'        rho = NULL, type = 'bootstrap_naive', M = NULL, R = 2, verbose = TRUE)
#'
#'
#'
#'
#' @references
#' Betti, G., & Verma, V. (2008). Fuzzy measures of the incidence of relative poverty and deprivation: a multi-dimensional perspective. Statistical Methods and Applications, 17, 225-250.
#'
#' Betti, G., Gagliardi, F., Lemmi, A., & Verma, V. (2015). Comparative measures of multidimensional deprivation in the European Union. Empirical Economics, 49(3), 1071-1100.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.
#'
#'
fs_var <- function(data, weight = NULL, ID = NULL, dimensions, HCR,
                   breakdown = NULL, alpha, rho = NULL, type = 'bootstrap_naive',
                   R = 500, M = NULL, stratum, psu, f = 0.01,
                   Xs, total, fixed = FALSE, verbose = TRUE){
  if(!(type %in% c("bootstrap_naive","bootstrap_calibrated", "jackknife"))) stop("Select a variance estimation method from the list:  bootstrap_naive, bootstrap_calibrated, jackknife ")
  if(!is.null(breakdown)) breakdown <- as.factor(breakdown)
  N <- nrow(data)
  if(is.null(M)) M <- nrow(data)
  if(is.null(ID)) ID <- seq_len(N)
  if(is.null(weight)) weight <- rep(N, N)
  if(fixed){
    step2 <- fs_transform(data, weight, ID = ID)
    step3 <- fs_weight(dimensions, step2, rho)
    fs.mf <- fs_construct(step3, weight, alpha, breakdown)$membership
  }
  switch(type,
         bootstrap_naive = {
           BootDistr <- lapply(1:R, function(x) {
             if(verbose == T) cat('Bootstrap Replicate : ', x, 'of', R, '\n')
             bootidx <- sample(1:N, size = M, replace = T)
             ID.boot <- ID[bootidx]
             data.boot <- data[ID.boot,]
             weight.boot <- weight[ID.boot]
             step2.boot <- fs_transform(data.boot, weight.boot)
             step3.boot <- fs_weight(dimensions, step2.boot, rho)
             if(fixed){
               mu.boot <- fs.mf$mu[ID.boot%in%fs.mf$ID]
               weight.boot <- weight[ID.boot%in%fs.mf$ID]
               if(!is.null(breakdown)){
                 breakdown.boot <- breakdown[ID.boot]
                 sapply(fs.mf, function(x) {
                   mu.boot <- x$mu[ID.boot%in%x$ID]
                   weight.boot <- x$weight[ID.boot%in%x$ID]
                   tapply(data.frame(mu.boot, weight.boot, breakdown.boot), ~breakdown.boot, function(x) weighted.mean(x$mu.boot, x$weight.boot))
                 })
               } else {
                 sapply(fs.mf, function(x) {
                   mu.boot <- x$mu[ID.boot%in%x$ID]
                   weight.boot <- x$weight[ID.boot%in%x$ID]
                   weighted.mean(x = mu.boot, w = weight.boot)
                 })
               }
             } else {
               if(!is.null(breakdown)) {
                 breakdown.boot <- breakdown[ID.boot]
                 try(fs_construct(step3.boot, weight.boot, alpha, breakdown.boot)$estimate) # attenzione ai NA, tratto come 0
               } else {
                 try(fs_construct(step3.boot, weight.boot, alpha))$estimate # attenzione ai NA, tratto come 0
               }
             }
           })

           if(!is.null(breakdown)){
             J <- length(unique(breakdown))
             P <- 1+max(dimensions)
             var.array = array(unlist(BootDistr), dim = c(J, P, R),
                               dimnames = list(levels(breakdown),
                                               c(paste0("FS", 1:(P-1)), "Overall"),
                                               NULL))
             var.hat = apply(var.array, 1:2, var, na.rm = TRUE)
             # var.hat <- list(variance = Reduce(modifiedSum, BootDistr)/R)
           } else {
             var.hat <- apply(do.call(rbind, BootDistr), 2, var, na.rm = TRUE)
             # par(mfrow = c(floor((1+max(dimensions))/2), 2))
             # for(i in 1:nrow(BootDistr)) hist(BootDistr, xlab = '', main = paste(rownames(BootDistr)[i], "Bootstrap distribution"), probability = T)
             # var.hat <- apply(BootDistr, 1, var) # decidere se restituire questo o anche la distributzione come sotto
             # var.hat <- list(distribution = BootDistr, variance = apply(BootDistr, 1, var)) #
           }
           var.hat
         },
         bootstrap_calibrated = {
           BootDistr <- lapply(1:R, function(x) {
             if(verbose == T) cat('Bootstrap Replicate : ', x, 'of', R, '\n')
             bootidx <- sample(1:N, size = M, replace = T)
             ID.boot <- ID[bootidx]
             data.boot <- data[ID.boot,]
             weight.boot <- sampling::calib(Xs = Xs, d = weight[bootidx], total = total, method = "linear")
             step2.boot <- fs_transform(data.boot, weight.boot, ID = NULL)
             step3.boot <- fs_weight(dimensions, step2.boot, rho)
             if(fixed){
               mu.boot <- fs.mf$mu[ID.boot%in%fs.mf$ID]
               weight.boot <- weight[ID.boot%in%fs.mf$ID]
               if(!is.null(breakdown)){
                 breakdown.boot <- breakdown[ID.boot]
                 sapply(fs.mf, function(x) {
                   mu.boot <- x$mu[ID.boot%in%x$ID]
                   tapply(data.frame(mu.boot, weight.boot, breakdown.boot), ~breakdown.boot, function(x) weighted.mean(x$mu.boot, x$weight.boot))
                 })
               } else {
                 sapply(fs.mf, function(x) {
                   mu.boot <- x$mu[ID.boot%in%x$ID]
                   weighted.mean(x = mu.boot, w = weight.boot)
                 })
               }
             } else {
               if(!is.null(breakdown)) {
                 breakdown.boot <- breakdown[ID.boot]
                 try(fs_construct(step3.boot, weight.boot, alpha, breakdown.boot)$estimate) # attenzione ai NA, tratto come 0
               } else {
                 try(fs_construct(step3.boot, weight.boot, alpha))$estimate # attenzione ai NA, tratto come 0
               }
             }
           })

           if(!is.null(breakdown)){
             J <- length(unique(breakdown))
             P <- 1+max(dimensions)
             var.array = array(unlist(BootDistr), dim = c(J, P, R),
                               dimnames = list(levels(breakdown),
                                               c(paste0("FS", 1:(P-1)), "Overall"),
                                               NULL))
             var.hat = apply(var.array, 1:2, var, na.rm = TRUE)
             # var.hat <- list(variance = Reduce(modifiedSum, BootDistr)/R)
           } else {
             var.hat <- apply(do.call(rbind, BootDistr), 2, var, na.rm = TRUE)
             # par(mfrow = c(floor((1+max(dimensions))/2), 2))
             # for(i in 1:nrow(BootDistr)) hist(BootDistr, xlab = '', main = paste(rownames(BootDistr)[i], "Bootstrap distribution"), probability = T)
             # var.hat <- apply(BootDistr, 1, var) # decidere se restituire questo o anche la distributzione come sotto
             # var.hat <- list(distribution = BootDistr, variance = apply(BootDistr, 1, var)) #
           }
           var.hat
         },

         jackknife = {
           P <- max(dimensions)
           if(!is.null(breakdown)) J <- length(unique(breakdown))
           tab <- data.frame(table(stratum, psu))
           a <- rowSums(with(tab, table(stratum, psu)))
           if(any(a<2)) stop("There should be at least 2 PSUs in each stratum")
           strata <- unique(stratum)
           H <- length(strata) # can be also a character string then
           w_jh <- tapply(weight, list(stratum, psu), sum, na.rm = T) # sum of the weights inside the strata
           w_h <- rowSums(w_jh) # sum of the weights inside the strata
           z_h = vector(mode = 'list', length = H)
           col.labels <- c(paste0('FS', 1:P), 'Overall')
           var_h <- matrix(0, nrow = H, ncol = P+1); colnames(var_h) <- col.labels
           if(!is.null(breakdown)) var_h <- array(NA, dim = c(H, J, P+1), dimnames = list(NULL, levels(breakdown), col.labels))
           for(h in 1:H){ # for stratum
             if(verbose == T) cat('doing for stratum',h,'of',H,'\n')
             stratum_h <- strata[h]
             psu_h <- tab$psu[tab$stratum==stratum_h] # psu-s in statum h
             a_h <- length(psu_h)
             z_hi <- matrix(0, nrow = a_h, ncol = P+1)
             if(!is.null(breakdown)) z_hi <- array(0, dim = c(J, P+1, a_h))
             g_hi <- rep(0, a_h)
             for(i in 1:a_h){ # for PSUs in stratum h
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
               ID.jack <- ID[delete.idx]
               data.jack <- data[delete.idx,]
               weight.jack <- w[delete.idx]
               step2.jack <- fs_transform(data.jack, weight.jack, ID = NULL)
               step3.jack <- fs_weight(dimensions, step2.jack, rho)
               if(!is.null(breakdown)){
                 breakdown.jack <- breakdown[delete.idx]
                 step7.jack <- fs_construct(step3.jack, weight.jack, alpha, breakdown.jack)$estimate # attenzione ai NA, tratto come 0
                 if(fixed){
                   z_hi[,,i] <- sapply(fs.mf, function(x) {
                     mu.jack <- x$mu[delete.idx]
                     tapply(data.frame(mu.jack, weight.jack, breakdown.jack), ~breakdown.jack, function(x) weighted.mean(x$mu.jack, x$weight.jack))
                   })
                 } else {
                   z_hi[,,i] <- step7.jack
                 }
                 g_hi.mat <- array(g_hi, dim = c(a_h, P+1, J))
               } else {
                 if(fixed){
                   z_hi[i,] <- sapply(fs.mf, function(x) {
                     mu.jack <- x$mu[delete.idx]
                     weighted.mean(x = mu.jack, w = weight.jack)
                   })
                 } else {
                 step7.jack <- fs_construct(step3.jack, weight.jack, alpha)$estimate # attenzione ai NA, tratto come 0
                 z_hi[i,] <- step7.jack
                 }
               }
             }

             if(!is.null(breakdown)){
               # z_hi.bar <- Reduce(modifiedSum, z_hi)/a_h
               z_hi.bar <- array(apply(z_hi, 1:2, mean, na.rm = T), dim = c(J, P+1, a_h), dimnames = list(levels(breakdown),col.labels, NULL ))
               squared.dev <- (z_hi - z_hi.bar)^2
               var_h[h,,] <- Reduce('+', lapply(1:a_h, function(i) g_hi[i]*squared.dev[,,i])) # oppure usare modified sum
             } else {
               z_hi.bar <- apply(z_hi, 2, mean)
               z_hi.bar <- rep(1, a_h)%*%t(z_hi.bar) # matrix of means
               var_h[h,] <- (1-f)*t(g_hi)%*%((z_hi - z_hi.bar)^2)
             }
           }

           if(!is.null(breakdown)){
             var.hat <- apply(var_h, 2:3, sum)
           } else {
             var.hat <- apply(var_h, 2, sum)
           }
         })

  if(!is.null(breakdown)) {
    var.hat <- list(variance = var.hat, size = table(breakdown), type = type)
  } else {
    var.hat <- list(variance = var.hat, type = type)
  }
  var.hat <- FuzzySupplementary(var.hat)
  return(var.hat)
}
