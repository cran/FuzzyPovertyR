#' Fuzzy supplementary poverty estimation, finding the alpha parameter (step 6)
#'
#' @description Step 6. This function solves $E(mu)^(alpha-1) = HCR$ for alpha.
#'
#' @param steps4_5 The results obtained from `fs_weight`.
#' @param weight A numeric vector of sampling weights. if NULL weights will set equal to n (n = sample size)
#' @param HCR The value of the head count ratio used to compute alpha so that the membership function equals the HCR
#' @param interval The range to look for the value of alpha.
#' @param verbose Logical. whether to print the proceeding of the procedure.
#'
#' @return The alpha parameter that solves the non-linear equation $E(mu) = HCR$
#' @export
#'
#' @examples
#' #This example is based on the dataset eusilc included in the package
#' #The Step 6 of the FS index is computed
#' #The step 2-5 are the following (step 1 is the eusilc dataset)
#' #For more on each step see the ad hoc function included in the package
#'
#' #Step 2
#'
#' step2 = fs_transform(eusilc[,4:23], weight = eusilc$DB090, ID = eusilc$ID)
#'
#' #Step 3 is the definition of the dimension.
#' #For more about the step see Betti et al. (2018)
#'
#' dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)
#'
#' #Step 4-5 finding weights
#'
#' steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL)
#'
#' #Step 6 computation of alpha parameter
#'
#' fs_equate(steps4_5 = steps4_5,
#'           weight = eusilc$DB090,
#'           HCR = 0.12, interval = c(1,10))
#' @references
#' Betti, G., Gagliardi, F., Lemmi, A., & Verma, V. (2015). Comparative measures of multidimensional deprivation in the European Union. Empirical Economics, 49(3), 1071-1100.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.
#'
#'
fs_equate <- function(steps4_5, weight, HCR, interval = c(1,10), verbose = TRUE){ # weight has to be attached to the data frame. should not be specified each time.
  steps4_5 <- steps4_5$steps4_5
  if(is.null(weight)) weight <- nrow(steps4_5)
  FS.data <- unique(steps4_5[,c('ID','s_i')])
  FS.data$weight <- weight # potrebbe essere meglio averla dallo step prima? altrimenti devo aggiungere di nuovo l'opzione in caso uno il peso non ce l'abbia.

  s <- FS.data[['s_i']]

  FS.data.ord <- FS.data %>% dplyr::arrange(s)
  s.ord <- FS.data.ord$s_i
  w.ord <- FS.data.ord$weight

  alpha <- uniroot(fs_objective,
                   interval = interval,
                   s.ord = s.ord,
                   w.ord = w.ord,
                   HCR = HCR,
                   verbose)$root
  if(verbose) cat('Done.\n')
  return(alpha)

}
