#' Fuzzy supplementary poverty estimation (all steps)
#'
#' @description Step 1-7. Constructs the fuzzy supplementary poverty measure based without step-by-step functions.
#'
#' @param data A matrix or a data frame of identified items (see Step 1 of Betti et. al, 2018)
#' @param weight A numeric vector of sampling weights. if NULL weights will set equal to n (n = sample size)
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence
#' @param dimensions A numeric vector (of length  `ncol(data)`) of assignments of items in data to dimensions
#' @param rho Optional critical value to be used for calculation of weights in the Kendall correlation matrix. If NULL rho is set equal to the point of largest gap between the ordered set of correlation values encountered (see Betti and Verma, 2008)
#' @param HCR The value of the head count ratio used to compute alpha so that the expected value of the membership function equals HCR
#' @param interval A numeric vector of length two to look for the value of alpha (if not supplied)
#' @param alpha The value of the exponent in equations of "verma", "verma1999" and "TFR". If NULL it is calculated so that it equates the expectation of the membership function to HCR.
#' @param breakdown A Dimension of sub-domains to calculate estimates for (using the same alpha). If numeric will be coerced to a Dimension
#'
#' @return An object of class FuzzySupplementary containing the fuzzy membership function for each unit, the point estimate (i.e. the expected value of the function), and the alpha parameter.
#' @export
#'
#' @examples
#'
#' #This example is based on the dataset eusilc included in the package
#' #The FS index is compute with breakdown and using an HCR = 0.12
#' #with summary and plot
#'
#' FS <- fs_construct_all(data = eusilc[,4:23], weight = eusilc$DB090, # step 2
#'                        dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5), # step 3
#'                        rho = NULL, # steps 4 and 5
#'                        HCR = .12, # step 6
#'                        breakdown = eusilc$db040) # step 7 with breakdowns
#' summary(FS)
#' plot(FS)
#'
#' @references
#'
#'  Betti, G., & Verma, V. (2008). Fuzzy measures of the incidence of relative poverty and deprivation: a multi-dimensional perspective. Statistical Methods and Applications, 17, 225-250.
#'
#'  Betti, G., Gagliardi, F., Lemmi, A., & Verma, V. (2015). Comparative measures of multidimensional deprivation in the European Union. Empirical Economics, 49(3), 1071-1100.
#'
#'  Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.
#'
#'
fs_construct_all <- function(data, weight = NULL, ID = NULL,
                             dimensions, rho = NULL, HCR,
                             interval = c(1,10),
                             alpha = NULL, breakdown = NULL) {

  step2 <- fs_transform(data = data, weight = weight, ID = ID)
  steps4_5 <- fs_weight(dimensions, step2 = step2, rho = rho)
  if(is.null(alpha)) alpha <- fs_equate(steps4_5 = steps4_5, weight = weight, HCR = HCR, interval = interval)
  step7 <- fs_construct(steps4_5 = steps4_5, weight = weight, alpha = alpha, breakdown = breakdown)

  return(step7)

}



