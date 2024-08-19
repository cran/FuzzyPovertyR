#' The summary of a FuzzySupplementary object
#' @description Summary method for class "FuzzySupplementary"
#'
#' @param object An object of class "FuzzySupplementary"
#' @param ... Additional options
#'
#' @return The summary method for class "FuzzySupplementary"
#' @export
#'
#' @examples
#'
#' #This example is based on the dataset eusilc included in the package
#' #The summary of FS index is compute with breakdown and using an HCR = 0.12
#'
#' FS <- fs_construct_all(data = eusilc[,4:23], weight = eusilc$DB090, # step 2
#'                        dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5), # step 3
#'                        rho = NULL, # steps 4 and 5
#'                        HCR = .12, # step 6
#'                        breakdown = eusilc$db040) # step 7 with breakdowns
#' summary(FS)
#'
summary.FuzzySupplementary <- function(object,...) {
  if(names(object)[1]=="step2") summary(object$step2)
  if(names(object)[1]=="steps4_5") object$steps4_5 %>% dplyr::ungroup() %>% dplyr::select(Dimension, Item, w_a, w_b, w) %>% unique() %>% print()
  if(names(object)[1]=="membership") {

    cat("Fuzzy supplementary results: \n\n",
        "Summary of the membership functions: \n\n",
        "Quantiles: \n\n")
    writeLines( capture.output( t(sapply(object$membership, function(x) quantile(x$mu))) %>% round(4) ))
    cat("\n Estimate(s): \n\n")
    writeLines( capture.output( round(object$estimate, digits = 3) ) )
    cat("\n Parameter(s): \n\n Alpha: \n\n")
    writeLines(capture.output(  round(unlist(object$alpha), digits = 3)  ))

  }
  if(names(object)[1]=="variance") {

    cat("Variance of Fuzzy supplementary results: \n\n",
        "Type of estimator: \n\n", object$type)

    cat("\n\n Estimate(s): \n\n")
    writeLines( capture.output( round(object$variance, digits = 6) ) )
  }

}

