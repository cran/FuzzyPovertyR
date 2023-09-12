#' Head Count Ratio (HCR)
#'
#' @description This function calculates the head count ratio.
#'
#' @details The head count ration is defined as the sum of the sampling weight of statistical units
#' whose vale of the predicate variable is below the poverty line. The poverty line is usually defined as a
#' fraction of a weighted quantile (in official statistics the median) of the predicate distribution
#'
#' @param predicate A numeric vector of a predicate variable (i.e. income or expenditure)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param p The quantile to be calculated from the predicate variable. Default is the median.
#' @param q The percentage of the quantile to be used in determining the poverty line. default is 0.6.
#' @param poverty.line The poverty line. If it is NULL it is estimated from data.
#' @return A list containing the classification of the units as poor (TRUE) and not-poor (FALSE), the estimated Head Count Ratio, and the poverty line.
#' @export
#'
#' @examples
#' N <- 100
#' p <- 0.5
#' q <- 0.6
#' predicate <- rchisq(N, 15) # predicate variable
#' HCR(predicate)
#'
HCR <- function(predicate, weight = NULL, p = 0.5, q = 0.6, poverty.line = NULL) {

  if(is.null(weight)) weight <- rep(1/length(predicate), length(predicate))
  if(is.null(poverty.line)) poverty.line <- q*weighted_quantile(x = predicate, w = weight, p = 0.5)
  poors <- ifelse(predicate <= poverty.line, TRUE, FALSE)
  return(
    list( classification = data.frame(predicate = predicate, poor = poors),
          poverty.line = poverty.line,
          HCR = mean(poors)
               )
    )
}
