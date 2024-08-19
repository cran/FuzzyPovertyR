#' s3 class fuzzy poverty
#' @export
#'
#' @param x an object
#' @return an object of class FuzzyPoverty
#'
FuzzyPoverty <- function(x){
  class(x) <- "FuzzyMonetary"
  return(x)
}

#' s3 class fuzzy poverty
#'
#' @param x an object
#' @return an object of class FuzzyPoverty
#'
FuzzySupplementary <- function(x){
  class(x) <- "FuzzySupplementary"
  return(x)
}
