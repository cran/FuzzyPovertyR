#' Fuzzy supplementary poverty estimation
#'
#' @description Step 2. This function maps a set of answers to binary or categorical items to the (0,1) interval.
#'
#' @details
#' The function calculates deprivation score.
#' To obtain consistent measures of supplementary poverty it is important that items are in the right order.
#' Lower levels of the items have to correspond to more deprivation while higher levels of the items to a less deprivation.
#'
#' @param data A matrix or a data frame of identified items (see Step 1 of Betti et. al, 2018)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param depr.score The deprivation score to be used (see d or s in Betti et al (2018))
#' @param ... other parameters
#'
#' @return An object of class FuzzySupplementary containing a matrix of the same dimension of `data` with items mapped into the (0,1) interval
#' @export
#'
#' @examples
#' data(eusilc)
#' step2 = fs_transform(eusilc[,4:23], weight = eusilc$DB090, ID = eusilc$ID)
#'
#' @references
#' Betti, G., Gagliardi, F., Lemmi, A., & Verma, V. (2015). Comparative measures of multidimensional deprivation in the European Union. Empirical Economics, 49(3), 1071-1100.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.
#'
fs_transform = function(data, weight = NULL, ID = NULL, depr.score = "s", ...) {

  N <- nrow(data)
  if(is.null(ID)) ID <- seq_len(N)
  if(is.null(weight)) weight <- rep(N,N)
  deprivation_scores <- apply(data, 2, fuzzyScaleItem, weight, ID)
  transformed_items <- data.frame( ID, do.call(cbind, lapply(deprivation_scores, function(x) x[[depr.score]]) ), row.names = ID )
  transformed_items <- FuzzySupplementary(list(step2 = transformed_items))
  return(transformed_items)

}
