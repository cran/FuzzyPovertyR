#' Fuzzy supplementary poverty estimation (Steps 4 and 5)
#'
#' @description Step 4 and Step 5. Calculates the weights of dimensions discovered after Dimension analysis.
#'
#' @details This function calculates the two set of weights w_a and w_b (see References)
#'
#' @param dimensions A numeric vector (of length  `ncol(data)`) of assignments of items in data to dimensions
#' @param step2 The data frame resulting from step2
#' @param rho Optional critical value to be used for calculation of weights in the Kendall correlation matrix. If NULL rho is set equal to the point of largest gap between the ordered set of correlation values encountered (see Betti and Verma, 2008)
#'
#' @return An object of class FuzzySupplementary with calculated weights and deprivation scores in each dimension identified.
#' @export
#'
#' @examples
#' #This example is based on the dataset eusilc included in the package
#' #The step 2-3 are the following (step 1 is the eusilc dataset)
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
#' @references
#' Betti, G., & Verma, V. (2008). Fuzzy measures of the incidence of relative poverty and deprivation: a multi-dimensional perspective. Statistical Methods and Applications, 17, 225-250.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.

fs_weight <- function(dimensions, step2, rho = NULL){
  step2 <- step2$step2
  J <- max(dimensions) # number of identified dimensions
  cor.list <- vector(mode = "list", length = J)

  j <- 1:J

  wb.jh_list <- unlist( lapply( j, wb.jh, step2 = step2, dimensions = dimensions, rho) ) # per ogni dimensione calcolo il peso di ogni indicatore
  wb.jh_df <- data.frame(Item = names(wb.jh_list), w_b = wb.jh_list)

  Items <- colnames(step2)[-1] # elimino la colonna ID, si potrebbe evitare lo step?
  # calcolare i coefficienti di variazione degli (1-s)
  result <- step2 %>%
    tidyr::gather('Item', 's', -'ID') %>%
    dplyr::inner_join(data.frame(Item = Items, Dimension = dimensions), by = 'Item') %>%
    dplyr::group_by(Dimension, Item) %>% # raggruppo per item, dimensione
    dplyr::mutate(w_a = sd(s) / mean(s) ) %>%
    dplyr::inner_join(wb.jh_df, by = 'Item') %>% # aggiungo i pesi dallo step prima
    dplyr::mutate(w = w_a*w_b) %>%
    dplyr::group_by(Dimension, ID) %>%
    dplyr::mutate(s_hi = weighted.mean(s, w = w)) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(s_i =  mean(s_hi)) # CONTROLLARE FORMULA! CON P1080 BETTI EMPIRICAL ECONOMICS

  steps4_5 <- FuzzySupplementary(list(steps4_5 = result))

  return(steps4_5)
}
