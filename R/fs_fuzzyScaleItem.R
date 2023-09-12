#' Fuzzy supplementary poverty estimation
#'
#' @description Step 2. It calculates deprivation score for item j and ind. i as $d_{j,i} = (1-F(c_{j,i})) / (1-F(1)) where F(c_{j,i})$ is the value of j-th item cumulation function for the i individual
#' @param item A factor or numeric vector representing answers to an item (a column of data) that has to be rescaled. it will be converted to an ordered factor.
#' @param weight A vector of sampling weights. If it is NULL (the default) weights are assigned assuming simple random sampling of units.
#' @param ID A vector of length `nrow(data)` containing individuals IDs. if NULL (the default) row numbers will be used.
#' @param ... other parameters
#'
#' @import dplyr
#' @return The item rescaled according to Betti et. al 2018.
#'
#'
#' @references
#' #' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.

fuzzyScaleItem = function(item, weight, ID, ...){
  ordered_item = factor(item, ordered = T)

  weight_sum = sum(weight)

  # compute weights
  outW <- data.frame(ID = 1:length(ordered_item), # ID riga
                     ordered_item,
                     weight = weight) %>%
    dplyr::arrange(ordered_item) %>%
    dplyr::group_by(ordered_item) %>%
    dplyr::mutate(f = sum(weight)/weight_sum) # per ogni livello di item sommo i pesi e divido per la somma dei pesi

  tmp <- unique(data.frame(ordered_item = outW$ordered_item,
                           f = outW$f))

  tmp <- data.frame(tmp, F_cum = cumsum(tmp$f))

  # join weights with individual information
  outW2 = outW %>%
    dplyr::inner_join(tmp %>% select(ordered_item, F_cum), by = c("ordered_item" = "ordered_item")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(s = round((1 - F_cum) / (1 - min(F_cum)),5), # changed to ensure comparability with betti due to R factor coding
                  d = 1 - s,
                  Item = item) %>%
    # filter(item != Weight) %>% # PERCHé QUESTO PASSAGGIO?
    dplyr::arrange(ID) %>% dplyr::select(ID,
                                         Item,
                                         Item_level = ordered_item,
                                         F_cum, # POSSO ANCHE OMETTERE
                                         d,s )

  return(outW2)

}
