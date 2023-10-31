#' Fuzzy monetary poverty estimation
#'
#' @description Detects and inverts deprivation items for FS
#' @param data a data-set of n columns with the considered items
#' @param vec_order a vector length n with TRUE or FALSE. True if the order of the variable is to be inverted, False otherwise
#'
#' @import dplyr
#' @return A dataset with the same item of data  with inverted order for those with vec_order==TRUE
#' @export
#'
#' @examples #Create data
#' @examples data=data.frame("X"=rep(c(1,2,3,4),20), "Y"=rep(c(7,8,9,1),20))
#' @examples #Crete vec_order
#' @examples vec_order=c(TRUE,FALSE)
#' @examples ##
#' @examples fs_order(data=data, vec_order)
#'
#'

fs_order=function (data, vec_order){
  if (!(all(vec_order) %in% c(TRUE, FALSE))) stop("Incorrect order parameter.")
  for (i in 1:ncol(data)) {

    newdata = matrix(nrow = nrow(data), ncol = ncol(data))

    extract = data %>% dplyr::distinct(data[,i]) %>% dplyr::pull()

    if (vec_order[i] == TRUE) {

      dataex = data.frame("extract" = extract, "new_order" = rev(extract))
    }
    else {
      dataex = data.frame(extract = extract, new_order = extract)
    }
    newdata[, i] = data[, i]
    newdata = as.data.frame(newdata)
    colnames(newdata)[i] = "extract"
    colnames(dataex)[1] = "extract"
    newdata = dplyr::inner_join(newdata, dataex, by = "extract")
    if (i == 1) {
      order_data = newdata[, ncol(data) + 1]
    }   else {
      order_data = cbind(order_data, newdata[, ncol(data) +
                                               1])
    }
    order_data = as.data.frame(order_data)
    colnames(order_data)[i] = colnames(data)[i]
  }
  return(order_data)
}
