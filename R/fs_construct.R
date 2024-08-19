#' Fuzzy supplementary poverty estimation (Step 7)
#'
#' @description Step 7. Constructs the fuzzy supplementary poverty measure based on Steps1-6.
#'
#' @param steps4_5 The results from `fs_equate`.
#' @param weight A numeric vector of sampling weights of length nrow(step1). if NULL weights will set equal to n (n = sample size)
#' @param alpha The value of the exponent in the FM equation. If NULL it is calculated so that it equates the expectation of the membership function to HCR.
#' @param breakdown A Dimension of sub-domains to calculate estimates for (using the same alpha). If numeric will be coerced to a Dimension.
#'
#' @import dplyr
#' @return An object of class FuzzySupplementary containing the fuzzy membership function for each unit, the point estimate (i.e. the expected value of the function), and the alpha parameter.
#' @export
#'
#' @examples
#'
#' #This example is based on the dataset eusilc included in the package
#' #The FS index is compute without and with breakdown and using an HCR = 0.12
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
#' alpha <- fs_equate(steps4_5 = steps4_5,
#'                    weight = eusilc$DB090, HCR = 0.12,
#'                    interval = c(1,10))
#'
#' #Step 7 the FS index without breakdown
#'
#' fs_results = fs_construct(steps4_5 = steps4_5,
#'              weight = eusilc$DB090, alpha = alpha, breakdown = NULL)
#'
#' #Step 7 the FS index with breakdown
#'
#' fs_results = fs_construct(steps4_5 = steps4_5,
#'              weight = eusilc$DB090, alpha = alpha, breakdown = eusilc$db040)
#' @references
#' Betti, G., Gagliardi, F., Lemmi, A., & Verma, V. (2015). Comparative measures of multidimensional deprivation in the European Union. Empirical Economics, 49(3), 1071-1100.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.
#'

fs_construct <- function(steps4_5, weight, alpha, breakdown = NULL){
  steps4_5 <- steps4_5$steps4_5
  if(!is.null(alpha)) if(alpha < 1) stop("The value of alpha has to be >=1")
  J <- max(steps4_5$Dimension)

  res.list <-vector(mode = 'list', length = J+1)
  headers <- c(paste0('FS', 1:J), 'Overall')
  names(res.list) <- headers

  FS.data <- unique(steps4_5[,c('ID','s_i')])
  if(!is.null(breakdown)) FS.data <- data.frame(unique(steps4_5[,c('ID','s_i')]), breakdown)
  FS.data$weight <- weight # potrebbe essere meglio averla dallo step prima? altrimenti devo aggiungere di nuovo l'opzione in caso uno il peso non ce l'abbia. si il peso se non c'è lo attribuisco prima e me lo porto dietro sempre

  s <- FS.data[['s_i']]

  FS.data.ord <- FS.data %>% dplyr::arrange(s)
  s.ord <- FS.data.ord$s_i
  w.ord <- FS.data.ord$weight

  FS.data.ord$mu <- fs_mu(s.ord, w.ord, alpha)

  res.list[['Overall']] <- FS.data.ord

  for(j in 1:J){
    FS.data <- unique(steps4_5[steps4_5$Dimension==j, c('ID','s_hi')])
    if(!is.null(breakdown)) FS.data <- data.frame(unique(steps4_5[steps4_5$Dimension==j, c('ID','s_hi')]), breakdown)
    FS.data$weight <- weight # potrebbe essere meglio averla dallo step prima? altrimenti devo aggiungere di nuovo l'opzione in caso uno il peso non ce l'abbia

    s <- FS.data[['s_hi']]

    FS.data.ord <- FS.data %>% dplyr::arrange(s)
    s.ord <- FS.data.ord[['s_hi']]
    w.ord <- FS.data.ord$weight

    FS.data.ord$mu <- fs_mu(s.ord, w.ord, alpha)
    res.list[[j]] <- FS.data.ord

  }

  estimate <- sapply(res.list, function(x) weighted.mean(x$mu, x$weight))

  if(!is.null(breakdown)){
    P <- length(unique(breakdown))
    estimate <- sapply(1:(J+1), function(j) sapply(split(res.list[[j]], f = ~ res.list[[j]]$breakdown), function(x) weighted.mean(x$mu, x$weight)))
    colnames(estimate) <- headers
  }

  res <- list( membership = res.list, estimate = estimate, alpha = alpha)
  res <- FuzzySupplementary(res)
  return(res)
}
