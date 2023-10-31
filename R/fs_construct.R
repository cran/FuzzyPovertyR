#' Fuzzy supplementary poverty estimation.
#'
#' @description Step 7. Constructs the fuzzy supplementary poverty measure based on Steps1-6.
#'
#' @param steps4_5 The results from `fs_equate`.
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used
#' @param alpha The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR.
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha). If numeric will be coherced to a factor.
#'
#' @import dplyr
#' @return A list of results containing the fuzzy meambership function for each unit, the point estimate (i.e. the expected value of the function), and the alpha parameter.
#' @export
#'
#' @examples
#' data(eusilc)
#' step2 = fs_transform(eusilc[,4:23], weight = eusilc$DB090, ID = eusilc$ID)
#' dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)
#' steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL)
#' alpha <- fs_equate(steps4_5 = steps4_5,
#' weight = eusilc$DB090, HCR = .16, interval = c(1,10))
#'
#' fs_results = fs_construct(steps4_5 = steps4_5,
#' weight = eusilc$DB090, alpha = alpha, breakdown = NULL)
#'
#' fs_results = fs_construct(steps4_5 = steps4_5,
#' weight = eusilc$DB090, alpha = alpha, breakdown = eusilc$db040)

fs_construct <- function(steps4_5, weight, alpha, breakdown = NULL){
  if(!is.null(alpha)) if(alpha < 1) stop("The value of alpha has to be >=1")
  J <- max(steps4_5$Factor)

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
    FS.data <- unique(steps4_5[steps4_5$Factor==j, c('ID','s_hi')])
    if(!is.null(breakdown)) FS.data <- data.frame(unique(steps4_5[steps4_5$Factor==j, c('ID','s_hi')]), breakdown)
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


  return(list( membership = res.list, estimate = estimate, alpha = alpha))
}
