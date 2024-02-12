#' The summary of a FuzzyMonetary object
#' @description Summary method for class "FuzzyMonetary"
#'
#' @param object An object of class "FuzzyMonetary"
#' @param ... Additional options
#'
#' @return The summary method for class "FuzzyMonetary"
#' @export
#'
summary.FuzzyMonetary <- function(object,...) {
  res <- object$results$mu; probs = c(0, .2, .4, .6, .8, 1)

  if(!is.null(object$fm)){
  if(object$fm!="ZBM" ) {
    cat("Fuzzy monetary results: \n\n",
        "Summary of", object$fm, "membership function: \n\n",
        "Quantiles: \n\n")
    writeLines( capture.output( rbind(round(quantile(res, probs = probs ), digits = 3) ) %>% as.data.frame(row.names = '' ) ) )
    cat("\n Estimate(s): \n\n")
    writeLines( capture.output( round(object$estimate, digits = 3) ) )
    cat("\n Parameter(s): \n\n")
    writeLines(capture.output(  round(unlist(object$parameters), digits = 3)  ))
  } else if (object$fm=="ZBM") {
        cat("Fuzzy monetary results: \n\n",
            "Summary of", object$fm, "membership function: \n\n",
            "Mean  over the individual states: \n\n")
        writeLines( capture.output( round(mean(rowMeans(object$mu)), digits = 3) ) )
        cat("\n Estimate(s): \n\n")
        writeLines( capture.output( round(object$estimate, digits = 3) ) )
        cat("\n Parameter(s): \n\n")
        writeLines(capture.output(  round(unlist(object$parameters), digits = 3)  ))
  }
   } else{

    cat("Variance of Fuzzy monetary results: \n\n",
        "Type of estimator: \n\n", object$type)

    cat("\n\n Estimate(s): \n\n")
    writeLines( capture.output( round(object$variance, digits = 6) ) )
  }

}

