#' Calculation of Equivalized Poverty Predicate
#'
#' @description This function takes as input a numeric vector representing a predicate variable and turns it into its equivalised version
#' using different equivalence scales.
#'
#' @param predicate A numeric vector  (or the variable name) representing the poverty predicate (i.e. income or expenditure)
#' @param ncomp A numerical vector (or the variable name) of the total number of components for the j-th family.
#' @param age A numerical vector (or the variable name) of the number of components for the j-th family less than 16 years-old
#' @param scale.eq The equivalence scale. Options are: "carbonaro", "n.par" (non parametric), "OECD7050", "modifiedOECD" (Default) or "new"
#' @param newscale a data.frame with two columns: "ncomp"  defining the number of components and  "s.eq" that define the corresponding
# value of equivalent people. It is to define only if scale.eq ="new"
#' @param data An optional data frame containing the variables to be used
#' @import dplyr
#'
#' @return A data.frame containing the equivalised predicate variable.
#' @export
#'
#' @examples
#'
#'
#' #Using OECD scale
#'
#' eq_predicate(predicate = "HY022", ncomp = "ncomp", age = "age16",
#'              scale.eq = "OECD7050", data = eusilc) #OECD7050
#'
#' eq_predicate(predicate = "HY022", ncomp = "ncomp", age = "age16",
#'              scale.eq = "modifiedOECD", data = eusilc) #modifiedOECD
#'
#' #Define a new scale
#'
#' newscal <- data.frame("ncomp" = c(1:7), "s.eq" = runif(7,1,10) ) # new
#'
#'
#' eq_predicate(predicate = "HY022", ncomp = "ncomp", scale.eq = "new",
#'              newscale = newscal, data = eusilc)
#'
#' @references
#'
#' Bernini, C., Emili, S., & Ferrante, M. R. (2024). Regional disparities in the sensitivity of wellbeing to poverty measures. In Spatial Inequalities and Wellbeing (pp. 136-157). Edward Elgar Publishing.
#'
#' Betti, G. (1999). Nonparametric equivalence scales with application to Poland. Statistics Research Report.
#'
#' Chanfreau, J., & Burchardt, T. (2008). Equivalence scales: rationales, uses and assumptions. Edinburgh: Scottish Government.
#'

eq_predicate = function(predicate,
                        ncomp,
                        age = NULL,
                        scale.eq = "modifiedOECD",
                        newscale,
                        data = NULL) {
  if (!(scale.eq %in% c("carbonaro", "n.par", "OECD7050", "modifiedOECD", "new")))
    stop(
      "Select a scale of equivalence from the list:  carbonaro, n.par, OECD7050, modifiedOECD, new"
    )

  if (scale.eq  %in%  c("carbonaro", "n.par", "new")) {
    if (!is.null(data)) {
      predicate <- data[[predicate]]
      ncomp <- data[[ncomp]]
    }
  } else {
    if (!is.null(data)) {
      predicate <- data[[predicate]]
      ncomp <- data[[ncomp]]
      age <- data[[age]]
    }
  }

  if (scale.eq == "carbonaro") {
    datascale <- data.frame(
      "ncomp" = c(1:7),
      "s.eq" = c(0.60, 1.00, 1.33, 1.63, 1.90, 2.16, 2.40)
    )
    ncomp[ncomp > 7] <- 7
    eq_predicate <- data.frame("predicate" = predicate, "ncomp" = ncomp)
    eq_predicate <- dplyr::inner_join(eq_predicate, datascale, by = "ncomp")
    eq_predicate$predicate.eq <- eq_predicate$predicate / eq_predicate$s.eq

  } else if (scale.eq == "n.par") {
    datascale <- data.frame(
      "ncomp" = c(1:7),
      "s.eq" = c(0.55, 1.00, 1.20, 1.42, 1.67, 1.84, 1.84)
    )
    eq_predicate <- data.frame("predicate" = predicate, "ncomp" = ncomp)
    ncomp[ncomp > 7] <- 7
    eq_predicate <- dplyr::inner_join(eq_predicate, datascale, by = "ncomp")
    eq_predicate$predicate.eq <- eq_predicate$predicate / eq_predicate$s.eq

  } else if (scale.eq == "OECD7050") {
    eq_predicate <- data.frame(
      "predicate" = predicate,
      "ncomp" = ncomp,
      "age" = age,
      "s.eq" = (1 + (ncomp - 1 - age) * 0.7 + age * 0.5)
    )
    eq_predicate$predicate.eq <- eq_predicate$predicate / eq_predicate$s.eq

  } else if (scale.eq == "modifiedOECD") {
    eq_predicate <- data.frame(
      "predicate" = predicate,
      "ncomp" = ncomp,
      "age" = age,
      "s.eq" = (1 + (ncomp - 1 - age) * 0.5 + age * 0.3)
    )
    eq_predicate$predicate.eq <- eq_predicate$predicate / eq_predicate$s.eq

  } else if (scale.eq == "new") {
    ncomp[ncomp > max(newscale$ncomp)] <- max(newscale$ncomp)
    datascale <- newscale
    eq_predicate <- data.frame("predicate" = predicate, "ncomp" = ncomp)
    eq_predicate <- dplyr::inner_join(eq_predicate, datascale, by = "ncomp")
    eq_predicate$predicate.eq <- eq_predicate$predicate / eq_predicate$s.eq

  }

  eq_predicate <- round(eq_predicate, digits = 3)

  return(eq_predicate)
}
