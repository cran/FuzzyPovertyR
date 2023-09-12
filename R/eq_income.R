#' Fuzzy predicate poverty estimation
#'
#' @description This function takes as input a numeric vector representing a predicate variable and turns it into its equivalised verision
#' using different equivalence scales.
#'
#' @param predicate A numeric vector representing the poverty predicate (i.e. income or expenditure)
#' @param ncomp A numerical vector of the total number of components for the j-th family.
#' @param age A numerical vector of the number of components for the j-th family less than 16 years-old
#' @param scale.eq The equivalence scale. Options are: Carbonaro, n.par (non parametric), OECD7050, modifiedOECD or new
#' @param newscale a data.frame with two columns: "ncomp"  defining the number of components and  "s.eq" that define the corresponding
# value of equivalent people. It is to define only if scale.eq ="new"
#' @import dplyr
#'
#' @return A numeric vector containing the equivalised predicate variable.
#' @export
#'
#' @examples
#'
#' aa=runif(100, 0, 1000) # predicate
#' ncomp=rep(c(1,3,5,7,4),20) #n componenti
#' age16=ncomp-1 #componenti < 16
#' eq_income(predicate=aa, ncomp=ncomp, scale.eq="carbonaro") #carbonaro
#' eq_income(predicate=aa, ncomp=ncomp, scale.eq="n.par") #non-parametric
#' eq_income(predicate=aa, ncomp=ncomp, age=age16, scale.eq="OECD7050") #OECD7050
#' eq_income(predicate=aa, ncomp=ncomp, age=age16, scale.eq="modifiedOECD") #modifiedOECD
#' newscal=data.frame("ncomp"=c(1:9), "s.eq"=runif(9,1,10)) # new
#' ncomp=rep(c(10,3,5,7,4),20)
#' eq_income(predicate=aa, ncomp=ncomp,scale.eq="new", newscale=newscal)
#'

eq_income = function(predicate, ncomp, age, scale.eq,newscale){
  if (scale.eq=="carbonaro"){
    datascale=data.frame("ncomp"=c(1:7), "s.eq"=c(1,1.67,2.23,2.73,3.18,3.61,4.01))
    ncomp[ncomp>7]=7
    eq_income=data.frame("predicate"=predicate, "ncomp"=ncomp)
    eq_income=dplyr::inner_join(eq_income, datascale, by="ncomp")
    eq_income$predicate.eq=eq_income$predicate/eq_income$s.eq
  } else if (scale.eq=="n.par"){
    datascale=data.frame("ncomp"=c(1:7), "s.eq"=c(1,1.69,2.08,2.41,2.92,3.32, 3.32))
    eq_income=data.frame("predicate"=predicate, "ncomp"=ncomp)
    ncomp[ncomp>7]=7
    eq_income=dplyr::inner_join(eq_income, datascale, by="ncomp")
    eq_income$predicate.eq=eq_income$predicate/eq_income$s.eq
  } else if (scale.eq=="OECD7050"){
    eq_income=data.frame("predicate"=predicate, "ncomp"=ncomp, "age"=age, "s.eq"=(1+(ncomp-1-age)*0.7+age*0.5))
    eq_income$predicate.eq= eq_income$predicate/ eq_income$s.eq
  } else if (scale.eq=="modifiedOECD"){
    eq_income=data.frame(  "predicate"=predicate, "ncomp"=ncomp, "age"=age,"s.eq"=(1+(ncomp-1-age)*0.5+age*0.3))
    eq_income$predicate.eq= eq_income$predicate/ eq_income$s.eq
  } else if (scale.eq=="new") {
    ncomp[ncomp>max(newscale$ncomp)]=max(newscale$ncomp)
    datascale=newscale
    eq_income=data.frame("predicate"=predicate, "ncomp"=ncomp)
    eq_income=dplyr::inner_join(eq_income, datascale, by="ncomp")
    eq_income$predicate.eq=eq_income$predicate/eq_income$s.eq
  }
  return(eq_income)
}
