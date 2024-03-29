% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_var.R
\name{fm_var}
\alias{fm_var}
\title{Fuzzy monetary poverty estimation}
\usage{
fm_var(
  predicate,
  weight,
  fm,
  ID = NULL,
  type = "bootstrap",
  R = 100,
  M = NULL,
  stratum,
  psu,
  f = 0.01,
  verbose = FALSE,
  HCR,
  interval = c(1, 10),
  alpha = NULL,
  hh.size,
  z_min,
  z_max,
  z1,
  z2,
  b,
  z,
  breakdown = NULL,
  data = NULL
)
}
\arguments{
\item{predicate}{A numeric vector representing the poverty predicate (i.e. income or expenditure)}

\item{weight}{A numeric vector of sampling weights. if NULL simple random sampling weights will be used.}

\item{fm}{the type of membership function to use}

\item{ID}{A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.}

\item{type}{The variance estimation method chosen. One between \code{bootstrap} (default) or \code{jackknife}.}

\item{R}{The number of bootstrap replicates. Default is 500.}

\item{M}{The size of bootstrap samples. Default is \code{nrow(data)}.}

\item{stratum}{The vector identifying the stratum (if 'jackknife' is chosen as variance estimation technique).}

\item{psu}{The vector identifying the psu (if 'jackknife' is chosen as variance estimation technique).}

\item{f}{The finite population correction fraction (if 'jackknife' is chosen as variance estimation technique).}

\item{verbose}{Logical. whether to print the proceeding of the variance estimation procedure.}

\item{HCR}{If fm="verma" or fm="verma1999" or fm="TFR" . The value of the head count ratio.}

\item{interval}{If fm="verma" or fm="verma1999" or fm="TFR". A numeric vector of length two to look for the value of alpha (if not supplied).}

\item{alpha}{If fm="verma" or fm="verma1999" or fm="TFR". The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR}

\item{hh.size}{If fm="ZBM". A numeric vector of household size.}

\item{z_min}{A parameter of the membership function if fm="belhadj2011"}

\item{z_max}{A parameter of the membership function if fm="belhadj2011"}

\item{z1}{A parameter of the membership function if fm="belhadj2015" or fm="cerioli"}

\item{z2}{A parameter of the membership function if fm="belhadj2015" or fm="cerioli"}

\item{b}{A parameter of the membership function if fm="belhadj2015". The shape parameter (if b=1 the mf is linear between z1 and z2)}

\item{z}{A parameter of the membership function if fm="chakravarty".}

\item{breakdown}{A factor of sub-domains to calculate estimates for (using the same alpha). If numeric will be coerced to a factor.}

\item{data}{an optional data frame containing the variables to be used.}
}
\value{
An object of class FuzzyMonetary containing the estimate of variance with the method selected. if breakdown is not NULL, the variance is estimated for each sub-domain.
}
\description{
This function estimates the variance of the fuzzy monetary poverty index
}
\examples{
data(eusilc)
HCR <- 0.14
hh.size <- rep(1, 1000)
fm_var(predicate = eusilc$eq_income, weight = eusilc$DB090,
fm = "verma", breakdown = eusilc$db040, type = "bootstrap", HCR = .14, alpha = 9)

}
