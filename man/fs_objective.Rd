% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fs_objective.R
\name{fs_objective}
\alias{fs_objective}
\title{Fuzzy supplementary poverty estimation}
\usage{
fs_objective(s.ord, w.ord, alpha, HCR, verbose)
}
\arguments{
\item{s.ord}{A vector of ordered deprivation scores.}

\item{w.ord}{A vector of ordered sampling weights. In the same order of \code{s.ord}.}

\item{alpha}{The alpha parameter.}

\item{HCR}{The head count ratio.}

\item{verbose}{Logical. whether to print the proceeding of the procedure.}
}
\value{
The difference between the expected value of the membership function and the head count ratio.
}
\description{
The objective function to find the root of.
}
