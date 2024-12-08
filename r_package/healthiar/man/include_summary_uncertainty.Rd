% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/include_summary_uncertainty.R
\name{include_summary_uncertainty}
\alias{include_summary_uncertainty}
\title{Get Monte Carlo confidence intervals}
\usage{
include_summary_uncertainty(results, n_sim)
}
\arguments{
\item{results}{\code{variable} in which the results of an attribute function is stored.}

\item{n_sim}{\code{numeric value} indicating the number of simulations to be performed.}
}
\value{
This function returns confidence intervals for the attributable health impacts using a Monte Carlo simulation.
}
\description{
Determine summary uncertainty (based on at least one variable
with central, lower and upper estimate) based on attribute() or compare()
function output.
}
\note{
Experimental function
}
\examples{
TBD
}
\author{
Axel Luyten
}
\keyword{internal}
