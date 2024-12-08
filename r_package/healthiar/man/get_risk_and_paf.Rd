% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_risk_and_paf.R
\name{get_risk_and_paf}
\alias{get_risk_and_paf}
\title{Get input data and PAF}
\usage{
get_risk_and_paf(input)
}
\arguments{
\item{input}{\code{Data frame} with the input data}
}
\value{
This function returns a \code{data.frame} with the input data adding a column for the population attributable fraction
Moreover, the data frame includes columns such as:
\itemize{
 \item Attributable fraction
 \item Health impact
 \item Outcome metric
 \item And many more.
}
}
\description{
Calculates the population attributable fraction (PAF) based on the input data and puts the results in additional columns joined to the input data frame.
}
\note{
Experimental function
}
\examples{
TBD
}
\author{
Alberto Castro
}
\keyword{internal}
