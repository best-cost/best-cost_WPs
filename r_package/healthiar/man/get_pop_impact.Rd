% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pop_impact.R
\name{get_pop_impact}
\alias{get_pop_impact}
\title{Get population impact over time}
\usage{
get_pop_impact(
  year_of_analysis,
  input_with_risk_and_pop_fraction,
  outcome_metric,
  min_age
)
}
\arguments{
\item{year_of_analysis}{\code{Numeric value} of the year of analysis, which corresponds to the first year of the life table}

\item{input_with_risk_and_pop_fraction}{\code{Tibble} showing the input data and the PAF (population attributable fraction) or PIF (population impact fraction)}

\item{outcome_metric}{\code{String} to define the outcome metric. Choose between "death", "yll" and "yld"}
}
\value{
This function returns a \code{data.frame} with one row for each value of the
concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
Moreover, the data frame include columns such as:
\itemize{
 \item Attributable fraction
 \item Health impact
 \item Outcome metric
 \item And many more.
}
}
\description{
Get population impact over time
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
