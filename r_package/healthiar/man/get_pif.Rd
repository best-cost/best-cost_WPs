% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pif.R
\name{get_pif}
\alias{get_pif}
\title{Get population impact fraction}
\usage{
get_pif(rr_conc_1, rr_conc_2, prop_pop_exp_1, prop_pop_exp_2)
}
\arguments{
\item{rr_conc_1}{\code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 1. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.}

\item{rr_conc_2}{\code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 2. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.}

\item{prop_pop_exp_1}{\code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).}

\item{prop_pop_exp_2}{\code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).}
}
\value{
This function returns a \code{value} corresponding to the population attributable fraction
}
\description{
Calculates the population impact fraction of a health outcome due to exposure to an environmental stressor
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
