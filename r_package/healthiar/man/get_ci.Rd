% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ci.R
\name{get_ci}
\alias{get_ci}
\title{Get Monte Carlo confidence intervals}
\usage{
get_ci(
  rr_central = NULL,
  rr_lower = NULL,
  rr_upper = NULL,
  exp_central = NULL,
  exp_lower = NULL,
  exp_upper = NULL,
  cutoff_central = NULL,
  cutoff_lower = NULL,
  cutoff_upper = NULL,
  bhd_central = NULL,
  bhd_lower = NULL,
  bhd_upper = NULL,
  dw_central = NULL,
  dw_lower = NULL,
  dw_upper = NULL,
  erf_shape = NULL,
  erf_increment = NULL,
  erf_eq = NULL,
  prop_pop_exp = NULL,
  approach_risk,
  year_of_analysis = NULL,
  input = NULL,
  health_metric = NULL,
  min_age = NULL,
  max_age = NULL,
  approach_exposure = NULL,
  pop_exp = NULL
)
}
\arguments{
\item{rr_central, rr_lower, rr_upper}{\code{Numeric values} referring to the central estimate of the relative risk and the corresponding lower and upper 95\% confidence interval bounds.}

\item{exp_central, exp_lower, exp_upper}{\code{Numeric values} of the exposure to the environmental stressor referring to the central estimate and (optionally) to lower and upper bound of the 95\% confidence interval. If only one value is provided, it will be assumed that it refers to population-weighted mean exposure in ug/m3. If a {vector} is provided, it will be assumed that it refers to the exposure categories (average exposure in the category) in a exposure distribution (this information is linked to the proportion of population exposed).}

\item{bhd_central, bhd_lower, bhd_upper}{\code{Numeric value} showing the central estimate and (optionally) the lower bound and the upper bound of the confidence interval of the baseline health data (e.g. incidence of the health outcome in the population).}

\item{dw_central, dw_lower, dw_upper}{\code{Numeric value} showing the disability weights (central estimate, lower and upper 95\% confidence intervals) associated with the morbidity health outcome}

\item{erf_shape}{\code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".}

\item{erf_increment}{\code{Numeric value} showing the increment of the exposure-response function in ug/m3 (usually 10 or 5).}

\item{prop_pop_exp}{\code{Numeric value} or {Numeric vector} Fraction (values between 0 & 1) of the total population exposed to (one or more) exposure categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.}

\item{approach_risk}{\code{String} showing the risk risk method. To choose between: "relative_risk" (default) or "absolute_risk".}

\item{year_of_analysis}{\code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.}

\item{health_metric}{\code{String} showing the change in outcome metric to assess attributable health impacts. To choose between "same_input_output" (default), "yld_from_prevalence", "deaths_from_lifetable", "yll_from_lifetable", "yld_from_lifetable" and "daly_from_lifetable".}

\item{min_age}{\code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).}

\item{max_age}{\code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).}

\item{approach_exposure}{\code{String} showing whether air pollution is constant or only in one year. Options: "single_year" (default), "constant"}

\item{pop_exp}{\code{Numeric value} or {vector} showing the population exposed for each of the exposure categories. The length of this input variable must be the same as "exp".}
}
\value{
This function returns a Monte Carlo summary uncertainty for the attributable health impacts.
}
\description{
Determine summary uncertainty based on at least one variable
with central, lower and upper estimate
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
