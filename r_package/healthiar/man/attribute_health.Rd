% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attribute_health.R
\name{attribute_health}
\alias{attribute_health}
\title{Attribute health impact from the same health metric and using on relative risk}
\usage{
attribute_health(
  approach_risk = "relative_risk",
  exp_central,
  exp_lower = NULL,
  exp_upper = NULL,
  prop_pop_exp = 1,
  cutoff_central = NULL,
  cutoff_lower = NULL,
  cutoff_upper = NULL,
  rr_central = NULL,
  rr_lower = NULL,
  rr_upper = NULL,
  erf_increment = NULL,
  erf_shape = NULL,
  erf_eq_central = NULL,
  erf_eq_lower = NULL,
  erf_eq_upper = NULL,
  bhd_central = NULL,
  bhd_lower = NULL,
  bhd_upper = NULL,
  geo_id_raw = NULL,
  geo_id_aggregated = NULL,
  approach_multiexposure = NULL,
  population = NULL,
  info = NULL
)
}
\arguments{
\item{approach_risk}{\code{String} showing the risk risk method. To choose between: "relative_risk" (default) or "absolute_risk".}

\item{exp_central, exp_lower, exp_upper}{\code{Numeric values} of the exposure to the environmental stressor referring to the central estimate and (optionally) to lower and upper bound of the 95\% confidence interval. If only one value is provided, it will be assumed that it refers to population-weighted mean exposure in ug/m3. If a {vector} is provided, it will be assumed that it refers to the exposure categories (average exposure in the category) in a exposure distribution (this information is linked to the proportion of population exposed).}

\item{prop_pop_exp}{\code{Numeric value} or {Numeric vector} Fraction (values between 0 & 1) of the total population exposed to (one or more) exposure categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.}

\item{rr_central, rr_lower, rr_upper}{\code{Numeric values} referring to the central estimate of the relative risk and the corresponding lower and upper 95\% confidence interval bounds.}

\item{erf_increment}{\code{Numeric value} showing the increment of the exposure-response function in ug/m3 (usually 10 or 5).}

\item{erf_shape}{\code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".}

\item{erf_eq_central, erf_eq_lower, erf_eq_upper}{\code{String} or \code{function} referring to the equation of the user-defined exposure-response function. If a \code{string} is entered, the function must contains only one variable: x (exposure). E.g. "3+x+x^2". If a \code{function} is entered, it has to have a function class. If only the values of the x-axis (exposure) and y axis (relative risk) of the dots in the exposure-response function are available, a cubic spline natural interpolation can be assumed to get the function using, e.g., \code{stats::splinefun(x, y, method="natural")}}

\item{bhd_central, bhd_lower, bhd_upper}{\code{Numeric value} showing the central estimate and (optionally) the lower bound and the upper bound of the confidence interval of the baseline health data (e.g. incidence of the health outcome in the population).}

\item{geo_id_raw}{\code{Vector} showing the id code of the each geographic area considered in the assessment. If a vector is entered here, the data for each geographical area have to be provided as list in the corresponding arguments.}

\item{geo_id_aggregated}{\code{Vector} showing the id code of the geographic area for which raw geo ids have to be aggregated. The vector has to have the same length as geo_id_raw. Therefore, geo_id_aggregated should have duplicated values for those geo_id_r}

\item{approach_multiexposure}{\code{String} showing the approach that has to be used in assessments with multiple exposures. To choose among: "additive", "multiplicative" or "combined".}

\item{population}{code{Vector} with numeric values referring to the population in the geographical unit}

\item{info}{\code{String} or {data frame} showing additional information or id. The suffix "info" will be added to the column name. Default value = NULL.}
}
\value{
TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
concentration-response function i.e. central, lower and upper bound confidence interval.
Moreover, the data frame includes columns such as:
\itemize{
 \item Attributable fraction
 \item Health impact
 \item Outcome metric
 \item And many more.
}
}
\description{
It calculates the health impacts, mortality or morbidity,
of exposure to an environmental stressor based on the same health metric
(in the baseline health data as in the result) and using relative risk.
}
\examples{
TBD
}
\author{
Alberto Castro
}
