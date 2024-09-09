# Title and description

#' Attributable years of life lost based on life tables
#'
#' Calculates the years of life lost attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).
#' @inheritParams attribute
#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval.
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export
attribute_yll_from_lifetable <-
  function(approach_multiexposure = NULL,
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL, erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           approach_exposure = "single_year",
           approach_newborns = "without_newborns",
           first_age_pop, last_age_pop,
           population_midyear_male, population_midyear_female,
           deaths_male = NULL, deaths_female = NULL,
           year_of_analysis,
           corrected_discount_rate = NULL,
           min_age = NULL, max_age = NULL,
           valuation = NULL,
           info = NULL){

    output <-
      bestcost::attribute(
        health_metric = "yll_from_lifetable", # Set outcome metric
        approach_risk = "relative_risk",
        approach_multiexposure = approach_multiexposure,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        pop_exp = NULL,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
        dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
        duration = NULL,
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        first_age_pop = first_age_pop, last_age_pop = last_age_pop,
        population_midyear_male = population_midyear_male, population_midyear_female = population_midyear_female,
        deaths_male = deaths_male, deaths_female = deaths_female,
        year_of_analysis = year_of_analysis,
        min_age = min_age, max_age = max_age,
        corrected_discount_rate = corrected_discount_rate,
        geo_id_raw = NULL, geo_id_aggregated = NULL,
        valuation = valuation,
        info = info)

    return(output)

  }
