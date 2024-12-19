#' Attribute health impact from the same health metric and using on relative risk

#' @description It calculates the health impacts, mortality or morbidity,
#' of exposure to an environmental stressor based on the same health metric
#' (in the baseline health data as in the result) and using relative risk.

#' @inheritParams attribute

#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame includes columns such as:
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
#' @export

attribute_health <-
  function(approach_risk = "relative_risk",
           exp_central,
           exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           geo_id_raw = NULL, geo_id_aggregated = NULL,
           approach_multiexposure = NULL,
           population = NULL,
           info = NULL){

    output <-
      healthiar::attribute(
        health_metric = "same_input_output",
        approach_risk = approach_risk,
        approach_multiexposure = approach_multiexposure,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        population = population,
        approach_exposure = NULL,
        approach_newborns = NULL,
        first_age_pop = NULL, last_age_pop = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        year_of_analysis = NULL,
        min_age = NULL, max_age = NULL,
        dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
        duration_central = NULL,
        geo_id_raw = geo_id_raw , geo_id_aggregated = geo_id_aggregated,
        info = info)

    return(output)


  }
