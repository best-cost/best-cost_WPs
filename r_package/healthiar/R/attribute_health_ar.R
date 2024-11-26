#' Attributable health cases based on absolute risk
#'
#' @description Calculates the health impacts, of an environmental stressor
#' (e.g. noise) using the absolute risk instead of the relative risk
#' @inheritParams attribute
#' @return
#' TBD. E.g. This function returns a \code{list} with two \code{data.frames}, one with the total health impact and the second one with a row for each category of the exposure distribution.
#' The data frame include columns such as:
#' \itemize{
#'  \item TBD
#' }
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

attribute_health_ar <-
  function(exp_central, exp_lower = NULL, exp_upper = NULL,
           pop_exp,
           erf_eq_central, erf_eq_lower = NULL, erf_eq_upper = NULL,
           geo_id_raw = NULL, geo_id_aggregated = NULL,
           info = NULL,
           summary_uncertainty = NULL){

    output <-
      healthiar::attribute(
        health_metric = "same_input_output",
        approach_multiexposure = NULL,
        approach_risk = "absolute_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = NULL,
        pop_exp = pop_exp,
        cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
        rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
        erf_increment = NULL,
        erf_shape = NULL,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
        approach_exposure = NULL,
        approach_newborns = NULL,
        first_age_pop = NULL, last_age_pop = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        year_of_analysis = NULL,
        min_age = NULL, max_age = NULL,
        corrected_discount_rate = NULL,
        dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
        duration_central = NULL,
        geo_id_raw = geo_id_raw , geo_id_aggregated = geo_id_aggregated,
        info = info,
        summary_uncertainty = summary_uncertainty)

    return(output)
  }
