#' Compare attributable health cases based on single baseline health value and relative risk

#' @description Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @inheritParams compare
#'
#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
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
#' @note Experimental function
#' @export
compare_health <-
  function(approach_multiexposure = NULL,
           approach_comparison = "delta",
           approach_risk = "relative_risk",
           exp_central_1, exp_lower_1 = NULL, exp_upper_1 = NULL,
           exp_central_2, exp_lower_2 = NULL, exp_upper_2 = NULL,
           prop_pop_exp_1 = 1,
           prop_pop_exp_2 = 1,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           bhd_central_1 = NULL, bhd_lower_1 = NULL, bhd_upper_1 = NULL,
           bhd_central_2 = NULL, bhd_lower_2 = NULL, bhd_upper_2 = NULL,
           population_1 = NULL, population_2 = NULL,
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           info_1 = NULL, info_2 = NULL){



    output <-
      healthiar::compare(
        approach_multiexposure = approach_multiexposure,
        approach_comparison = approach_comparison,
        health_metric = "same_input_output",
        approach_risk = approach_risk,
        exp_central_1, exp_lower_1 = exp_lower_1, exp_upper_1 = exp_upper_1,
        exp_central_2, exp_lower_2 = exp_lower_2, exp_upper_2 = exp_upper_2,
        prop_pop_exp_1 = prop_pop_exp_1,
        prop_pop_exp_2 = prop_pop_exp_2,
        cutoff_central = cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central_1 = bhd_central_1, bhd_lower_1 = bhd_lower_1, bhd_upper_1 = bhd_upper_1,
        bhd_central_2 = bhd_central_2, bhd_lower_2 = bhd_lower_2, bhd_upper_2 = bhd_upper_2,
        approach_exposure_1 = NULL,
        approach_newborns_1 = NULL,
        first_age_pop_1 = NULL, last_age_pop_1 = NULL,
        population_midyear_male_1 = NULL, population_midyear_female_1 = NULL,
        year_of_analysis_1 = NULL,
        approach_exposure_2 = NULL,
        approach_newborns_2 = NULL,
        first_age_pop_2 = NULL, last_age_pop_2 = NULL,
        population_midyear_male_2 = NULL, population_midyear_female_2 = NULL,
        year_of_analysis_2 = NULL,
        min_age = NULL, max_age = NULL,
        dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
        duration_central = NULL,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        population_1 = population_1, population_2 = population_2,
        info_1 = info_1 , info_2 = info_2)


    return(output)
  }
