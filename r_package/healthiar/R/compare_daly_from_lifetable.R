#' Compare attributable disability-adjusted lived years applying a life table approach

#' @description Calculates the year lived with disability between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
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
compare_daly_from_lifetable <-
  function(approach_multiexposure = NULL,
           approach_comparison = "delta",
           exp_central_1, exp_lower_1 = NULL, exp_upper_1 = NULL,
           exp_central_2, exp_lower_2 = NULL, exp_upper_2 = NULL,
           prop_pop_exp_1 = 1,
           prop_pop_exp_2 = 1,
           cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           approach_exposure_1 = "single_year",
           approach_newborns_1 = "without_newborns",
           first_age_pop_1, last_age_pop_1,
           deaths_male_1 = NULL,
           deaths_female_1 = NULL,
           population_midyear_male_1, population_midyear_female_1,
           year_of_analysis_1,
           approach_exposure_2 = "single_year",
           approach_newborns_2 = "without_newborns",
           first_age_pop_2, last_age_pop_2,
           deaths_male_2 = NULL,
           deaths_female_2 = NULL,
           population_midyear_male_2, population_midyear_female_2,
           year_of_analysis_2,
           min_age = NULL, max_age = NULL,
           dw_central, dw_lower = NULL, dw_upper = NULL,
           duration_central, duration_lower = NULL, duration_upper = NULL,
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           info_1 = NULL, info_2 = NULL){



    output <-
      healthiar::compare(
        approach_multiexposure = approach_multiexposure,
        approach_comparison = approach_comparison,
        health_metric = "daly_from_lifetable",
        approach_risk = "relative_risk",
        exp_central_1, exp_lower_1 = exp_lower_1, exp_upper_1 = exp_upper_1,
        exp_central_2, exp_lower_2 = exp_lower_2, exp_upper_2 = exp_upper_2,
        prop_pop_exp_1 = prop_pop_exp_1,
        prop_pop_exp_2 = prop_pop_exp_2,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central_1 = NULL, bhd_lower_1 = NULL, bhd_upper_1 = NULL,
        bhd_central_2 = NULL, bhd_lower_2 = NULL, bhd_upper_2 = NULL,
        approach_exposure_1 = approach_exposure_1,
        approach_newborns_1 = approach_newborns_1,
        first_age_pop_1 = first_age_pop_1, last_age_pop_1 = last_age_pop_1,
        deaths_male_1 = deaths_male_1,
        deaths_female_1 = deaths_female_1,
        population_midyear_male_1 = population_midyear_male_1, population_midyear_female_1 = population_midyear_female_1,
        year_of_analysis_1 = year_of_analysis_1,
        approach_exposure_2 = approach_exposure_2,
        approach_newborns_2 = approach_newborns_2,
        first_age_pop_2 = first_age_pop_2, last_age_pop_2 = last_age_pop_2,
        deaths_male_2 = deaths_male_2,
        deaths_female_2 = deaths_female_2,
        population_midyear_male_2 = population_midyear_male_2, population_midyear_female_2 = population_midyear_female_2,
        year_of_analysis_2 = year_of_analysis_2,
        min_age = min_age, max_age = max_age,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        info_1 = info_1 , info_2 = info_2)


    return(output)
  }
