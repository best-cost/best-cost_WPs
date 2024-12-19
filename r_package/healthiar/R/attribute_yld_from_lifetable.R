#' Health impacts based on life tables

#' @description
#' Calculates the years lived with disability attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).
#' @inheritParams attribute
#' @return
#' This function returns a \code{data.frame}
#' @import dplyr
#' @import purrr
#' @author Axel Luyten
#' @note Experimental function
#' @export
attribute_yld_from_lifetable <-
  function(approach_multiexposure = NULL,
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL, erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           approach_exposure = "single_year",
           approach_newborns = "without_newborns",
           first_age_pop, last_age_pop,
           deaths_male = NULL,
           deaths_female = NULL,
           population_midyear_male, population_midyear_female,
           year_of_analysis,
           time_horizon = NULL,
           min_age = NULL, max_age = NULL,
           dw_central, dw_lower = NULL, dw_upper = NULL,
           duration_central, duration_lower = NULL, duration_upper = NULL,
           info = NULL){

    output<-
      healthiar::attribute(
        health_metric = "yld_from_lifetable",
        approach_risk = "relative_risk",
        approach_multiexposure = approach_multiexposure,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        first_age_pop = first_age_pop, last_age_pop = last_age_pop,
        deaths_male = deaths_male,
        deaths_female = deaths_female,
        population_midyear_male = population_midyear_male, population_midyear_female = population_midyear_female,
        year_of_analysis = year_of_analysis,
        time_horizon = time_horizon,
        min_age = min_age, max_age = max_age,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        geo_id_raw = NULL , geo_id_aggregated = NULL,
        info = info)

    return(output)

  }
