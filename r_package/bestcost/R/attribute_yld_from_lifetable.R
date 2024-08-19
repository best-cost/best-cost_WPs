#' Health impacts based on life tables

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
  function(exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL, erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           approach_exposure = "single_year",
           approach_newborns = "without_newborns",
           first_age_pop, last_age_pop,
           deaths_male = NULL,
           deaths_female = NULL,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_midyear_male, population_midyear_female,
           year_of_analysis,
           corrected_discount_rate = NULL,
           min_age = NULL, max_age = NULL,
           disability_weight_central,
           duration,
           info = NULL){

    output<-
      bestcost::attribute(
        health_metric = "yld_from_lifetable",
        approach_risk = "relative_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        pop_exp = NULL,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_c_central = erf_c_central, erf_c_lower = erf_c_lower, erf_c_upper = erf_c_upper,
        bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        first_age_pop = first_age_pop, last_age_pop = last_age_pop,
        deaths_male = deaths_male,
        deaths_female = deaths_female,
        prob_natural_death_male = prob_natural_death_male, prob_natural_death_female = prob_natural_death_female,
        prob_total_death_male = prob_total_death_male, prob_total_death_female = prob_total_death_female,
        population_midyear_male = population_midyear_male, population_midyear_female = population_midyear_female,
        year_of_analysis = year_of_analysis,
        min_age = min_age, max_age = max_age,
        corrected_discount_rate = corrected_discount_rate,
        disability_weight_central = disability_weight_central,
        duration = duration,
        geo_id_raw = NULL , geo_id_aggregated = NULL,

        info = info)

    return(output)

  }
