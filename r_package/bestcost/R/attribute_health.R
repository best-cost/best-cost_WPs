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
  function(
      exp_central, exp_lower = NULL, exp_upper = NULL,
      prop_pop_exp = 1,
      cutoff,
      rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
      erf_increment = NULL,
      erf_shape = NULL,
      erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
      bhd_central, bhd_lower = NULL, bhd_upper = NULL,
      geo_id_raw = NULL, geo_id_aggregated = NULL,
      info = NULL){

    output <-
      bestcost::attribute(
        health_metric = "same_input_output",
        risk_method = "relative_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        pop_exp = NULL,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_c_central = erf_c_central, erf_c_lower = erf_c_lower, erf_c_upper = erf_c_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_lower,
        first_age_pop = NULL, last_age_pop = NULL,
        prob_natural_death_male = NULL, prob_natural_death_female = NULL,
        prob_total_death_male = NULL, prob_total_death_female = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        year_of_analysis = NULL,
        min_age = NULL, max_age = NULL,
        corrected_discount_rate = NULL,
        disability_weight = NULL,
        duration = NULL,
        geo_id_raw = geo_id_raw , geo_id_aggregated = geo_id_aggregated,
        info = info)

    return(output)


  }
