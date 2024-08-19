# Title and description

#' Attributable deaths based on life tables
#'
#' Calculates the premature deaths attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).
#' @inheritParams attribute
#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
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

attribute_deaths_from_lifetable <-
  function(exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL, erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           approach_exposure = "constant",
           approach_newborns = "without_newborns",
           deaths_male = NULL,
           deaths_female = NULL,
           first_age_pop, last_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_midyear_male, population_midyear_female,
           year_of_analysis,
           min_age = NULL, max_age = NULL,
           info = NULL){

    output <-
      bestcost::attribute(
        health_metric = "deaths_from_lifetable",
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
        dw_central = NULL,
        duration = NULL,
        # Lifetable arguments
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
        corrected_discount_rate = NULL,
        geo_id_raw = NULL, geo_id_aggregated = NULL,
        info = info)

    return(output)

  }
