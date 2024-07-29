# Title and description

#' Compare attributable deaths obtained applying a life table approach

#' Calculates the deaths between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
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
compare_deaths_from_lifetable <-
  function(approach_comparison = "delta",
           exp_central_1, exp_lower_1 = NULL, exp_upper_1 = NULL,
           exp_central_2, exp_lower_2 = NULL, exp_upper_2 = NULL,
           prop_pop_exp_1 = 1,
           prop_pop_exp_2 = 1,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           first_age_pop_1, last_age_pop_1,
           prob_natural_death_male_1, prob_natural_death_female_1,
           prob_total_death_male_1, prob_total_death_female_1,
           population_midyear_male_1, population_midyear_female_1,
           year_of_analysis_1,
           first_age_pop_2, last_age_pop_2,
           prob_natural_death_male_2, prob_natural_death_female_2,
           prob_total_death_male_2, prob_total_death_female_2,
           population_midyear_male_2, population_midyear_female_2,
           year_of_analysis_2,
           min_age = NULL, max_age = NULL,
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           info_1 = NULL, info_2 = NULL){



    output <-
      bestcost::compare(
        approach_comparison = approach_comparison,
        health_metric = "deaths_from_lifetable",
        risk_method = "relative_risk",
        exp_central_1, exp_lower_1 = exp_lower_1, exp_upper_1 = exp_upper_1,
        exp_central_2, exp_lower_2 = exp_lower_2, exp_upper_2 = exp_upper_2,
        prop_pop_exp_1 = prop_pop_exp_1,
        prop_pop_exp_2 = prop_pop_exp_2,
        pop_exp_1 = NULL,
        pop_exp_2 = NULL,
        cutoff = cutoff,
        rr_central = rr_central , rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_c_central = erf_c_central, erf_c_lower = erf_c_lower, erf_c_upper = erf_c_upper,
        bhd_central_1 = NULL, bhd_lower_1 = NULL, bhd_upper_1 = NULL,
        bhd_central_2 = NULL, bhd_lower_2 = NULL, bhd_upper_2 = NULL,
        first_age_pop_1 = first_age_pop_1, last_age_pop_1 = last_age_pop_1,
        prob_natural_death_male_1 = prob_natural_death_male_1, prob_natural_death_female_1 = prob_natural_death_female_1,
        prob_total_death_male_1 = prob_total_death_male_1, prob_total_death_female_1 = prob_total_death_female_1,
        population_midyear_male_1 = population_midyear_male_1, population_midyear_female_1 = population_midyear_female_1,
        year_of_analysis_1 = year_of_analysis_1,
        first_age_pop_2 = first_age_pop_2, last_age_pop_2 = last_age_pop_2 ,
        prob_natural_death_male_2 = prob_natural_death_male_2, prob_natural_death_female_2 = prob_natural_death_female_2,
        prob_total_death_male_2 = prob_total_death_male_2, prob_total_death_female_2 = prob_total_death_female_2,
        population_midyear_male_2 = population_midyear_male_2, population_midyear_female_2 = population_midyear_female_2,
        year_of_analysis_2 = year_of_analysis_2,
        min_age = min_age, max_age = max_age,
        disability_weight = NULL,
        duration = NULL,
        corrected_discount_rate = NULL,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        info_1 = info_1 , info_2 = info_2)


    return(output)
  }
