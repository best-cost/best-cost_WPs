#' Attributable health cases based on relative risk

#' @description Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @inheritParams attribute_deaths_lifetable_rr
#' @param bhd_central \code{Numeric value} showing the only or central estimate (if confidence interval) of the baseline health data (e.g. incidence of the health outcome in the population).
#' @param bhd_upper \code{Numeric value} showing the lower bound estimate (confidence interval) of the baseline health data.
#' @param bhd_lower \code{Numeric value} showing the upper bound estimate (confidence interval) of the baseline health data.
#' @param risk_method \code{String} showing the calculation method of the risk: "relative_risk" or "absolute_risk".
#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
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
#' @inherit attribute_deaths_lifetable_rr note
#' @export
attribute_copy <-
  function(health_metric = "same_input_output",
           risk_method = "relative_risk",
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           pop_exp = NULL,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           disability_weight = NULL,
           duration = NULL,
           first_age_pop = NULL, last_age_pop = NULL,
           prob_natural_death_male = NULL, prob_natural_death_female = NULL,
           prob_total_death_male, prob_total_death_female = NULL,
           population_midyear_male = NULL, population_midyear_female = NULL,
           year_of_analysis = NULL,
           min_age = NULL, max_age = NULL,
           corrected_discount_rate = NULL,
           geo_id_raw = NULL, geo_id_aggregated = NULL,
           info = NULL){


    # Compile input data (except lifetable) and calculate paf putting all into a data frame
    input <-
      bestcost:::compile_input(
        exp_central = exp_central, exp_lower =  exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        pop_exp = pop_exp,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_c_central = erf_c_central, erf_c_lower = erf_c_lower, erf_c_upper = erf_c_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        info = info,
        health_metric = health_metric,
        disability_weight = disability_weight,
        risk_method = risk_method)

    # Only if lifetable approach
    # Compile list of life table data frame (by sex)
    # Col 1: age; col 2: probability of death; col 3: population

    if(grepl("lifetable", health_metric)){

      lifetable_withPop <-
        bestcost:::compile_lifetable_pop(
          first_age_pop =  first_age_pop,
          last_age_pop = last_age_pop,
          prob_natural_death_male = prob_natural_death_male,
          prob_natural_death_female = prob_natural_death_female,
          prob_total_death_male = prob_total_death_male,
          prob_total_death_female = prob_total_death_female,
          population_midyear_male = population_midyear_male,
          population_midyear_female =  population_midyear_female)

    }


    # Calculate the health impacts for each case (uncertainty, category, geo area...)
    output_raw <-
      bestcost:::get_impact_copy(input = input,
                           lifetable_withPop = lifetable_withPop,
                           year_of_analysis = year_of_analysis,
                           min_age = min_age,
                           max_age = max_age,
                           disability_weight = disability_weight,
                           duration = duration)

    # Get the main and detailed output by aggregating and/or filtering cases (rows)
    output <-
      bestcost:::get_output(output_raw)




    return(output)
  }
