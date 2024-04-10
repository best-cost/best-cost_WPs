#' Health impacts based on life tables
#'
#' Calculates the years lived with disability attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).
#' @inheritParams attribute_deaths_lifetable_rr
#' @param corrected_discount_rate \code{Numeric value} of the corrected discount rate as proportion (i.e. 0.1 instead of 10\%).
#' @param disability_weight \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the disease duration
#' @param info \code{String} showing additional information or id for the pollutant. The suffix "info" will be added to the column name. Default value = NULL.
#' @return
#' This function returns a \code{data.frame}
#' @import dplyr
#' @import purrr
#' @author Axel Luyten
#' @note Experimental function
#' @export
attribute_yld_lifetable_rr <-
  function(exp, prop_pop_exp = 1,
           rr, rr_increment, erf_shape, cutoff,
           first_age_pop, last_age_pop, interval_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_male, population_female,
           year_of_analysis,
           corrected_discount_rate = 0,
           min_age = NULL, max_age = NULL,
           erf_c = NULL,
           disability_weight,
           info = NULL,
           duration = NULL){

    # Check input data ####

    # Compile input data and calculate paf putting all into a data frame
    input <-
      bestcost::compile_input(
        exp = exp,
        prop_pop_exp = prop_pop_exp,
        disability_weight = disability_weight,
        duration = duration,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        bhd = NULL,
        min_age = min_age,
        max_age = max_age,
        info = info,
        method = paste0("lifetable_rr_corrected"))


    # Get PAF and add to the input data frame
    input_risk_paf <-
      bestcost::get_risk_and_paf(input = input)

    # Compile list of life table data frame (by sex)
    # Col 1: age; col 2: probability of death; col 3: population

    lifetable_withPop <- list(
      male =
        data.frame(
          age = seq(from = first_age_pop,
                    to = last_age_pop,
                    by = interval_age_pop),
          age_end = seq(from = first_age_pop + interval_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop + interval_age_pop),
          death_probability_natural = prob_natural_death_male,
          death_probability_total = prob_total_death_male,
          population = population_male),

      female =
        data.frame(
          age = seq(from = first_age_pop,
                    to = last_age_pop,
                    by = interval_age_pop),
          age_end = seq(from = first_age_pop + interval_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop + interval_age_pop),
          death_probability_natural = prob_natural_death_female,
          death_probability_total = prob_total_death_female,
          population = population_female))

    # Get attributable cases in YOA + 1 ####

    pop_impact <-
      bestcost::get_pop_impact(
        lifetab_withPop = lifetable_withPop,
        year_of_analysis = year_of_analysis,
        paf = input_risk_paf[, c("rr_ci", "paf")])

    yld <-
        bestcost::get_yld(
          pop_impact = pop_impact,
          year_of_analysis = year_of_analysis,
          min_age = min_age,
          max_age = max_age,
          first_age_pop = first_age_pop,
          last_age_pop = last_age_pop,
          meta = input_risk_paf,
          disability_weight = disability_weight,
          duration = duration)

    # Compile output ####
    output <-
      list(
        total = yld[["total"]],
        detailed = list(yld_detaild = yld[["detailed"]],
                        pop_impact = pop_impact)
        )

    return(output)

  }
