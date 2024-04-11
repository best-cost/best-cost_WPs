#' Health impacts based on life tables
#'
#' Calculates the years lived with disability attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).
#' @param exp \code{Numeric values} Population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {Numeric vector} Fraction (values between 0 & 1) of the total population exposed to (one or more) exposure categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Numeric vector} of three numeric values referring to the central estimate of the exposure-response function and the corresponding lower and upper 95\% confidence interval bounds.
#' @param rr_increment \code{Numeric value} showing the increment of the exposure-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{Character string} either "linear" or "loglinear".
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data
#' @param interval_age_pop \code{Numeric value} of the interval (in years) of each age group from population and life table data
#' @param prob_natural_death_male \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males.
#' @param prob_natural_death_female \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females.
#' @param prob_total_death_male \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males.
#' @param prob_total_death_female \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females.
#' @param population_male \code{Numeric vector} containing the mid-year male population for the year of analysis.
#' @param population_female \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
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

    lifetable_withPop <-
      bestcost::compile_lifetable_pop(
        first_age_pop =  first_age_pop,
        last_age_pop = last_age_pop,
        interval_age_pop =  interval_age_pop,
        prob_natural_death_male = prob_natural_death_male,
        prob_natural_death_female = prob_natural_death_female,
        prob_total_death_male = prob_total_death_male,
        prob_total_death_female = prob_total_death_female,
        population_male = population_male,
        population_female =  population_female)

    # Get attributable cases in YOA + 1 ####

    pop_impact <-
      bestcost::get_pop_impact(
        lifetab_withPop = lifetable_withPop,
        year_of_analysis = year_of_analysis,
        paf = input_risk_paf[, c("rr_ci", "paf")])



    # Get YLD based on pop impact
    yld <-
        bestcost::get_yld(
          pop_impact = pop_impact,
          year_of_analysis = year_of_analysis,
          min_age = min_age,
          max_age = max_age,
          meta = input_risk_paf,
          disability_weight = disability_weight,
          duration = duration)

    # Compile output ####
    output <-
      list(
        total = yld[["total"]],
        detailed = list(by_age_year_sex = pop_impact[["pop_impact"]],
                        by_sex = yld[["detailed"]]))

    return(output)

  }
