# Title and description

#' Attributable deaths based on life tables
#'
#' Calculates the premature deaths attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).
#' @param exp \code{Numeric values} Population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {Numeric vector} Fraction (values between 0 & 1) of the total population exposed to (one or more) exposure categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Numeric vector} of three numeric values referring to the central estimate of the exposure-response function and the corresponding lower and upper 95\% confidence interval bounds.
#' @param rr_increment \code{Numeric value} showing the increment of the exposure-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data (age interval = 1 year)
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data (age interval = 1 year)
#' @param prob_natural_death_male \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males.
#' @param prob_natural_death_female \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females.
#' @param prob_total_death_male \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males.
#' @param prob_total_death_female \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females.
#' @param population_midyear_male \code{Numeric vector} containing the mid-year male population for the year of analysis.
#' @param population_midyear_female \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
#' @param info \code{String} or {data frame} showing additional information or id. The suffix "info" will be added to the column name. Default value = NULL.
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
attribute_deaths_lifetable_rr <-
  function(exp, prop_pop_exp = 1,
           cutoff,
           rr, rr_increment, erf_shape,
           erf_c = NULL,
           first_age_pop, last_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_midyear_male, population_midyear_female,
           year_of_analysis,
           min_age = NULL, max_age = NULL,
           info = NULL){

    # Check input data ####



    input <-
      bestcost::compile_input(
        exp = exp,
        prop_pop_exp = prop_pop_exp,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        bhd = NULL,
        min_age = min_age,
        max_age = max_age,
        info = info,
        method = paste0("lifetable_rr_corrected"),
        disability_weight = NULL,
        duration = NULL)


    # Get PAF and add to the input data frame
    input_risk_paf <-
      bestcost::get_risk_and_paf(input = input)


    # Compile list of life table data frame (by sex)
    # Col 1: age; col 2: probability of death; col 3: population

    lifetable_withPop <-
      bestcost::compile_lifetable_pop(
        first_age_pop =  first_age_pop,
        last_age_pop = last_age_pop,
        prob_natural_death_male = prob_natural_death_male,
        prob_natural_death_female = prob_natural_death_female,
        prob_total_death_male = prob_total_death_male,
        prob_total_death_female = prob_total_death_female,
        population_midyear_male = population_midyear_male,
        population_midyear_female =  population_midyear_female)

    # Get population impact ####
    pop_impact <-
      bestcost::get_pop_impact(
        lifetab_withPop = lifetable_withPop,
        year_of_analysis = year_of_analysis,
        pop_fraction = input_risk_paf[, c("erf_ci", "paf")],
        outcome_metric = "death")


    # Calculate deaths ####
    deaths <-
      bestcost::get_deaths(
        pop_impact = pop_impact,
        year_of_analysis = year_of_analysis,
        min_age = min_age,
        max_age = max_age,
        meta = input_risk_paf)


    # Compile output ####
    output <-
      list(total = deaths[["total"]],
           detailed = list(by_age_year_sex = pop_impact[["pop_impact"]],
                           by_sex = deaths[["detailed"]]))

    return(output)

  }





