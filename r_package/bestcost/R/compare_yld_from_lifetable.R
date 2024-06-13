# Title and description

#' Compare attributable years lived with disability obtained applying a life table approach

#' Calculates the year lived with disability between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @param comparison_method \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @param exp_central_1 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 1.
#' @param exp_central_2 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 2.
#' @param prop_pop_exp_1 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 1. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param prop_pop_exp_2 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 2. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the central estimate as well as the lower and upper bound of the confidence interval.
#' @param erf_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param first_age_pop_1 \code{Numeric value} starting age of the youngest age group from population and life table data in the scenario 1.
#' @param last_age_pop_1 \code{Numeric value} ending age of the oldest age group from population and life table data in the scenario 1.
#' @param prob_natural_death_male-1 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males in the scenario 1.
#' @param prob_natural_death_female_1 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females in the scenario 1.
#' @param prob_total_death_male_1 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males in the scenario 1.
#' @param prob_total_death_female_1 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females in the scenario 1.
#' @param population_midyear_male_1 \code{Numeric vector} containing the mid-year male population for the year of analysis of the scenario 1.
#' @param population_midyear_female_1 \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis_1 \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table in the scenario 2.
#' @param first_age_pop_2 \code{Numeric value} starting age of the youngest age group from population and life table data in the scenario 2.
#' @param last_age_pop_2 \code{Numeric value} ending age of the oldest age group from population and life table data in the scenario 2.
#' @param prob_natural_death_male_2 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males in the scenario 2.
#' @param prob_natural_death_female_2 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females in the scenario 2.
#' @param prob_total_death_male_2 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males in the scenario 2.
#' @param prob_total_death_female_2 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females in the scenario 2.
#' @param population_midyear_male_2 \code{Numeric vector} containing the mid-year male population for the year of analysis of the scenario 2.
#' @param population_midyear_female_2 \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis_2 \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
#' @param disability_weight \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the disease duration
#' @param info_1 \code{String} or {data frame} showing additional information or id of the scenario 1. The suffix "info" will be added to the column name. Default value = NULL.
#' @param info_2 \code{String} or {data frame} showing additional information or id of the scenario 1. The suffix "info" will be added to the column name. Default value = NULL.

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
compare_yld_from_lifetable <-
  function(comparison_method = "delta",
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
           disability_weight,
           duration = NULL,
           info_1 = NULL, info_2 = NULL){



    output <-
      bestcost::compare(
        comparison_method = comparison_method,
        health_metric = "yld_from_lifetable",
        risk_method = "relative_risk",
        exp_central_1, exp_lower_1 = exp_lower_1, exp_upper_1 = exp_upper_1,
        exp_central_2, exp_lower_2 = exp_lower_2, exp_upper_2 = exp_upper_2,
        prop_pop_exp_1 = prop_pop_exp_1,
        prop_pop_exp_2 = prop_pop_exp_2,
        pop_exp_1 = NULL,
        pop_exp_2 = NULL,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_c_central = erf_c_central, erf_c_lower = erf_c_lower, erf_c_upper = erf_c_upper,
        bhd_central_1 = NULL, bhd_lower_1 = NULL, bhd_upper_1 = NULL,
        bhd_central_2 = NULL, bhd_lower_2 = NULL, bhd_upper_2 = NULL,
        disability_weight = disability_weight,
        duration = duration,
        first_age_pop_1 = first_age_pop_1, last_age_pop_1 = last_age_pop_1,
        prob_natural_death_male_1 = prob_natural_death_male_1, prob_natural_death_female_1 = prob_natural_death_female_1,
        prob_total_death_male_1 = prob_total_death_male_1, prob_total_death_female_1 = prob_total_death_female_1,
        population_midyear_male_1 = population_midyear_male_1, population_midyear_female_1 = population_midyear_female_1,
        year_of_analysis_1 = year_of_analysis_1,
        first_age_pop_2 = first_age_pop_2, last_age_pop_2 = last_age_pop_2,
        prob_natural_death_male_2 = prob_natural_death_male_2, prob_natural_death_female_2 = prob_natural_death_female_2,
        prob_total_death_male_2 = prob_total_death_male_2, prob_total_death_female_2 = prob_total_death_female_2,
        population_midyear_male_2 = population_midyear_male_2, population_midyear_female_2 = population_midyear_female_2,
        year_of_analysis_2 = year_of_analysis_2,
        min_age = min_age, max_age = max_age,
        info_1 = info_1, info_2 = info_2)


    return(output)
  }
