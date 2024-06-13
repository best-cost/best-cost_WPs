# Title and description

#' Compare attributable health cases based on single baseline health value and relative risk

#' Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @param comparison_method \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @param exp_1 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 1.
#' @param exp_2 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 2.
#' @param prop_pop_exp_1 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 1. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param prop_pop_exp_2 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 2. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the central estimate as well as the lower and upper bound of the confidence interval.
#' @param erf_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param bhd_1 \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population) in the scenario 1.
#' @param bhd_2 \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population) in the scenario 1.
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
compare_health <-
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
           bhd_central_1, bhd_lower_1 = NULL, bhd_upper_1 = NULL,
           bhd_central_2, bhd_lower_2 = NULL, bhd_upper_2 = NULL,
           info_1 = NULL, info_2 = NULL){


    output <-
      bestcost::compare(
        comparison_method = comparison_method,
        health_metric = "same_input_output",
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
        bhd_central_1 = bhd_central_1, bhd_lower_1 = bhd_lower_1, bhd_upper_1 = bhd_upper_1,
        bhd_central_2 = bhd_central_2, bhd_lower_2 = bhd_lower_2, bhd_upper_2 = bhd_upper_2,
        disability_weight = NULL,
        duration = NULL,
        first_age_pop_1 = NULL, last_age_pop_1 = NULL,
        prob_natural_death_male_1 = NULL, prob_natural_death_female_1 = NULL,
        prob_total_death_male_1 = NULL, prob_total_death_female_1 = NULL,
        population_midyear_male_1 = NULL, population_midyear_female_1 = NULL,
        year_of_analysis_1 = NULL,
        first_age_pop_2 = NULL, last_age_pop_2 = NULL,
        prob_natural_death_male_2 = NULL, prob_natural_death_female_2 = NULL,
        prob_total_death_male_2 = NULL, prob_total_death_female_2 = NULL,
        population_midyear_male_2 = NULL, population_midyear_female_2 = NULL,
        year_of_analysis_2 = NULL,
        min_age = NULL, max_age = NULL,
        info_1 = NULL, info_2 = NULL
      )

    return(output)
  }

