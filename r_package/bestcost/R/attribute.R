#' Attributable health cases based on relative risk

#' @description Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param health_metric \code{String} showing the change in outcome metric to assess attributable health impacts. To choose between "same_input_output" (default), "yld_from_prevalence", "deaths_from_lifetable", "yll_from_lifetable", "yld_from_lifetable" and "daly_from_lifetable".
#' @param approach_risk \code{String} showing the risk risk method. To choose between: "relative_risk" (default) or "absolute_risk".
#' @param exp_central,exp_lower,exp_upper \code{Numeric values} of the exposure
#' to the environmental stressor referring to the central estimate and (optionally)
#' to lower and upper bound of the confidence interval. If only one value is provided,
#' it will be assumed that it refers to population-weighted mean exposure in ug/m3.
#' If a {vector} is provided, it will be assumed that it refers to the exposure categories
#' (average exposure in the category) in a exposure distribution
#' (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {Numeric vector} Fraction (values between 0 & 1) of the total population exposed to (one or more) exposure categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param pop_exp \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories. The length of this input variable must be the same as "exp".
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr_central,rr_lower,rr_upper Three \code{numeric values} referring to the central estimate of the relative risk and the corresponding lower and upper 95\% confidence interval bounds.
#' @param erf_increment \code{Numeric value} showing the increment of the exposure-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.<<<<<<< HEAD
#' @param approach_exposure \code{String} showing whether air pollution is constant or only in one year. Options: "single_year" (default), "constant"
#' @param approach_newborns \code{String} showing whether newborns are considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns"
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data (age interval = 1 year)
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data (age interval = 1 year)
#' @param prob_natural_death_male,prob_natural_death_female \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for male and female respectively.
#' @param prob_total_death_male,prob_total_death_female  \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for male and female respectively.
#' @param population_midyear_male,population_midyear_female \code{Numeric vector} containing the mid-year male population for the year of analysis for male and female respectively.
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
#' @param bhd_central,bhd_lower,bhd_upper \code{Numeric value} showing the central
#' @param dw_central \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the disease duration
#' @param corrected_discount_rate \code{Numeric value} showing the discount rate for future years including correction from inflation rate
#' estimate and (optionally) the lower bound and the upper bound of the confidence
#' interval of the baseline health data (e.g. incidence of the health outcome in the population).
#' @param geo_id_raw \code{Vector} showing the id code of the each geographic area considered in the assessment. If a vector is entered here, the data for each geographical area have to be provided as list in the corresponding arguments.
#' @param geo_id_aggregated \code{Vector} showing the id code of the geographic area for which raw geo ids have to be aggregated. The vector has to have the same length as geo_id_raw. Therefore, geo_id_aggregated should have duplicated values for those geo_id_r
#' @param info \code{String} or {data frame} showing additional information or id. The suffix "info" will be added to the column name. Default value = NULL.
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
#'
#' @author Alberto Castro
#' @export

attribute <-
  function(health_metric = "same_input_output",
           approach_risk = "relative_risk",
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           pop_exp = NULL,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           # Lifetable arguments
           approach_exposure = NULL,
           approach_newborns = NULL,
           first_age_pop = NULL, last_age_pop = NULL,
           prob_natural_death_male = NULL, prob_natural_death_female = NULL,
           prob_total_death_male, prob_total_death_female = NULL,
           population_midyear_male = NULL, population_midyear_female = NULL,
           deaths_male = NULL, deaths_female = NULL, # For AirQ+ method for lifetable
           year_of_analysis = NULL,
           min_age = NULL, max_age = NULL,
           dw_central = NULL,
           duration = NULL,
           corrected_discount_rate = NULL,
           # Iteration arguments
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           # Meta-information
           info = NULL){

    # Check input data
    #stopifnot(exprs = {
    #length(exp) == length(prop_pop_exp)
    #})

    # if (approach = "cutoff_in_exposure") { # whole script in this loop

    # Compile input data (except lifetable)
    input <-
      bestcost:::compile_input(
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
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
        dw_central = dw_central,
        approach_risk = approach_risk,
        # Lifetable arguments if needed
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        first_age_pop =  first_age_pop,
        last_age_pop = last_age_pop,
        prob_natural_death_male = prob_natural_death_male,
        prob_natural_death_female = prob_natural_death_female,
        prob_total_death_male = prob_total_death_male,
        prob_total_death_female = prob_total_death_female,
        population_midyear_male = population_midyear_male,
        population_midyear_female =  population_midyear_female,
        deaths_male = deaths_male,
        deaths_female = deaths_female)


    # Calculate the health impacts for each case (uncertainty, category, geo area...)
    impact_raw <-
      bestcost:::get_impact(input = input,
                            year_of_analysis = year_of_analysis,
                            min_age = min_age,
                            max_age = max_age,
                            corrected_discount_rate = corrected_discount_rate,
                            dw_central = dw_central,
                            duration = duration,
                            pop_fraction_type = "paf")

    # Get the main and detailed output by aggregating and/or filtering cases (rows)
    output <-
      bestcost:::get_output(impact_raw)




    return(output)
  }

# } end of loop (for cutoff variante)


# if(approach = "scenario_A_minus_scenario_B") { # cutoff in the exposure response function
# bestcost:::compare() # calculates scenario_A_minus_scenario_B
# comparison_yll_lifetable_delta  <- # code copied from "testing_Rpackage.Rmd"
# impact_raw  <-
#   bestcost::compare(
#     health_metric = health_metric,
#     approach_comparison = "delta",
#     exp_central_1 = exp_central, # Put exp_central here, maybe
#     prop_pop_exp_1 = prop_pop_exp, # Fake data just for testing purposes
#     exp_central_2 = cutoff, # Fake data just for testing purposes
#     prop_pop_exp_2 = prop_pop_exp, # Fake data just for testing purposes
#     cutoff = 0,   # put to 0, so that in get_risk we don't have cutoff - cutoff = 0
#     rr_central = rr_central,
#     rr_lower = rr_lower,
#     rr_upper = rr_upper,
#     erf_increment = erf_increment,
#     erf_shape = erf_shape,
#     first_age_pop_1 = first_age_pop, #
#     last_age_pop_1 = 99,
#     prob_natural_death_male_1 = lifetable_withPopulation[["male"]]$death_probability_natural,
#     prob_natural_death_female_1 = lifetable_withPopulation[["female"]]$death_probability_natural,
#     prob_total_death_male_1 = lifetable_withPopulation[["male"]]$death_probability_total,
#     prob_total_death_female_1 = lifetable_withPopulation[["female"]]$death_probability_total,
#     population_midyear_male_1 = lifetable_withPopulation[["male"]]$population,
#     population_midyear_female_1 = lifetable_withPopulation[["female"]]$population,
#     year_of_analysis_1 = 2019,
#     first_age_pop_2 = first_age_pop, #
#     last_age_pop_2 = 99,
#     prob_natural_death_male_2 = lifetable_withPopulation[["male"]]$death_probability_natural,
#     prob_natural_death_female_2 = lifetable_withPopulation[["female"]]$death_probability_natural,
#     prob_total_death_male_2 = lifetable_withPopulation[["male"]]$death_probability_total,
#     prob_total_death_female_2 = lifetable_withPopulation[["female"]]$death_probability_total,
#     population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
#     population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
#     year_of_analysis_2 = 2019,
#     info_1 = input_data_mortality$pollutant[2],
#     info_2 = input_data_mortality$pollutant[2],
#     min_age = 20,
#     dw_central = dw_central,
#     duration = duration)
#
#     return(output)
# }
