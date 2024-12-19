#' Attributable health cases based on relative risk

#' @description
#' Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param approach_multiexposure \code{String} showing the approach that has to be used in assessments with multiple exposures. To choose among: "additive", "multiplicative" or "combined".
#' @param health_metric \code{String} showing the change in outcome metric to assess attributable health impacts. To choose between "same_input_output" (default), "yld", "deaths_from_lifetable", "yll_from_lifetable", "yld_from_lifetable" and "daly_from_lifetable".
#' @param approach_risk \code{String} showing the risk risk method. To choose between: "relative_risk" (default) or "absolute_risk".
#' @param exp_central,exp_lower,exp_upper \code{Numeric values} of the exposure to the environmental stressor referring to the central estimate and (optionally) to lower and upper bound of the 95\% confidence interval. If only one value is provided, it will be assumed that it refers to population-weighted mean exposure in ug/m3. If a {vector} is provided, it will be assumed that it refers to the exposure categories (average exposure in the category) in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {Numeric vector} Fraction (values between 0 & 1) of the total population exposed to (one or more) exposure categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff_central,cutoff_lower,cutoff,upper \code{Numeric value} showing the central exposure cut-off in ug/m3 and (optionally) the lower and upper bounds of the 95\% confidence interval. The cut-off level refers to the exposure level below which no health effects occur.
#' @param rr_central,rr_lower,rr_upper \code{Numeric values} referring to the central estimate of the relative risk and the corresponding lower and upper 95\% confidence interval bounds.
#' @param erf_increment \code{Numeric value} showing the increment of the exposure-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_eq_central,erf_eq_lower,erf_eq_upper \code{String} or \code{function} referring to the equation of the user-defined exposure-response function. If a \code{string} is entered, the function must contains only one variable: x (exposure). E.g. "3+x+x^2". If a \code{function} is entered, it has to have a function class. If only the values of the x-axis (exposure) and y axis (relative risk) of the dots in the exposure-response function are available, a cubic spline natural interpolation can be assumed to get the function using, e.g., \code{stats::splinefun(x, y, method="natural")}
#' @param approach_exposure \code{String} showing whether air pollution is constant or only in one year. Options: "single_year" (default), "constant"
#' @param approach_newborns \code{String} showing whether newborns are considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns"
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data (age interval = 1 year)
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data (age interval = 1 year)
#' @param population_midyear_male,population_midyear_female \code{Numeric vector} containing the mid-year male population for the year of analysis for male and female respectively.
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param time_horizon \code{Numeric value} corresponding to time horizon (number of years) for which the impacts should be considered. For example, would be 10 if one is interested in the impacts of an intervention during the next 10 years.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
#' @param bhd_central,bhd_lower,bhd_upper \code{Numeric value} showing the central estimate and (optionally) the lower bound and the upper bound of the confidence interval of the baseline health data (e.g. incidence of the health outcome in the population).
#' @param dw_central,dw_lower,dw_upper \code{Numeric value} showing the disability weights (central estimate, lower and upper 95\% confidence intervals) associated with the morbidity health outcome
#' @param duration_central,duration_lower,duration_upper \code{Numeric value} showing the central estimate of the disease duration and (optionally) the lower and upper bounds of the 95\% confidence interval.
#' @param population code{Vector} with numeric values referring to the population in the geographical unit
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
  function(approach_risk = "relative_risk",
           exp_central, exp_lower = NULL, exp_upper = NULL,
           cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL, erf_shape = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           # Arguments for advanced use
           prop_pop_exp = 1,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           # Lifetable arguments
           population_midyear_male = NULL, population_midyear_female = NULL,
           deaths_male = NULL, deaths_female = NULL, # For AirQ+ method for lifetable
           first_age_pop = NULL, last_age_pop = NULL,
           min_age = NULL, max_age = NULL,
           year_of_analysis = NULL,
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
           approach_exposure = NULL,
           approach_newborns = NULL,
           time_horizon = NULL,
           # Iteration arguments
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           # Meta-information
           population = NULL,
           health_metric = "same_input_output",
           approach_multiexposure = NULL,
           info = NULL){

    # Check input data
    #stopifnot(exprs = {
    #length(exp) == length(prop_pop_exp)
    #})

    # Compile input data
    input <-
      healthiar::compile_input(
        approach_risk = approach_risk,
        approach_multiexposure = approach_multiexposure,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        info = info,
        health_metric = health_metric,
        population = population,
        # YLD
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        # Lifetable arguments if needed
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        year_of_analysis = year_of_analysis,
        time_horizon = time_horizon,
        min_age = min_age,
        max_age = max_age,
        first_age_pop =  first_age_pop,
        last_age_pop = last_age_pop,
        population_midyear_male = population_midyear_male,
        population_midyear_female =  population_midyear_female,
        deaths_male = deaths_male,
        deaths_female = deaths_female)

    # Calculate the health impacts for each case (uncertainty, category, geo area...)
    impact_raw <-
      healthiar:::get_impact(input = input,
                             pop_fraction_type = "paf")

    # Get the main and detailed output by aggregating and/or filtering cases (rows)
    output <-
      healthiar:::get_output(impact_raw)

    return(output)
  }
