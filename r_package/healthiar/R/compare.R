#' Compare attributable health cases based on single baseline health value and relative risk

#' @description Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @param approach_comparison \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @param exp_central_1 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 1.
#' @param exp_central_2 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 2.
#' @param prop_pop_exp_1 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 1. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param prop_pop_exp_2 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 2. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param pop_exp_1 \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories for the scenario 1. The length of this input variable must be the same as "exp".
#' @param pop_exp_2 \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories for the scenario 2. The length of this input variable must be the same as "exp".
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr_central,rr_lower,rr_upper \code{Numeric values} referring to the central estimate of the relative risk and the corresponding lower and upper 95\% confidence interval bounds.
#' @param erf_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_eq_central,erf_eq_lower,erf_eq_upper Equation of the user-defined exposure-response function that puts the relative risk (y) in relation with exposure (x). If the function is provided as \code{string}, it can only contains one variable: x (exposure). E.g. "3+x+x^2". If the function is provided as a \code{function}, the object should have a function class. If only the values of the x-axis (exposure) and y axis (relative risk) of the dots in the exposure-response function are available, a cubic spline natural interpolation can be assumed to get the function using, e.g., \code{stats::splinefun(x, y, method="natural")}
#' @param approach_exposure_1 \code{String} showing whether air pollution is constant or only in one year. Options: "single_year" (default), "constant"
#' @param approach_newborns_1 \code{String} showing whether newborns are considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns"
#' @param first_age_pop_1 \code{Numeric value} starting age of the youngest age group from population and life table data in the scenario 1.
#' @param last_age_pop_1 \code{Numeric value} ending age of the oldest age group from population and life table data in the scenario 1.
#' @param population_midyear_male_1 \code{Numeric vector} containing the mid-year male population for the year of analysis of the scenario 1.
#' @param population_midyear_female_1 \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis_1 \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table in the scenario 2.
#' @param approach_exposure_2 \code{String} showing whether air pollution is constant or only in one year. Options: "single_year" (default), "constant"
#' @param approach_newborns_2 \code{String} showing whether newborns are considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns"
#' @param first_age_pop_2 \code{Numeric value} starting age of the youngest age group from population and life table data in the scenario 2.
#' @param last_age_pop_2 \code{Numeric value} ending age of the oldest age group from population and life table data in the scenario 2.
#' @param population_midyear_male_2 \code{Numeric vector} containing the mid-year male population for the year of analysis of the scenario 2.
#' @param population_midyear_female_2 \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis_2 \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
#' @param dw_central,dw_lower,dw_upper Three \code{Numeric value} showing the disability weights (central estimate, lower and upper 95% confidence intervals) associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the disease duration
#' @param corrected_discount_rate \code{Numeric value} showing the discount rate for future years including correction from inflation rate
#' @param approach_discount \code{String} referring to the assumed equation for the discount factor.Per default: "exponential". Otherwise: "hyperbolic_harvey_1986" or "hyperbolic_mazur_1987".
#' @param geo_id_raw \code{Vector} showing the id code of the each geographic area considered in the assessment. If a vector is entered here, the data for each geographical area have to be provided as list in the corresponding arguments.
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
#' @author Alberto Castro
#' @note Experimental function
#' @export
compare <-
  function(approach_multiexposure = NULL,
           approach_comparison = "delta",
           health_metric = "same_input_output",
           approach_risk = "relative_risk",
           exp_central_1, exp_lower_1 = NULL, exp_upper_1 = NULL,
           exp_central_2, exp_lower_2 = NULL, exp_upper_2 = NULL,
           prop_pop_exp_1 = 1,
           prop_pop_exp_2 = 1,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           bhd_central_1 = NULL, bhd_lower_1 = NULL, bhd_upper_1 = NULL,
           bhd_central_2 = NULL, bhd_lower_2 = NULL, bhd_upper_2 = NULL,
           population_1 = NULL, population_2 = NULL,
           # Lifetable arguments
           approach_exposure_1 = NULL,
           approach_newborns_1 = NULL,
           first_age_pop_1 = NULL, last_age_pop_1 = NULL,
           deaths_male_1 = NULL,
           deaths_female_1 = NULL,
           population_midyear_male_1 = NULL, population_midyear_female_1 = NULL,
           year_of_analysis_1 = NULL,
           approach_exposure_2 = NULL,
           approach_newborns_2 = NULL,
           first_age_pop_2 = NULL, last_age_pop_2 = NULL,
           deaths_male_2 = deaths_male_2,
           deaths_female_2 = deaths_female_2,
           population_midyear_male_2 = NULL, population_midyear_female_2 = NULL,
           year_of_analysis_2 = NULL,
           min_age = NULL, max_age = NULL,
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
           corrected_discount_rate = NULL,
           approach_discount = NULL,
           # Iteration
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           info_1 = NULL, info_2 = NULL){

    # Calculate attributable health impacts in the scenario 1
    impact_raw_1 <-
      healthiar::attribute(
        approach_multiexposure = approach_multiexposure,
        approach_risk = approach_risk,
        health_metric = health_metric,
        exp_central = exp_central_1, exp_lower = exp_lower_1, exp_upper = exp_upper_1,
        prop_pop_exp = prop_pop_exp_1,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central_1, bhd_lower = bhd_lower_1, bhd_upper = bhd_upper_1,
        approach_exposure = approach_exposure_1,
        approach_newborns = approach_newborns_1,
        first_age_pop = first_age_pop_1,
        last_age_pop = last_age_pop_1,
        deaths_male = deaths_male_1,
        deaths_female = deaths_female_1,
        population_midyear_male = population_midyear_male_1,
        population_midyear_female = population_midyear_female_1,
        population = population_1,
        year_of_analysis = year_of_analysis_1,
        min_age = min_age,
        max_age = max_age,
        corrected_discount_rate = corrected_discount_rate,
        approach_discount = approach_discount ,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        info = info_1)

    # Calculate attributable health impacts in the scenario 2
    impact_raw_2 <-
      healthiar::attribute(
        approach_multiexposure = approach_multiexposure,
        approach_risk = approach_risk,
        health_metric = health_metric,
        exp_central = exp_central_2, exp_lower = exp_lower_2, exp_upper = exp_upper_2,
        prop_pop_exp = prop_pop_exp_2,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central_2, bhd_lower = bhd_lower_2, bhd_upper = bhd_upper_2,
        approach_newborns  = approach_newborns_2,
        approach_exposure = approach_exposure_2,
        first_age_pop = first_age_pop_2,
        last_age_pop = last_age_pop_2,
        deaths_male = deaths_male_2,
        deaths_female = deaths_female_2,
        population_midyear_male = population_midyear_male_2,
        population_midyear_female = population_midyear_female_2,
        population = population_2,
        year_of_analysis = year_of_analysis_2,
        min_age = min_age,
        max_age = max_age,
        corrected_discount_rate = corrected_discount_rate,
        approach_discount = approach_discount ,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        info = info_2)


    # Identify the arguments that have _1 or _2 in the name (scenario specific)
    # This is useful for joining data frames below
    scenario_specific_arguments <-
      grep("_1|_2", names(formals(compare)), value = TRUE) |>
      gsub("_1|_2", "", x = _) |>
      unique()
    #Add impact and pop_fraction
    scenario_specific_arguments <-
      c(scenario_specific_arguments,
        "impact", "pop_fraction")

    # If the user choose "pif"  as comparison method
    # pif is additonally calculated
    # impact is overwritten with the new values that refer to pif instead of paf
    if(approach_comparison == "delta"){

      # Identify the columns that are to be used to join impact_raw_1 and _2
      joining_columns_output <-
        healthiar:::find_joining_columns(
          df1 = impact_raw_1[["health_detailed"]][["raw"]],
          df2 = impact_raw_2[["health_detailed"]][["raw"]],
          except = scenario_specific_arguments)

        # Merge the result tables by common columns
        impact_raw_main <-
          dplyr::left_join(
            impact_raw_1[["health_detailed"]][["raw"]],
            impact_raw_2[["health_detailed"]][["raw"]],
            by = joining_columns_output,
            suffix = c("_1", "_2")) |>
          # Calculate the delta (difference) between scenario 1 and 2
          dplyr::mutate(impact = impact_1 - impact_2,
                        impact_rounded = round(impact, 0))

        impact_raw <- list(health_main =  impact_raw_main)
    }


    # If the user choose "pif"  as comparison method
    # pif is additonally calculated
    # impact is overwritten with the new values that refer to pif instead of paf
    # Use if instead of else if becuase otherwise the package will read here inside
    # and produce an error because the variables are different
    if(approach_comparison == "pif"){
      # Either both NULL or identical. Use the function identical() to enable NULL==NULL
      if(!identical(bhd_lower_1, bhd_lower_2) & identical(bhd_lower_1, bhd_lower_2) & identical(bhd_upper_1, bhd_upper_2)){
        stop("Baseline health data have to be identical for scenario 1 and 2.")
      }

      # Error if length of fraction_of_year_lived > 1
      if (!identical(first_age_pop_1, first_age_pop_2) &
          !identical(last_age_pop_1, last_age_pop_2) &
          !identical(population_midyear_male_1, population_midyear_male_2) &
          !identical(population_midyear_female_1, population_midyear_female_2) &
          !identical(year_of_analysis_1, year_of_analysis_2)){
          stop("Age interval, probability of dying and population in the scenario 1 and 2 have to be identical")
      }


      # Compile input data of scenario 1
      input_1 <-
        healthiar::compile_input(
          health_metric = health_metric,
          approach_risk = approach_risk,
          exp_central = exp_central_1, exp_lower = exp_lower_1, exp_upper = exp_upper_1,
          prop_pop_exp = prop_pop_exp_1,
          cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
          bhd_central = bhd_central_1, bhd_lower = bhd_lower_1, bhd_upper = bhd_upper_1,
          rr_central = rr_central,
          rr_lower = rr_lower,
          rr_upper = rr_upper,
          erf_increment = erf_increment,
          erf_shape = erf_shape,
          erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
          min_age = min_age,
          max_age = max_age,
          info = info_1,
          geo_id_raw = geo_id_raw,
          geo_id_aggregated = geo_id_aggregated,
          population = population_1,
          # YLD
          dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
          duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
          # Lifetable data
          approach_exposure = approach_exposure_1,
          approach_newborns = approach_newborns_1,
          first_age_pop =  first_age_pop_1,
          last_age_pop = last_age_pop_1,
          deaths_male = deaths_male_1,
          deaths_female = deaths_female_1,
          population_midyear_male = population_midyear_male_1,
          population_midyear_female =  population_midyear_female_1,
          # Discount
          corrected_discount_rate = corrected_discount_rate,
          approach_discount = approach_discount)

      # Compile input data of scenario 2
      input_2 <-
        healthiar::compile_input(
          health_metric = health_metric,
          approach_risk = approach_risk,
          exp_central = exp_central_2, exp_lower = exp_lower_2, exp_upper = exp_upper_2,
          prop_pop_exp = prop_pop_exp_2,
          cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
          bhd_central = bhd_central_2, bhd_lower = bhd_lower_2, bhd_upper = bhd_upper_2,
          rr_central = rr_central,
          rr_lower = rr_lower,
          rr_upper = rr_upper,
          erf_increment = erf_increment,
          erf_shape = erf_shape,
          erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
          min_age = min_age,
          max_age = max_age,
          info = info_2,
          geo_id_raw = geo_id_raw,
          geo_id_aggregated = geo_id_aggregated,
          population = population_2,
          # YLD
          dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
          duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
          # Lifetable data
          approach_exposure = approach_exposure_2,
          approach_newborns = approach_newborns_2,
          first_age_pop =  first_age_pop_2,
          last_age_pop = last_age_pop_2,
          deaths_male = deaths_male_2,
          deaths_female = deaths_female_2,
          population_midyear_male = population_midyear_male_2,
          population_midyear_female =  population_midyear_female_2,
          # Discount
          corrected_discount_rate = corrected_discount_rate,
          approach_discount = approach_discount)

      # Identify the arguments scenario specific arguments excluding bhd
      # This will be used for the exceptions in the joining columns
      # Scenario-specific arguments cannot be used as joining columns
      # because we want to keep different columns for scenario_1 and _2
      # bhd and lifetable_with_pop_nest are excluded
      # because they have to be identical in scenario_1 and _2
      # for the pif approach by definition
      scenario_specific_arguments_excluding_bhd <-
        setdiff(scenario_specific_arguments, c("bhd_central", "bhd_lower", "bhd_upper"))


      # Get identical columns to join data frames (as above)
      joining_columns_input <-
        healthiar:::find_joining_columns(
          df1 = input_1,
          df2 = input_2,
          except =  scenario_specific_arguments_excluding_bhd)

      # Merge the input tables by common columns
      input <-
        dplyr::left_join(
          input_1,
          input_2,
          by = joining_columns_input,
          suffix = c("_1", "_2"))


      # Calculate the health impacts for each case (uncertainty, category, geo area...)
      impact_raw <-
        healthiar:::get_impact(
          input = input,
          year_of_analysis = year_of_analysis,
          min_age = min_age,
          max_age = max_age,
          corrected_discount_rate = corrected_discount_rate,
          approach_discount = approach_discount,
          pop_fraction_type = "pif")
      }


      # Organize output
      # Classify the individual results of each scenario in delta and pif method
      # in a list

      output <-
        healthiar:::get_output(impact_raw = impact_raw)

      output[["health_detailed"]][["scenario_1"]] <- impact_raw_1
      output[["health_detailed"]][["scenario_2"]] <- impact_raw_2



    return(output)
  }
