# Title and description

#' Compare attributable years of life lost obtained applying a life table approach

#' Calculates the years of life lost between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @param comparison_method \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @param exp_1 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 1.
#' @param exp_2 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 2.
#' @param prop_pop_exp_1 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 1. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param prop_pop_exp_2 \code{Numeric value} or {vector} showing the proportion of population exposed in the scenario 2. The value is a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the central estimate as well as the lower and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param first_age_pop_1 \code{Numeric value} starting age of the youngest age group from population and life table data in the scenario 1.
#' @param last_age_pop_1 \code{Numeric value} ending age of the oldest age group from population and life table data in the scenario 1.
#' @param interval_age_pop_1 \code{Numeric value} of the interval (in years) of each age group from population and life table data in the scenario 1.
#' @param prob_natural_death_male-1 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males in the scenario 1.
#' @param prob_natural_death_female_1 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females in the scenario 1.
#' @param prob_total_death_male_1 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males in the scenario 1.
#' @param prob_total_death_female_1 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females in the scenario 1.
#' @param population_male_1 \code{Numeric vector} containing the mid-year male population for the year of analysis of the scenario 1.
#' @param population_female_1 \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis_1 \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table in the scenario 2.
#' @param first_age_pop_2 \code{Numeric value} starting age of the youngest age group from population and life table data in the scenario 2.
#' @param last_age_pop_2 \code{Numeric value} ending age of the oldest age group from population and life table data in the scenario 2.
#' @param interval_age_pop_2 \code{Numeric value} of the interval (in years) of each age group from population and life table data in the scenario 2.
#' @param prob_natural_death_male_2 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males in the scenario 2.
#' @param prob_natural_death_female_2 \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females in the scenario 2.
#' @param prob_total_death_male_2 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males in the scenario 2.
#' @param prob_total_death_female_2 \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females in the scenario 2.
#' @param population_male_2 \code{Numeric vector} containing the mid-year male population for the year of analysis of the scenario 2.
#' @param population_female_2 \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis_2 \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
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
compare_yll_lifetable_rr <-
  function(comparison_method = "delta",
           exp_1, exp_2,
           prop_pop_exp_1 = 1, prop_pop_exp_2 = 1,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           erf_c = NULL,
           first_age_pop_1, last_age_pop_1, interval_age_pop_1,
           prob_natural_death_male_1, prob_natural_death_female_1,
           prob_total_death_male_1, prob_total_death_female_1,
           population_male_1, population_female_1,
           year_of_analysis_1,
           first_age_pop_2, last_age_pop_2, interval_age_pop_2,
           prob_natural_death_male_2, prob_natural_death_female_2,
           prob_total_death_male_2, prob_total_death_female_2,
           population_male_2, population_female_2,
           year_of_analysis_2,
           min_age = NULL, max_age = NULL,
           info_1 = NULL, info_2 = NULL){



    # Calculate attributable health impacts in the scenario 1
    att_health_1 <-
      bestcost::attribute_yll_lifetable_rr(
        exp = exp_1,
        prop_pop_exp = prop_pop_exp_1,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        first_age_pop = first_age_pop_1,
        last_age_pop = last_age_pop_1,
        interval_age_pop = interval_age_pop_1,
        prob_natural_death_male = prob_natural_death_male_1,
        prob_natural_death_female = prob_natural_death_male_1,
        prob_total_death_male = prob_total_death_male_1,
        prob_total_death_female = prob_total_death_female_1,
        population_male = population_male_1,
        population_female = population_female_1,
        year_of_analysis = year_of_analysis_1,
        max_age = max_age,
        min_age = min_age,
        info = info_1)

    # Calculate attributable health impacts in the scenario 2
    att_health_2 <-
      bestcost::attribute_yll_lifetable_rr(
        exp = exp_2,
        prop_pop_exp = prop_pop_exp_2,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        first_age_pop = first_age_pop_2,
        last_age_pop = last_age_pop_2,
        interval_age_pop = interval_age_pop_2,
        prob_natural_death_male = prob_natural_death_male_2,
        prob_natural_death_female = prob_natural_death_male_2,
        prob_total_death_male = prob_total_death_male_2,
        prob_total_death_female = prob_total_death_female_2,
        population_male = population_male_2,
        population_female = population_female_2,
        year_of_analysis = year_of_analysis_2,
        max_age = max_age,
        min_age = min_age,
        info = info_2)

    if(comparison_method == "delta"){

    # Identify the columns that are common for scenario 1 and 2
    joining_columns <-
      names(att_health_1[["total"]])[! grepl(c("exp|bhd|paf|rr_conc|impact|impact_rounded|info"),
                                             names(att_health_1[["total"]]))]


    # Merge the result tables by common columns
    att_health <-
      dplyr::left_join(
        att_health_1[["total"]],
        att_health_2[["total"]],
        by = joining_columns,
        suffix = c("_1", "_2"))%>%
      # Calculate the delta (difference) between scenario 1 and 2
      dplyr::mutate(impact = impact_1 - impact_2)
    }



    # If the user choose "pif"  as comparison method
    # pif is additonally calculated
    # impact is overwritten with the new values that refer to pif instead of paf
    if(comparison_method == "pif" &
       first_age_pop_1 == first_age_pop_2 &
       last_age_pop_1 == last_age_pop_2 &
       interval_age_pop_1 == interval_age_pop_2 &
       all(prob_natural_death_male_1 == prob_natural_death_male_2) &
       all(prob_natural_death_female_1 == prob_natural_death_female_2) &
       all(prob_total_death_male_1 == prob_total_death_male_2) &
       all(prob_total_death_female_1 == prob_total_death_female_2) &
       all(population_male_1 == population_male_2) &
       all(population_female_1 == population_female_2) &
       year_of_analysis_1 == year_of_analysis_2){

      # Compile input data of scenario 1
      input_1 <-
        bestcost::compile_input(
          exp = exp_1,
          prop_pop_exp = prop_pop_exp_1,
          cutoff = cutoff,
          rr = rr,
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          erf_c = erf_c,
          bhd = NULL,
          min_age = min_age,
          max_age = max_age,
          info = info_1,
          method = paste0("lifetable_rr_corrected"),
          disability_weight = NULL,
          duration = NULL)

      # Compile input data of scenario 2
      input_2 <-
        bestcost::compile_input(
          exp = exp_2,
          prop_pop_exp = prop_pop_exp_2,
          cutoff = cutoff,
          rr = rr,
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          erf_c = erf_c,
          bhd = NULL,
          min_age = min_age,
          max_age = max_age,
          info = info_2,
          method = paste0("lifetable_rr_corrected"),
          disability_weight = NULL,
          duration = NULL)

      # Identify the columns that are common for scenario 1 and 2
      joining_columns <-
        names(input_1)[! grepl(c("exp|prop_pop_exp|info"),
                                               names(input_1))]


      # Merge the input tables by common columns
      input <-
        dplyr::left_join(
          input_1,
          input_2,
          by = joining_columns,
          suffix = c("_1", "_2"))


      # Get PAF and add to the input data frame
      input_risk_pif <-
        bestcost::get_risk_and_pif(input = input)%>%
        #Replace pif with paf to be able to use the lifetable functions
        dplyr::rename(paf=pif)


      # The life table has to be provided as a data.frame (by sex)
      # The first column has to be the age. Second, probability of death. Third, population.
      # Rename column names to standard names

      lifetable_withPop <- list(
        male =
          data.frame(
            age = seq(from = first_age_pop_1,
                      to = last_age_pop_1,
                      by = interval_age_pop_1),
            age_end = seq(from = first_age_pop_1+ interval_age_pop_1,
                          to = last_age_pop_1,
                          by = interval_age_pop_1 + interval_age_pop_1),
            death_probability_natural = prob_natural_death_male_1,
            death_probability_total = prob_total_death_male_1,
            population = population_male_1),

        female =
          data.frame(
            age = seq(from = first_age_pop_1,
                      to = last_age_pop_1,
                      by = interval_age_pop_1),
            age_end = seq(from = first_age_pop_1 + interval_age_pop_1,
                          to = last_age_pop_1,
                          by = interval_age_pop_1 + interval_age_pop_1),
            death_probability_natural = prob_natural_death_female_1,
            death_probability_total = prob_total_death_female_1,
            population = population_female_1))



      # Get population impact ####
      pop_impact <-
        bestcost::get_pop_impact(
          lifetab_withPop = lifetable_withPop,
          year_of_analysis = year_of_analysis_1,
          paf = input_risk_pif[, c("rr_ci", "paf")])


      # Calculate YLLs ####
      att_health <-
        bestcost::get_yll(
          pop_impact = pop_impact,
          year_of_analysis = year_of_analysis_1,
          min_age = min_age,
          max_age = max_age,
          meta = input_risk_pif)$total%>%
        # Replace paf with pif
        dplyr::rename(pif = paf)


    }


      # Round results
      att_health <-
        att_health %>%
        mutate(impact_rounded = round(impact, 0))

   output <- list(total = att_health,
                  detailed = list(scenario_1 = att_health_1,
                                  scenario_2 = att_health_2))


    return(output)
  }

