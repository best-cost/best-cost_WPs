# Title and description

#' Calculation of Health Impacts
#'
#' Calculates the mortality, i.e. premature deaths or years of life lost, attributed to the exposure to an environmental stressor using a life table approach. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {vector] showing the proportion of population exposed (as fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param crf_per \code{Numberic value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param crf_rescale_method \code{String} to choose among "linear" and "loglinear".
#' @param first_age_pop \code{Numberic value} of the first item of the age sequence from population and life table data
#' @param last_age_pop \code{Numberic value} of the last item of the age sequence from population and life table data
#' @param interval_age_pop \code{Numberic value} of the interval of the age sequence from population and life table data
#' @param prob_natural_death_male \code{Vector} containing the probability of dying because of natural cause (excluding non-natural deaths such as violence or accidents) by age or age group for male.
#' @param prob_natural_death_female \code{Vector} containing the probability of dying because of natural cause (excluding non-natural deaths such as violence or accidents) by age or age group for female.
#' @param prob_total_death_male \code{Vector} containing the probability of dying because of all causes (including non-natural deaths such as violence or accidents) by age or age group for male.
#' @param prob_total_death_female \code{Vector} containing the probability of dying because of all causes (including non-natural deaths such as violence or accidents) by age or age group for female.
#' @param population_male \code{Vector} containing the mid-year male population for the year of analysis.
#' @param population_female \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old).
#' @param corrected_discount_rate Numeric value with the corrected discount rate as proportion (i.e. 0.1 instead of 10\%).
#' @param info_pollutant \code{String} showing additional information or id for the pollutant. Default value = NULL.
#' @param info_outcome \code{String} showing additional information or id for the health outcome. Default value = NULL.
#' @param info_exp \code{String} showing additional information or id for the exposure. This information will be added to all rows of the results. Default value = NULL.
#' @param info_cf \code{String} showing additional information or id for counter-factual scenario (cut-off). This information will be added to all rows of the results. Default value = NULL.
#' @param info_crf \code{String} showing additional information or id for the concentration-response function. This information will be added to all rows of the results. Default value = NULL.
#' @param info_bhd \code{String} showing additional information or id for the baseline health data. This information will be added to all rows of the results. Default value = NULL.
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. mean, lower and upper bound confidence interval.
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
assess_mortality_lifetable <-
  function(exp, prop_pop_exp = 1,
           cf,
           crf, crf_per, crf_rescale_method,
           first_age_pop, last_age_pop, interval_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_male, population_female,
           year_of_analysis,
           corrected_discount_rate = 0,
           min_age = NULL, max_age = NULL,
           info_pollutant = NULL, info_outcome = NULL,
           info_exp = NULL, info_cf = NULL, info_crf = NULL, info_bhd = NULL){


    # Digest input data

    # Convert NULL into NA
    min_age <- ifelse(is.null(min_age), NA, min_age)
    max_age <- ifelse(is.null(max_age), NA, max_age)


    # Input data in data frame
    input_info <-
      expand.grid(
        crf = crf,
        exp = exp,
        cf = cf,
        crf_per = crf_per,
        crf_rescale_method = crf_rescale_method,
        # Information derived from input data
        approach_id = paste0("lifetable_", crf_rescale_method),
        age_range = ifelse(!is.na(max_age), paste0("below", max_age + 1),
                           ifelse(!is.na(min_age), paste0("from", min_age),
                                  NA))) %>%

      # Add additional information (info_x variables)
      dplyr::mutate(
        info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
        info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
        info_exp = ifelse(is.null(info_exp), NA, info_exp),
        info_cf = ifelse(is.null(info_cf), NA, info_cf),
        info_crf = ifelse(is.null(info_crf), NA, info_crf),
        info_bhd = ifelse(is.null(info_bhd), NA, info_bhd))


    # Calculate crf estimate which corresponds to the exposure
    # depending on the method
    input_info_paf <-
      input_info %>%
      dplyr::mutate(
        crf_forPaf =
          rescale_crf(crf = crf,
                      exp = exp,
                      cf = cf,
                      crf_per = crf_per,
                      method ={{crf_rescale_method}}
                      #{{}} ensures that the
                      # value from the function argument is used
                      # instead of from an existing column
                      ),
        crf_ci = ifelse(crf %in% min(crf), "low",
                        ifelse(crf %in% max(crf), "high",
                               "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(crf), "mean", ci)) %>%

      # Calculate attributable fraction (AF) as well as impact
      dplyr::mutate(approach_id = paste0("singleValue_", crf_rescale_method),
                    paf =  bestcost::get_paf(crf_conc = crf_forPaf))


    # The life table has to be provided as a data.frame (by sex)
    # The first column has to be the age. Second, probability of death. Third, population.
    # Rename column names to standard names

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


    # Get population impact
    shifted_popOverTime <-
      bestcost::get_pop_impact(
        lifetab_withPop = lifetable_withPop,
        year_of_analysis = year_of_analysis,
        paf = input_info_paf[, c("ci", "paf")])


    # Calculate deaths
    deaths <-
      bestcost::get_deaths(
        shifted_popOverTime = shifted_popOverTime,
        year_of_analysis = year_of_analysis,
        min_age = min_age,
        max_age = max_age,
        meta = input_info_paf)

    # Calculate years of life lost (yll)
    yll <-
      bestcost::get_yll(
        shifted_popOverTime = shifted_popOverTime,
        year_of_analysis = year_of_analysis,
        min_age = min_age,
        max_age = max_age,
        meta = input_info_paf,
        corrected_discount_rate = corrected_discount_rate)


    output <-
      list(
        shifted_popOverTime = shifted_popOverTime,
        deaths_long = deaths[["deaths_long"]],
        deaths = deaths[["deaths"]],
        yll_long = yll[["yll_long"]],
        yll = yll[["yll"]])

    return(output)

  }





