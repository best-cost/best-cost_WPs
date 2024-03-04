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
#' @param cases \code{Numeric vector} of number of new cases (incidence) of the morbidity health outcome in the year of analysis
#' @param dw \code{Numeric value} showing the disability weight associated with the morbidity health outcome
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
           prob_natural_death_male, prob_natural_death_female, prob_total_death_male, prob_total_death_female,
           population_male, population_female,
           year_of_analysis,
           corrected_discount_rate = 0, min_age = NULL, max_age = NULL,
           cases,
           info = NULL){

    # Check input data ####
    stopifnot(exprs = {
      length(exp) == length(prop_pop_exp)
    })

    # Convert NULL into NA in min_age and max_age
    min_age <- ifelse(is.null(min_age), NA, min_age)
    max_age <- ifelse(is.null(max_age), NA, max_age)

    # Input data in data frame ####
    # Compile rr data to assign categories
    input_withPaf <-
      data.frame(
        info = info,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        # Assign mean, low and high rr values
        rr_ci = ifelse(rr %in% min(rr), "low",
                       ifelse(rr %in% max(rr), "high",
                              "mean")),
        cutoff = cutoff,
        # Information derived from input data
        approach_id = paste0("lifetable_", erf_shape),
        age_range = ifelse(!is.na(max_age), paste0("below", max_age + 1),
                           ifelse(!is.na(min_age), paste0("from", min_age),
                                  NA))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) %>%
      # Add exposure categories to rr's with a cross join to produce all likely combinations between exp categories and rr estimates (central, upper & lower CI)
      dplyr::cross_join(., data.frame(exp = exp,
                                      prop_pop_exp = prop_pop_exp)) %>%
      # rescale rr's for PAF
      dplyr::mutate(
        rr_forPaf =
          bestcost::get_risk(rr = rr,
                             exp = exp,
                             cutoff = cutoff,
                             rr_increment = rr_increment,
                             erf_shape = {{erf_shape}}),
        #{{}} ensures that the
        # value from the function argument is used
        # instead of from an existing column
        # Add a column for the average exp (way to summarize exposure)
        exp_mean = mean(exp)) #%>%
    # # Group by exp in case that there are different exposure categories
    # dplyr::group_by(rr) %>%
    # dplyr::mutate(paf = bestcost::get_paf(rr_conc = rr_forPaf,
    #                                       prop_pop_exp = prop_pop_exp))

    # Calculate population attributable fraction (PAF) ####
    paf <-
      input_withPaf %>%
      # Group by exp in case that there are different exposure categories
      dplyr::group_by(rr) %>%
      # Calculate PAFs per row & then reduce nrow by summing PAFs belonging to same rr
      dplyr::summarize(paf = bestcost::get_paf(rr_conc = rr_forPaf,
                                               prop_pop_exp = prop_pop_exp))

    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    if(length(exp)>1){
      input_withPaf <-
        input_withPaf %>%
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_mean = mean(exp),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp = paste(exp, collapse = ", "),
          prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
          rr_forPaf = paste(rr_forPaf, collapse = ", "))%>%
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    # Join the input table with paf values
    input_withPaf <-
      input_withPaf %>%
      dplyr::left_join(paf,
                       input_withPaf,
                       by = "rr")





  }
