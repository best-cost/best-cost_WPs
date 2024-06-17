# Title and description

#' Attributable deaths based on life tables
#'
#' Calculates the premature deaths attributable to the exposure to an environmental stressor using a life table approach. It provides the central estimate of the impact and the corresponding 95\% confidence intervals (based on the 95\% confidence interval exposure-response function).

#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data
#' @param prob_natural_death_male \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for males.
#' @param prob_natural_death_female \code{Numeric vector} containing the probability of dying due to natural cause (excluding non-natural deaths due to violence or accidents) by age or age group for females.
#' @param prob_total_death_male \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for males.
#' @param prob_total_death_female \code{Numeric vector} containing the probability of dying due to all causes (including non-natural deaths due to violence or accidents) by age or age group for females.
#' @param population_midyear_male \code{Numeric vector} containing the mid-year male population for the year of analysis.
#' @param population_midyear_female \code{Vector} containing the mid-year female population for the year of analysis.
#' @return
#'
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#'
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal
#'
#'
compile_lifetable_pop <-
  function(first_age_pop, last_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_midyear_male, population_midyear_female){

    # The life table has to be provided as a data.frame (by sex)
    # The first column has to be the age. Second, probability of death. Third, population.
    # Rename column names to standard names

    lifetable_with_pop <- list(
      male =
        tidyr::tibble(
          age = seq(from = first_age_pop,
                    to = last_age_pop,
                    by = 1),
          age_end = seq(from = first_age_pop + 1,
                        to = last_age_pop + 1,
                        by = 1),
          death_probability_natural = prob_natural_death_male,
          death_probability_total = prob_total_death_male,
          population = population_midyear_male),

      female =
        tidyr::tibble(
          age = seq(from = first_age_pop,
                    to = last_age_pop,
                    by = 1),
          age_end = seq(from = first_age_pop + 1,
                        to = last_age_pop + 1,
                        by = 1),
          death_probability_natural = prob_natural_death_female,
          death_probability_total = prob_total_death_female,
          population = population_midyear_female))

  }
