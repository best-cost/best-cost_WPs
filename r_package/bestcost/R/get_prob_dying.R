# Title and description

#' Get probability of dying
#'
#' Calculates the probability of dying (natural, non-natural or all cause deaths) based on population and mortality data.
#' @param first_age_pop \code{Numberic value} of the first item of the age sequence from population and life table data.
#' @param last_age_pop \code{Numberic value} of the last item of the age sequence from population and life table data.
#' @param interval_age_pop \code{Vector} containing the interval of the age sequence from population and death data.
#' @param population \code{Vector} containing the mid-year population by age range.
#' @param deaths \code{Vector} containing deaths of the year of analysis by age range.
#' @param fraction_of_year_lived \code{Vector} containing the fraction of the age interval that was lived by those who died for each age interval. Default value = 0.5 (i.e. 50%) for all age intervals.
#'
#' @return
#' This function returns a \code{data frame} showing the probability of dying by age interval.
#'
#' @examples
#' get_prob_dying(first_age_pop = 0, last_age_pop = 99, interval_age_pop = 1, population = rep(5000, 99), deaths = rep(100, 99))
#'
#' @author Alberto Castro
#' @note Experimental function
#' @export

require(dplyr)

get_prob_dying <-
  function(first_age_pop, last_age_pop, interval_age_pop,
           population, deaths,
           fraction_of_year_lived =
             rep(0.5,
                 length(seq(from = first_age_pop,
                            to = last_age_pop,
                            by = interval_age_pop)))){

    by_original_interval <-
      # Enter input data
      data.frame(
        age_start = seq(from = first_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop),
        age_end = seq(from = first_age_pop + interval_age_pop,
                      to = last_age_pop,
                      by = interval_age_pop + interval_age_pop),
        population = population,
        deaths = deaths,
        fraction_of_year_lived = fraction_of_year_lived) %>%
      # Make calculation
      dplyr::mutate(
        death_rate = deaths / population,
        # The probability of dying is defined as
        # the number of people dying in a a year divided by
        # the number of people living at the beginning of the year (entry_population)
        # Simplified (divided by population), the following equation can be used
        # Formula from AirQ+ Manual for life tables
        # which refers to this WHO report
        # https://iris.who.int/bitstream/handle/10665/108463/E74256.pdf?sequence=1&isAllowed=y
        # Alternative source where the formulas are also nicely defined:
        # https://www.statsdirect.com/help/survival_analysis/abridged_life_table.htm
        prob_dying = (death_rate * interval_age_pop) /
          1 + ((1-fraction_of_year_lived) * death_rate * interval_age_pop),
        prob_surviving = 1 - prob_dying)

    by_single_year <-
      data.frame(
        age_start = seq(from = first_age_pop,
                        to = last_age_pop,
                        by = 1),
        age_end = seq(from = first_age_pop,
                      to = last_age_pop,
                      by = 1),
        age_start_group = rep(seq(from = first_age_pop,
                              to = last_age_pop,
                              by = interval_age_pop),
                              each = interval_age_pop))%>%




    return(output)
  }
