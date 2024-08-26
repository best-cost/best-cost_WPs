# ARCHIVED ON 2024-08-22 #######################################################

#' Get probability of dying
#'
#' Calculates the probability of dying (natural, non-natural or all cause deaths) based on population and mortality data for each single year.
#' @name get_prob_dying_by_single_age
#'
#' @param first_age_pop \code{Numeric value} of the first item of the age sequence from population and life table data.
#' @param last_age_pop \code{Numeric value} of the last item of the age sequence from population and life table data.
#' @param interval_age_pop \code{Vector} containing the interval of the age sequence from population and death data.
#' @param population_midyear \code{Vector} containing the mid-year population by age range.
#' @param deaths \code{Vector} containing deaths of the year of analysis by age range.
#' @param fraction_of_year_lived \code{Numeric value} or \code{Vector} containing the fraction of the age interval that was lived by those who died for each age interval. Default value = 0.5, i.e. 50\%, for all age intervals.
#'
#' @return
#' This function returns a \code{data frame} showing the probability of dying by age interval.
#'
#' @examples
#' get_prob_dying_by_single_age(first_age_pop = 0, last_age_pop = 99,
#' population_midyear = rep(5000, 99), deaths = rep(100, 99))
#'
#' @author Alberto Castro
#' @note Experimental function
#' @export

# List the required packages (to be loaded if not yet)
require(dplyr)


# Define the function
get_prob_dying_by_single_age  <-
  function(first_age_pop, last_age_pop, interval_age_pop,
           population_midyear, deaths,
           fraction_of_year_lived = 0.5){

    # Error if length of fraction_of_year_lived > 1
    if (length(fraction_of_year_lived) > 1){
    stop("input to argument fraction_of_year_lived is not single value")
    }

    # Error if length of fraction_of_year_lived > 1
    #stopifnot(length(fraction_of_year_lived) == 1) # OR fraction_of ...

    # If input value for parameter "fraction_of_year_lived" is single value convert to vector
    if (length(fraction_of_year_lived)==1){
      fraction_of_year_lived <-  rep(fraction_of_year_lived,
                                     length(seq(from = first_age_pop,
                                                to = last_age_pop,
                                                by = interval_age_pop)))}

    # Probability of dying using the user-defined age interval
    # The age interval can be 1 and then no further data preparation is needed
    # or the age interval can be more than one and in that case a further step is needed (see below)
    data_original_interval <-
      # Enter input data
      data.frame(
        age_start = seq(from = first_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop),
        age_end = seq(from = first_age_pop + interval_age_pop - 1,
                      to = last_age_pop + interval_age_pop - 1,
                      by = interval_age_pop),
        population_midyear = population_midyear,
        deaths = deaths,
        fraction_of_year_lived = fraction_of_year_lived) %>%

      # Make calculation
      dplyr::mutate(
        death_rate = deaths / population_midyear,

        # The probability of dying is defined as
        # the number of people dying in a year divided by
        # the number of people living at the beginning of the year (entry_population)
        # Simplified (divided by population), the following equation can be used
        # Formula from AirQ+ Manual for life tables
        # which refers to this WHO report
        # https://iris.who.int/bitstream/handle/10665/108463/E74256.pdf?sequence=1&isAllowed=y
        # Alternative source where the formulas are also nicely defined:
        # https://www.statsdirect.com/help/survival_analysis/abridged_life_table.htm
        prob_dying = (death_rate * interval_age_pop) /
          (1 + ((1-fraction_of_year_lived) * death_rate * interval_age_pop)),

        prob_surviving = 1 - prob_dying)

    # Store as function output
    output <- data_original_interval

    # If the user-defined age interval is higher than 1,
    if(interval_age_pop > 1){
      # this second step is needed to standardize to single-year age interval
      names(data_original_interval) <-
        paste0(names(data_original_interval), "_group")

      # Create data frame with the start and end age of single years intervals
      # and the the start year of the multiple year to use it as link for the join
      data_interpolated <-
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
        # Add information from the table with multiple-year interval
        dplyr::left_join(., data_original_interval,
                         by = "age_start_group") %>%
        # Rename fraction of year lived from multiple to single year
        # (it is the same for single year as for multiple year)
        dplyr::rename("fraction_of_year_lived" = "fraction_of_year_lived_group") %>%
        # Calculate probability of dying
        dplyr::mutate(
          prob_dying =
            death_rate_group /
            (1 + (1-fraction_of_year_lived)*death_rate_group),
          prob_surviving =
            1 - prob_dying,
          is_first_row_group =
            !duplicated(age_start_group))%>%
        # Calculate population_entry, deaths and population_midyear for each row
        # Actually, we just need to edit the first row but we edit all because
        # the rest of the rows are edited again below.
        dplyr::rowwise() %>%
        dplyr::mutate(
          population_entry =
            ifelse(is_first_row_group,
                   population_midyear_group /
                     sum(
                       c(0.5,
                         prob_surviving^(1:(interval_age_pop-1)),
                         0.5 * prob_surviving^interval_age_pop)),
                   NA),
          deaths =
            population_entry * prob_dying,
          population_midyear =
            population_entry - (1-fraction_of_year_lived) * deaths)

      # Now only for rows that are not the first item/row for each group
      for(i in which(!data_interpolated$is_first_row_group)){
        data_interpolated$population_entry[i] <-
          data_interpolated$population_entry[i-1] - data_interpolated$deaths[i-1]

        data_interpolated$deaths[i] <-
          data_interpolated$population_entry[i] * data_interpolated$prob_dying[i]

        data_interpolated$population_midyear <-
          data_interpolated$population_entry[i] -
          (1-data_interpolated$fraction_of_year_lived) *
          data_interpolated$deaths

      }
      # Store it as the new function output
      output <- data_interpolated
    }



    # Prepare output of the function
    output <-
      output %>%
      dplyr::select(age_start, age_end, fraction_of_year_lived,
                    deaths, population_midyear,
                    prob_dying, prob_surviving)


    return(output)
  }
