# Title and description

#' Get population for single year with change in air pollution exposure
#'
#' Get population for single year with change in air pollution exposure
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param paf Population attributable fraction
#' @return
#' This function returns a \code{data.frame} with the life table plus an additional column for population one year later without changing air pollution exposure.
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

project_pop_withExp_copy <-
  function(lifetable_withPop, year_of_analysis, paf){


    second_year <- year_of_analysis + 1

    output <-
      lifetable_withPop %>%
      # Add columns with the population and probability of death
      # moving the row to the bottom
      dplyr::mutate(
        population_lag =
          dplyr::lag(!!as.symbol(paste0("population_",
                                        year_of_analysis))),
        death_probability_natural_lag = dplyr::lag(death_probability_natural),
        death_probability_total_lag = dplyr::lag(death_probability_total))%>%
      # Calculate the population the second year (first column after first year)
      # Considering the health effect of air pollution
      dplyr::mutate(.,
                    "population_{second_year}" :=
                      population_lag * death_probability_natural_lag * paf)%>%
      # Remove the lag columns
      dplyr::select(-contains("_lag"))

    return(output)
  }
