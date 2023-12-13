# Title and description

#' Get population for single year with change in air pollution exposure
#'
#' Get population for single year with change in air pollution exposure
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),
#' @param paf Attributable fraction
#' @return
#' This function returns a \code{data.frame} with the life table plus an additional column for population one year later without changing air pollution exposure.
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function

get_popSingleYear_withAP <-
  function(lifetable_withPop, year_of_analysis, age_group, paf){


    second_year <- year_of_analysis + 1

    output <-
      lifetable_withPop %>%
      # Calculate the population the second year (first column after first year)
      # Considering the health effect of air pollution
      dplyr::mutate(
        population_lag =
          dplyr::lag(!!as.symbol(paste0("population_",
                                        year_of_analysis))),
        death_probability_natural_lag = dplyr::lag(death_probability_natural),
        death_probability_total_lag = dplyr::lag(death_probability_total))%>%
      # For infants
      {if(age_group %in% "infants")
        dplyr::mutate(.,
                      "population_{second_year}" :=
                        ifelse(age %in% 1,
                               population_lag * death_probability_natural_lag * paf,
                               population_lag*(1-death_probability_total_lag)))
        else .}%>%

      # For adults
      {if(age_group %in% "adults")
        dplyr::mutate(.,
                      "population_{second_year}" :=
                        population_lag * death_probability_natural_lag * paf)

        else .}%>%
      # Remove the lag columns
      dplyr::select(-contains("_lag"))

    return(output)
  }
