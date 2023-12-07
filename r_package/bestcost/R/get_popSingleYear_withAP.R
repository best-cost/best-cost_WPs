# Title and description

#' Get population over time with change in air pollution exposure
#'
#' Get population over time with change in air pollution exposure
#' @param lifetable_wPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param nonNatural_death \code{Data frame} with two columns: the first one for age, the second one for the percentage of non-natural deaths (sex specific),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
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
  function(lifetable_wPop, firstYear_lifetable, age_group, af){

    second_year <- firstYear_lifetable + 1

    output <-
      lifetable_wPop %>%
      # Calculate the population the second year (first column after first year)
      # Considering the health effect of air pollution
      dplyr::mutate(
        population_lag =
          dplyr::lag(!!as.symbol(paste0("population_",
                                        firstYear_lifetable))),
        death_probability_natural_lag = dplyr::lag(death_probability_natural),
        death_probability_total_lag = dplyr::lag(death_probability_total))%>%
      # For infants
      {if(age_group %in% "infants")
        dplyr::mutate(.,
                      "population_{second_year}" :=
                        ifelse(age %in% 1,
                               population_lag * death_probability_natural * paf,
                               population_lag*(1-death_probability_total_lag)))
        else .}%>%

      # For adults
      {if(age_group %in% "adults")
        dplyr::mutate(.,
                      "population_{second_year}" :=
                        population_lag * death_probability_natural * paf)

        else .}%>%
      # Remove the lag columns
      dplyr::select(-contains("_lag"))

    return(output)
  }
