# Title and description

#' Get population over time with change in air pollution exposure
#'
#' Get population over time with change in air pollution exposure
#' @param lifetable_wPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param nonNatural_death \code{Data frame} with two columns: the first one for age, the second one for the percentage of non-natural deaths (sex specific),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),
#' @return
#' This function returns a \code{data.frame} with the life table plus an additional column for population one year later without changing air pollution exposure.
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function

get_popOverTime_withAP <-
  function(lifetable_wPop, nonNatural_death, firstYear_lifetable, age_group){

    output <-
      # Add column with moving average percent of non-natural deaths
      dplyr::left_join(lifetab_withPop,
                       nonNatural_death[, c("age", "percent_nonNatural")],
                       by = "age")%>%
      # Calculate the population the second year (first column after first year)
      # Considering the health effect of air pollution
      dplyr::mutate(
        population_lag =
          dplyr::lag(!!as.symbol(paste0("population_",
                                        firstYear_lifetable))),
        death_probability_lag = dplyr::lag(death_probability),
        percent_nonNatural_lag = dplyr::lag(percent_nonNatural))%>%
      # For infants
      {if(age_group %in% "infants")
        dplyr::mutate(.,
                      "population_{second_year}" :=
                        ifelse(age %in% 1,
                               population_lag *
                                 # total population
                                 (1-
                                    #minus non-natural deaths
                                    death_probability_lag*percent_nonNatural_lag -
                                    # minus air pollution deaths
                                    death_probability_lag*(1-percent_nonNatural_lag)*(1-af)),
                               population_lag*(1-death_probability_lag)))
        else .}%>%

      # For adults
      {if(age_group %in% "adults")
        dplyr::mutate(.,
                      "population_{second_year}" :=
                        population_lag *
                        # total population
                        (1-
                           #minus non-natural deaths
                           death_probability_lag*percent_nonNatural_lag -
                           # minus air pollution deaths
                           death_probability_lag*(1-percent_nonNatural_lag)*(1-af)))

        else .}%>%
      # Remove the lag columns
      dplyr::select(-contains("_lag"))

    return(output)
  }
