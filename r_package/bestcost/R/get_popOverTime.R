# Title and description

#' Get population over time
#'
#' Get population over time
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3.
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param bhd Numeric value showing the baseline health data (incidence of the health outcome in the population),
#' @param crf_per Numeric value showing the increment of the concentration-response function in ug/m3 (usually 10 or 5)
#' @param crf_rescale_method String to choose among "linear" and "loglinear",
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param nonNatural_death \code{Data frame} with two columns: the first one for age, the second one for the percentage of non-natural deaths (sex specific),
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),
#' @return
#' This function returns a \code{data.frame} the population over time taking into account probability of dying
#' @import dplyr
#' @import tibble
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
get_popOverTime <-
  function(exp, cf, crf, crf_per, crf_rescale_method,
           lifetab_withPop, firstYear_lifetable, nonNatural_death,
           age_group){


    second_year <- firstYear_lifetable + 1

    # Calculate population for the next years
    popOverTime_noAP <-
      bestcost::get_popOverTime_noAP(
        lifetable_wPop = lifetab_withPop,
        firstYear_lifetable = firstYear_lifetable,
        year_loopStart = firstYear_lifetable + 1)


    # Calculate population in the next year assuming
    #- a 10ug/m3 reduction in PM (as in STE-2010) or
    #- the actual change in /level of air pollution
    # based on the CRF
    if(crf_rescale_method == "ap10"){
      crf_forPaf <- crf
    }
    if(crf_rescale_method %in% c("loglinear", "linear")){
      crf_forPaf <- rescale_crf(crf, exp$exp, cf$cf, crf_per, method = crf_rescale_method)
    }

    af <- get_paf(crf_forPaf)

    popOverTime_AP <-
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

    # Now calculate population over time (for the rest of years)
    # without considering air pollution
    popOverTime_AP <-
      bestcost::get_popOverTime_noAP(
        lifetable_wPop = popOverTime_AP,
        firstYear_lifetable = firstYear_lifetable,
        year_loopStart = firstYear_lifetable + 2)


    # Difference of population considering and not considering air pollutionn
    popOverTime_diff <-
      tibble::column_to_rownames(  # Add age to rowname
        dplyr::select(popOverTime_AP, age, contains("population_")),
        "age") -
      tibble::column_to_rownames(
        dplyr::select(popOverTime_noAP, age, contains("population_")),
        "age")
    popOverTime_diff <-
      tibble::rownames_to_column(popOverTime_diff, "age")

    output <- list(noAP = popOverTime_noAP,
                   AP = popOverTime_AP,
                   diff = popOverTime_diff)

    return(output)
  }
