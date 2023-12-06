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

    af <- bestcost::get_paf(crf_forPaf)
    # After the exposure to air pollution (first year),
    # get the population for the year after (second year)
    popOverTime_AP <-
      bestcost::get_popSingleYear_withAP(
        lifetable_wPop = lifetab_withPop,
        nonNatural_death = nonNatural_death,
        firstYear_lifetable = firstYear_lifetable,
        age_group = age_group,
        af = af)

    # Now calculate population over time (for the rest of years)
    # without considering air pollution
    popOverTime_AP <-
      bestcost::get_popOverTime_noAP(
        lifetable_wPop = popOverTime_AP,
        firstYear_lifetable = firstYear_lifetable,
        year_loopStart = firstYear_lifetable + 2)


    # Difference of population considering and not considering air pollution
    popOverTime_diff <-
      bestcost::get_pop_diff(popOverTime_AP = popOverTime_AP,
                             popOverTime_noAP = popOverTime_noAP)

    output <- list(noAP = popOverTime_noAP,
                   AP = popOverTime_AP,
                   diff = popOverTime_diff)

    return(output)
  }
