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
           lifetab_withPop, firstYear_lifetable,
           age_group){


    # Add the first year of the lifetable to the column name of population
    lifetab_withPop <-
      dplyr::rename(setNames(population,
                             paste0("population_", firstYear_lifetable)))


    # Calculate population in the next year assuming
    # the change in /level of air pollution
    # based on the CRF
    crf_forPaf <- rescale_crf(crf, exp$exp, cf$cf, crf_per, method = crf_rescale_method)
    paf <- bestcost::get_paf(crf_forPaf)

    popOverTime <-
      bestcost::get_popSingleYear_withAP(
        lifetable_wPop = lifetab_withPop,
        firstYear_lifetable = firstYear_lifetable,
        age_group = age_group,
        paf = paf)

    # Now calculate population over time (for the rest of years)
    # without considering air pollution
    popOverTime <-
      bestcost::get_popOverTime_noAP(
        lifetable_wPop = popOverTime_AP,
        firstYear_lifetable = firstYear_lifetable)

    output <- popOverTime

    return(output)
  }
