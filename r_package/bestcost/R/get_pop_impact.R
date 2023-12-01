# Title and description

#' Get population impact over time
#'
#' Get population impact over time
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3.
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param crf_per Numeric value showing the increment of the concentration-response function in ug/m3 (usually 10 or 5)
#' @param crf_rescale_method String to choose among "linear" and "loglinear",
#' @param lifetable_withPop_male \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param nonNatural_death \code{Data frame} with two columns: the first one for age, the second one for the percentage of non-natural deaths (sex specific),
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),

#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. mean, lower and upper bound confidence interval.
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function


get_pop_impact <-
  function(exp, cf, crf, crf_per, crf_rescale_method,
           lifetab_withPop, nonNatural_death, firstYear_lifetable,
           age_group){

    # Get popOvertime
    popOverTime <- list()

    for(s in sex){
      for(v in ci){
        popOverTime[[s]][[v]] <-
          bestcost::get_popOverTime( # AL: replaced healthiar with bestcost ####
            lifetab_withPop = lifetab_withPop[[s]],
            nonNatural_death = nonNatural_death[[s]],
            firstYear_lifetable = firstYear_lifetable,
            crf = crf$crf[crf$ci %in% v],
            exp = exp,
            cf = cf,
            crf_per = crf_per,
            crf_rescale_method = crf_rescale_method,
            age_group = age_group)
      }
    }


    # Get shifted_popOverTime

    shifted_popOverTime <- list()

    for(s in sex){
      for(v in ci){
        shifted_popOverTime[[s]][[v]] <-
          bestcost::move_rows_up(popOTime = popOverTime[[s]][[v]][["diff"]], # AL: replaced healthiar with bestcost ####
                                        firstYear_lifetable = firstYear_lifetable)
      }
    }

    output <-
      list(crf = crf,
           popOverTime = popOverTime,
           shifted_popOverTime = shifted_popOverTime)


    return(output)

  }
