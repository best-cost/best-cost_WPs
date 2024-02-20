# Title and description

#' Get population over time
#'
#' Get population over time
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param paf \code{Data frame} with three rows (mean, lower bound and upper bound)
#' @return
#' This function returns a \code{data.frame} the population over time taking into account probability of dying
#' @import dplyr
#' @import tibble
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export
get_popOverTime <-
  function(lifetab_withPop, year_of_analysis, paf){


    # Add the first year of the lifetable to the column name of population
    lifetab_withPop <-
      lifetab_withPop %>%
      dplyr::rename(!!paste0("population_", year_of_analysis) := population)


    # Calculate population in the next year assuming
    # the change in /level of air pollution
    # based on the RR
    popOverTime <-
      bestcost::get_popSingleYear_withAP(
        lifetable_withPop = lifetab_withPop,
        year_of_analysis = year_of_analysis,
        paf = paf)

    # Now calculate population over time (for the rest of years)
    # without considering air pollution
    popOverTime <-
      bestcost::get_popOverTime_noAP(
        lifetable_withPop = popOverTime,
        year_of_analysis = year_of_analysis)

    output <- popOverTime

    return(output)
  }
