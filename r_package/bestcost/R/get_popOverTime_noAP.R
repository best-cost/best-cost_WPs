# Title and description

#' Get population over time without changing air pollution exposure
#'
#' Get population over time without changing air pollution exposure
#' @param lifetable_wPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @return
#' This function returns a \code{data.frame} with the life table plus an additional column for population one year later without changing air pollution exposure.
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
get_popOverTime_noAP <-
function(lifetable_wPop, firstYear_lifetable){

  # Start of the loop
  year_loopStart = firstYear_lifetable + 2
  # End of the loop
  year_loopEnd <- firstYear_lifetable + nrow(lifetable_wPop) - 1

  # Calculate population for the next years without considering the effect of air pollution
  output <-
    lifetable_wPop

  for (yl in year_loopStart : year_loopEnd){
    output[, paste0("population_", yl)] <-
      dplyr::lag(output[, paste0("population_", yl-1)]) *
      (1 - dplyr::lag(output$death_probability_total))
  }


  return(output)

}
