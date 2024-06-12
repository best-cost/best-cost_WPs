# Title and description

#' Get population over time without changing air pollution exposure
#'
#' Get population over time without changing air pollution exposure
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @return
#' This function returns a \code{data.frame} with the life table plus an additional column for population one year later without changing air pollution exposure.
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export
project_pop_noExp_copy <-
function(lifetable_withPop, year_of_analysis){

  output <-
    lifetable_withPop

  years <- c( (year_of_analysis + 1) : ((year_of_analysis + nrow(lifetable_withPop) - 2)) )
  length_period <- length(years)

  for (i in 0:(length_period-1)){ # i in 0:97
    YEAR <- years[i+1]
    # print(YEAR+1)
    # print(i)
    output[1:((length_period)-i), paste0("population_", YEAR+1)] <-
      output[1:((length_period)-i), paste0("population_", YEAR)] * (1 - output$death_probability_total[(i+2):(length_period+1)])
  }

  return(output)

}
