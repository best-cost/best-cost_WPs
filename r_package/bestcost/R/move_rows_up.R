# Title and description

#' Move rows up
#'
#' Move rows up
#' @param popOTime Population over time,
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @return
#' This function returns a shifted value of the population over time taking into account probability of dying
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function

move_rows_up <-
  function(popOTime, firstYear_lifetable){

    # Start of the loop
    year_loopStart <- firstYear_lifetable+1
    # End of the loop
    year_loopEnd <- firstYear_lifetable + nrow(popOTime) - 1

    years <- year_loopStart:year_loopEnd

    output <- popOTime

    for(i in (1:length(years))){
      # Calculate population for the next years without considering the effect of air pollution
      # Calculate population in the next years based on the row above

      y <- years[i]

      output[, paste0("population_", y)] <-
        dplyr::lead(output[, paste0("population_", y)], n = i)

    }

    # Age as numeric
    output$age <- as.numeric(output$age)


    return(output)

  }
