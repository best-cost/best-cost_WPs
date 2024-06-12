# Title and description

#' Move rows up
#'
#' Move rows up
#' @param popOTime Population over time,
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @return
#' This function returns a shifted value of the population over time taking into account probability of dying
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

# CONTINUE HERE: TRY TO MAKE FUNCTION RUN WITH NEW CODE BELOW INSTEAD OF ORIGINAL CODE
# move_rows_up <-
#   function(popOTime, year_of_analysis){
#
#     years <- c( (year_of_analysis+1) : (year_of_analysis + nrow(popOTime) - 1) ) # vector indicating which year columns have to be shifted upwards
#
#     for(i in (1:length(years))){ # i.e. length(year_loopStart:year_loopEnd)
#
#       y <- years[i]
#
#       popOTime[, paste0("population_", y)] <-
#         dplyr::lead(popOTime[, paste0("population_", y)], n = i)
#
#     }
#
#     return(popOTime)
#
#   }

# Original code
move_rows_up <-
  function(popOTime, year_of_analysis){

    # Start of the loop
    year_loopStart <- year_of_analysis+1
    # End of the loop
    year_loopEnd <-
      year_of_analysis + length(grep(x = colnames(popOTime), pattern = "population_")) - 1

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
