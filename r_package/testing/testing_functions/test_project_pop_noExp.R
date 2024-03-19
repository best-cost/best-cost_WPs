#' Project population without changing air pollution exposure
#' @param lifetable_withPop \code{Data frame} with three columns: col 1: age; col 2: age-group specific natural probability of dying; col 3: population per age group
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table
#' @return
#' This function returns a \code{data.frame} the inputted life table plus additional columns with projected populations without changing air pollution exposure 
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

# UNDER @return: shouldn't it read "... without considering air pollution"? #####

# Load input data
load("../testing/testing_functions/test_project_pop_noExp.rda")

# Function call ####
output_function_call <-
  bestcost::project_pop_noExp(
    lifetable_withPop = lifetable_withPop,
    year_of_analysis = year_of_analysis)

# Function test ####

## OLD CODE ####
# Start of the loop
# year_loopStart = year_of_analysis + 2
# # End of the loop
# year_loopEnd <- year_of_analysis + nrow(lifetable_withPop) - 1
# 
# # Calculate population for the next years without considering the effect of air pollution
# output <-
#   lifetable_withPop
# 
# for (yl in year_loopStart : year_loopEnd){
#   output[, paste0("population_", yl)] <-
#     dplyr::lag(output[, paste0("population_", yl-1)]) *
#     (1 - dplyr::lag(output$death_probability_total))
# }

## NEW CODE ####
# Calculate population for the next years without considering the effect of air pollution
for (yl in (year_of_analysis + 2) : (year_of_analysis + nrow(lifetable_withPop) - 1)){
  lifetable_withPop[, paste0("population_", yl)] <-
    dplyr::lag(lifetable_withPop[, paste0("population_", yl-1)]) * # shifts selected column down one row: the last value of the vector is removed, and in first position NA is introduced
    (1 - dplyr::lag(lifetable_withPop$death_probability_total))
}
