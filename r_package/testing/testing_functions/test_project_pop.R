#' Get population over time
#' 
# SPECIFY EXACTLY HOW MANY COLUMNS REALLY NEEDED ####
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific)
#' @param year_of_analysis \code{Numeric value} Year of analysis = first year of life table
#' @param paf \code{Numeric value} Population attributable fraction
#' @return
#' This function returns a \code{Data frame} the population over time taking into account probability of dying
#' @import dplyr
#' @import tibble
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

# Load input data
load("../testing/testing_functions/test_project_pop.rda")

# Function call ####
# output_function_call <- bestcost::project_pop(
#   lifetab_withPop = lifetab_withPop,
#   year_of_analysis = year_of_analysis,
#   paf = paf)

# Function test ####

# Add the first year of the life table (i.e. YOA) to the population column name (as a suffix)
lifetab_withPop <-
  lifetab_withPop %>%
  dplyr::rename(!!paste0("population_", year_of_analysis) := population)

# Save input data for script "test_project_pop_withExp" ####
# lifetable_withPop <- lifetab_withPop
# rm(list=setdiff(ls(), c("lifetable_withPop", "year_of_analysis", "paf")))
# save.image("../testing/testing_functions/test_project_pop_withExp.rda")

# Adds the attributable deaths (=pop * prob. dying * PAF) to the exposure in the YOA in the column to the right one row down
popOverTime <-
  bestcost::project_pop_withExp(
    lifetable_withPop = lifetab_withPop,
    year_of_analysis = year_of_analysis,
    paf = paf)

# Save input data for script "test_project_pop_noExp" ####
# lifetable_withPop <- popOverTime
# rm(list=setdiff(ls(), c("lifetable_withPop", "year_of_analysis")))
# save.image("../testing/testing_functions/test_project_pop_noExp.rda")

# Calculate population over time (from year of analysis + 2 until yoa + 99)
# without considering air pollution
popOverTime <-
  bestcost::project_pop_noExp(
    lifetable_withPop = popOverTime,
    year_of_analysis = year_of_analysis)


