# Load input data
load("../testing/testing_functions/test_project_pop.rda")

# Add the first year of the lifetable to the column name of population
lifetab_withPop <-
  lifetab_withPop %>%
  dplyr::rename(!!paste0("population_", year_of_analysis) := population)


# Calculate population in the next year assuming
# the change in /level of air pollution
# based on the RR
popOverTime <-
  bestcost::project_pop_withExp(
    lifetable_withPop = lifetab_withPop,
    year_of_analysis = year_of_analysis,
    paf = paf)

# Now calculate population over time (for the rest of years)
# without considering air pollution
popOverTime <-
  bestcost::project_pop_noExp(
    lifetable_withPop = popOverTime,
    year_of_analysis = year_of_analysis)
