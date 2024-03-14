# Input data
load("../testing/testing_functions/test_get_pop_impact.rda")

# Function code ####
ci <- c("mean", "lowci", "highci") # variable used in code
sex <- c("female","male")

## Get popOvertime ####
popOverTime <- list()

# Save input data for script "test_get_pop_impact"
# paf <- as.numeric(paf[paf$ci=="mean", "paf"]) # Save paf "mean"
# lifetab_withPop <- lifetab_withPop[["female"]] # Save pop "female"
# rm(list=setdiff(ls(), c("lifetab_withPop", "year_of_analysis", "paf")))
# save.image("../testing/testing_functions/test_project_pop.rda")

for(s in sex){
  for(v in ci){
    popOverTime[[s]][[v]] <-
      # Projects the input population 100 years into the future (e.g. if start year 2019 --> end year 2118)
      bestcost::project_pop(
        lifetab_withPop = lifetab_withPop[[s]],
        year_of_analysis = year_of_analysis,
        paf = paf$paf[paf$ci %in% v])
  }
}

## Get pop_impact ####

# Save input data for script "test_move_rows_up"
# popOverTime <- popOverTime[["female"]][["mean"]] # Save pop "female" & "mean"
# rm(list=setdiff(ls(), c("popOverTime", "year_of_analysis")))
# save.image("../testing/testing_functions/test_move_rows_up.rda")

pop_impact <- list()

for(s in sex){
  for(v in ci){
    pop_impact[[s]][[v]] <-
      
      bestcost::move_rows_up(popOTime = popOverTime[[s]][[v]],
                             year_of_analysis = year_of_analysis)
  }
}

pop_impact <- #output <-
  list(paf = paf,
       popOverTime = popOverTime,
       pop_impact = pop_impact)
