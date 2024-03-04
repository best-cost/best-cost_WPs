# Input data
load("../testing/testing_functions/test_get_pop_impact.rda")
lifetab_withPop <- lifetable_withPop
rm(lifetable_withPop)

# Function code
ci <- c("mean", "lowci", "highci") # variable used in code
sex <- c("female","male")

# Get popOvertime
popOverTime <- list()

for(s in sex){
  for(v in ci){
    popOverTime[[s]][[v]] <-
      bestcost::project_pop(
        lifetab_withPop = lifetab_withPop[[s]],
        year_of_analysis = year_of_analysis,
        paf = paf$paf[paf$ci %in% v])
  }
}


# Get pop_impact

pop_impact <- list()

for(s in sex){
  for(v in ci){
    pop_impact[[s]][[v]] <-
      
      bestcost::move_rows_up(popOTime = popOverTime[[s]][[v]],
                             year_of_analysis = year_of_analysis)
  }
}

output <-
  list(paf = paf,
       popOverTime = popOverTime,
       pop_impact = pop_impact)