# Compare different versions of the loop  where impact_mortality() is called
# Code can be found in "testing_Rpackage.Rmd" in chunk "impact mortality"

load("testing/Performance/environment_before_loop_impact_mortality().Rdata")

library("tidyverse")
library("bestcost")


# Reference ####

impact_mortality_raw <- list()

system.time(
for(i in 1:nrow(input_data_mortality)){
  impact_mortality_raw[[input_data_mortality$year[i]]][[input_data_mortality$crf_id[[i]]]] <-

    bestcost::assess_mortality_lifetable(

      exp = input_data_mortality$exp[i], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
      cf = input_data_mortality$cf[i],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
      crf = unlist(input_data_mortality[i,
                                        c("crf_mean", "crf_lowci", "crf_highci")]) ,
      crf_per = 10,
      crf_rescale_method = "loglinear",
      lifetable_withPop_male = lifetable_withPopulation[["male"]],
      lifetable_withPop_female = lifetable_withPopulation[["female"]],
      firstYear_lifetable = 2019,
      nonNatural_death_male = mortality_data[["male"]], # % of non-natural male deaths in BHD
      nonNatural_death_female = mortality_data[["female"]], # % of non-natural female deaths in BHD
      pollutant = input_data_mortality$pollutant[i],
      age_group = input_data_mortality$outcome_group[i],
      min_age = 20,
      max_age = 0,
      corrected_discount_rate = 0)
}
)

# ~ 35 s

# Alternative 1: pre-define extent of list "impact_mortality_raw" ####
# Run directly after reference

system.time(
  for(i in 1:nrow(input_data_mortality)){
    impact_mortality_raw[[input_data_mortality$year[i]]][[input_data_mortality$crf_id[[i]]]] <-

      bestcost::assess_mortality_lifetable(

        exp = input_data_mortality$exp[i], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
        cf = input_data_mortality$cf[i],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
        crf = unlist(input_data_mortality[i,
                                          c("crf_mean", "crf_lowci", "crf_highci")]) ,
        crf_per = 10,
        crf_rescale_method = "loglinear",
        lifetable_withPop_male = lifetable_withPopulation[["male"]],
        lifetable_withPop_female = lifetable_withPopulation[["female"]],
        firstYear_lifetable = 2019,
        nonNatural_death_male = mortality_data[["male"]], # % of non-natural male deaths in BHD
        nonNatural_death_female = mortality_data[["female"]], # % of non-natural female deaths in BHD
        pollutant = input_data_mortality$pollutant[i],
        age_group = input_data_mortality$outcome_group[i],
        min_age = 20,
        max_age = 0,
        corrected_discount_rate = 0)
  }
)

# ~ 16 s
