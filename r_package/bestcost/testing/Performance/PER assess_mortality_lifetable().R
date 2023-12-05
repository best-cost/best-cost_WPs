# Compare different versions of the loop  where impact_mortality() is called
# Code can be found in "testing_Rpackage.Rmd" in chunk "impact mortality"

load("testing/input/performance/PER assess_mortality_lifetable() data.Rdata")
library("tidyverse") ; library("bestcost")

# Single call: Evaluate time for calling assess_mortality_life one time ####
sex <-  c("male", "female") # Define sex (can be removed once included in function code)
ci <- c("mean", "lowci", "highci") # Define values in the confidence interval (ci) (can be removed once included in function code)

profvis(
  bestcost::assess_mortality_lifetable(

  exp = input_data_mortality$exp[1], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
  cf = input_data_mortality$cf[1],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
  crf = unlist(input_data_mortality[1,
                                    c("crf_mean", "crf_lowci", "crf_highci")]) ,
  crf_per = 10,
  crf_rescale_method = "loglinear",
  lifetable_withPop_male = lifetable_withPopulation[["male"]],
  lifetable_withPop_female = lifetable_withPopulation[["female"]],
  firstYear_lifetable = 2019,
  nonNatural_death_male = mortality_data[["male"]], # % of non-natural male deaths in BHD
  nonNatural_death_female = mortality_data[["female"]], # % of non-natural female deaths in BHD
  pollutant = input_data_mortality$pollutant[1],
  age_group = input_data_mortality$outcome_group[1],
  min_age = 20,
  max_age = 0,
  corrected_discount_rate = 0),
  interval = 0.005,
  prof_output = "testing/output/performance/assess_mortality_lifetable() single call.Rprof"
  )

# 1.3 s (for single call)

# Whole loop: ####

impact_mortality_raw <- list()

profvis(for(i in 1:nrow(input_data_mortality)){
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
},
interval = 0.005,
prof_output = "testing/output/performance/assess_mortality_lifetable() whole loop.Rprof"
)

# ~15 s (for whole loop)



# Check the output(s) of the assess_mortality_lifetable() function ####
output1 <- bestcost::assess_mortality_lifetable(

  exp = input_data_mortality$exp[1], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
  cf = input_data_mortality$cf[1],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
  crf = unlist(input_data_mortality[1,
                                    c("crf_mean", "crf_lowci", "crf_highci")]) ,
  crf_per = 10,
  crf_rescale_method = "loglinear",
  lifetable_withPop_male = lifetable_withPopulation[["male"]],
  lifetable_withPop_female = lifetable_withPopulation[["female"]],
  firstYear_lifetable = 2019,
  nonNatural_death_male = mortality_data[["male"]], # % of non-natural male deaths in BHD
  nonNatural_death_female = mortality_data[["female"]], # % of non-natural female deaths in BHD
  pollutant = input_data_mortality$pollutant[1],
  age_group = input_data_mortality$outcome_group[1],
  min_age = 20,
  max_age = 0,
  corrected_discount_rate = 0)

output2 <- bestcost::assess_mortality_lifetable(

  exp = input_data_mortality$exp[2], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
  cf = input_data_mortality$cf[2],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
  crf = unlist(input_data_mortality[2,
                                    c("crf_mean", "crf_lowci", "crf_highci")]) ,
  crf_per = 10,
  crf_rescale_method = "loglinear",
  lifetable_withPop_male = lifetable_withPopulation[["male"]],
  lifetable_withPop_female = lifetable_withPopulation[["female"]],
  firstYear_lifetable = 2019,
  nonNatural_death_male = mortality_data[["male"]], # % of non-natural male deaths in BHD
  nonNatural_death_female = mortality_data[["female"]], # % of non-natural female deaths in BHD
  pollutant = input_data_mortality$pollutant[2],
  age_group = input_data_mortality$outcome_group[2],
  min_age = 20,
  max_age = 0,
  corrected_discount_rate = 0)


<<<<<<< HEAD:r_package/bestcost/testing/Performance/PER assess_mortality_lifetable().R
=======
# Reference (with profvis ####
one_call <- function(){
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
  }}

profvis(one_call(),
        interval = 0.005,
        prof_output = "testing/output/performance/assess_mortality_lifetable() whole loop.Rprof")

# 33 s
# no overview at all


# Evaluate


# Alternative: evaluate time for calling function one time ####

profvis(
  bestcost::assess_mortality_lifetable(

  exp = input_data_mortality$exp[1], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
  cf = input_data_mortality$cf[1],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
  crf = unlist(input_data_mortality[1,
                                    c("crf_mean", "crf_lowci", "crf_highci")]) ,
  crf_per = 10,
  crf_rescale_method = "loglinear",
  lifetable_withPop_male = lifetable_withPopulation[["male"]],
  lifetable_withPop_female = lifetable_withPopulation[["female"]],
  firstYear_lifetable = 2019,
  nonNatural_death_male = mortality_data[["male"]], # % of non-natural male deaths in BHD
  nonNatural_death_female = mortality_data[["female"]], # % of non-natural female deaths in BHD
  pollutant = input_data_mortality$pollutant[1],
  age_group = input_data_mortality$outcome_group[1],
  min_age = 20,
  max_age = 0,
  corrected_discount_rate = 0),
  interval = 0.005,
  prof_output = "testing/output/performance/assess_mortality_lifetable().Rprof"
  )

# 2.8 s (for one call)
# Not best overview (zoom into flame graph to see more detail)


>>>>>>> 73d14754c47fba014d44d3809a7f79a272dc65c5:r_package/bestcost/testing/Performance/assess_mortality_lifetable().R
