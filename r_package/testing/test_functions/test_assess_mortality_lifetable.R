# Objective script: facilitate testing of assess_mortality_lifetable()

library(bestcost) ; library(dplyr) ; library(purrr)
devtools::load_all()

# Subset one line of input_data_mortality
input_data <- input_data_mortality[2,]

# Test base function ####
test_base <- bestcost::assess_mortality_lifetable(
  exp = input_data_mortality$exp[2], # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
  cf = input_data_mortality$cf[2],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021 
  crf = unlist(input_data_mortality[2,
                                    c("crf_mean", "crf_lowci", "crf_highci")]),
  crf_per = 10, 
  crf_rescale_method = "loglinear",
  first_age_pop = 0,
  last_age_pop = 99,
  interval_age_pop = 1,
  prob_natural_death_male = lifetable_withPopulation[["male"]]$death_probability_natural,
  prob_natural_death_female = lifetable_withPopulation[["female"]]$death_probability_natural,
  prob_total_death_male = lifetable_withPopulation[["male"]]$death_probability_total,
  prob_total_death_female = lifetable_withPopulation[["female"]]$death_probability_total,
  population_male = lifetable_withPopulation[["male"]]$population, 
  population_female = lifetable_withPopulation[["female"]]$population, 
  year_of_analysis = 2019, 
  info_pollutant = input_data_mortality$pollutant[2], 
  min_age = input_data_mortality$min_age[2],
  max_age = input_data_mortality$max_age[2],
  corrected_discount_rate = 0)

# Define function input variables ####
exp = input_data_mortality$exp[1] # PM2.5=8.30=limit LRV CH, NO2=16.32=mean conc. in CH in 2019
cf = input_data_mortality$cf[1]   # PM2.5=5, NO2=10, i.e. WHO AQG 2021 
crf = unlist(input_data_mortality[1, c("crf_mean", "crf_lowci", "crf_highci")])
crf_per = 10 
crf_rescale_method = "loglinear"
first_age_pop = 0
last_age_pop = 99
interval_age_pop = 1
prob_natural_death_male = lifetable_withPopulation[["male"]]$death_probability_natural
prob_natural_death_female = lifetable_withPopulation[["female"]]$death_probability_natural
prob_total_death_male = lifetable_withPopulation[["male"]]$death_probability_total
prob_total_death_female = lifetable_withPopulation[["female"]]$death_probability_total
population_male = lifetable_withPopulation[["male"]]$population 
population_female = lifetable_withPopulation[["female"]]$population 
year_of_analysis = 2019 
info_pollutant = input_data_mortality$pollutant[1] 
min_age = input_data_mortality$min_age[1]
max_age = input_data_mortality$max_age[1]
corrected_discount_rate = 0
info_pollutant = NULL
info_outcome = NULL
info_exp = NULL
info_cf = NULL
info_crf = NULL
info_bhd = NULL

# Modify function code ####
assess_mortality_lifetable <-
  function(exp, cf,
           crf, crf_per, crf_rescale_method,
           first_age_pop, last_age_pop, interval_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_male, population_female,
           year_of_analysis,
           corrected_discount_rate = 0,
           min_age = NULL, max_age = NULL,
           info_pollutant = NULL, info_outcome = NULL,
           info_exp = NULL, info_cf = NULL, info_crf = NULL, info_bhd = NULL){
    
    
    # Digest input data
    
    # Convert NULL into NA
    min_age <- ifelse(is.null(min_age), NA, min_age)
    max_age <- ifelse(is.null(max_age), NA, max_age)
    
    # Input data in data frame
    input_info <-
      data.frame(
        crf = crf,
        exp = exp,
        cf = cf,
        crf_per = crf_per,
        crf_rescale_method = crf_rescale_method,
        # Infomation derived from input data
        approach_id = paste0("lifetable_", crf_rescale_method),
        age_range = ifelse(!is.na(max_age), paste0("below", max_age + 1),
                           ifelse(!is.na(min_age), paste0("from", min_age),
                                  NA))) %>%
      
      # Add additional information (info_x variables)
      dplyr::mutate(
        info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
        info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
        info_exp = ifelse(is.null(info_exp), NA, info_exp),
        info_cf = ifelse(is.null(info_cf), NA, info_cf),
        info_crf = ifelse(is.null(info_crf), NA, info_crf),
        info_bhd = ifelse(is.null(info_bhd), NA, info_bhd))
    
    
    # Calculate crf estimate which corresponds to the exposure
    # depending on the method
    input_info_paf <-
      input_info %>%
      dplyr::mutate(
        crf_forPaf =
          rescale_crf(crf = crf,
                      exp = exp,
                      cf = cf,
                      crf_per = crf_per,
                      method ={{crf_rescale_method}}
                      #{{}} ensures that the
                      # value from the function argument is used
                      # instead of from an existing column
          ),
        crf_ci = ifelse(crf %in% min(crf), "low",
                        ifelse(crf %in% max(crf), "high",
                               "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(crf), "mean", ci)) %>%
      
      # Calculate attributable fraction (AF) as well as impact
      dplyr::mutate(approach_id = paste0("singleValue_", crf_rescale_method),
                    paf =  bestcost::get_paf(crf_conc = crf_forPaf))
    
    
    # The life table has to be provided as a data.frame (by sex)
    # The first column has to be the age. Second, probability of death. Third, population.
    # Rename column names to standard names
    
    lifetable_withPop <- list(
      male =
        data.frame(
          age = seq(from = first_age_pop,
                    to = last_age_pop,
                    by = interval_age_pop),
          age_end = seq(from = first_age_pop + interval_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop + interval_age_pop),
          death_probability_natural = prob_natural_death_male,
          death_probability_total = prob_total_death_male,
          population = population_male),
      
      female =
        data.frame(
          age = seq(from = first_age_pop,
                    to = last_age_pop,
                    by = interval_age_pop),
          age_end = seq(from = first_age_pop + interval_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop + interval_age_pop),
          death_probability_natural = prob_natural_death_female,
          death_probability_total = prob_total_death_female,
          population = population_female))
    
    
    # Get population impact
    shifted_popOverTime <-
      bestcost::get_pop_impact(
        lifetab_withPop = lifetable_withPop,
        year_of_analysis = year_of_analysis,
        paf = input_info_paf[, c("ci", "paf")])
    
    
    # Calculate deaths
    deaths <-
      bestcost::get_deaths(
        shifted_popOverTime = shifted_popOverTime,
        year_of_analysis = year_of_analysis,
        min_age = min_age,
        max_age = max_age,
        meta = input_info_paf)
    
    # Calculate years of life lost (yll)
    yll <-
      bestcost::get_yll(
        shifted_popOverTime = shifted_popOverTime,
        year_of_analysis = year_of_analysis,
        min_age = min_age,
        max_age = max_age,
        meta = input_info_paf,
        corrected_discount_rate = corrected_discount_rate)
    
    output <-
      list(
        shifted_popOverTime = shifted_popOverTime,
        deaths_long = deaths[["deaths_long"]],
        deaths = deaths[["deaths"]],
        yll_long = yll[["yll_long"]],
        yll = yll[["yll"]])
    
    return(output)
    
  }

