# YLD incidence approach based on life table

exp = input_data_mortality$exp[2]
prop_pop_exp <- 1
cutoff <- input_data_mortality$cutoff[2] 
rr <- unlist(input_data_mortality[2, c("rr_mean", "rr_lowci", "rr_highci")])
rr_increment <- 10 
erf_shape <- "log_linear"
first_age_pop <- 0
last_age_pop <- 99
interval_age_pop <- 1
prob_natural_death_male <- lifetable_withPopulation[["male"]]$death_probability_natural
prob_natural_death_female <- lifetable_withPopulation[["female"]]$death_probability_natural
prob_total_death_male <- lifetable_withPopulation[["male"]]$death_probability_total
prob_total_death_female <- lifetable_withPopulation[["female"]]$death_probability_total
population_male <- lifetable_withPopulation[["male"]]$population 
population_female <- lifetable_withPopulation[["female"]]$population 
year_of_analysis <- 2019 
info <- input_data_mortality$pollutant[2]
#min_age <- input_data_mortality$min_age[2]
min_age <- 20
#max_age <- input_data_mortality$max_age[2]
max_age <- 99
corrected_discount_rate <- 0
disability_weight <- 0.5 # 0.133 = Asthma, uncontrolled
erf_c = NULL
duration <- 5

# Compile input data ####
input <-
  bestcost::compile_input(
    exp = exp,
    prop_pop_exp = prop_pop_exp,
    cutoff = cutoff,
    disability_weight = disability_weight,
    duration = duration,
    rr = rr,
    rr_increment = rr_increment,
    erf_shape = erf_shape,
    erf_c = erf_c,
    bhd = NULL,
    min_age = min_age,
    max_age = max_age,
    info = info,
    method = paste0("lifetable_rr_corrected"))


# Get PAF and add to the input data frame
input_risk_paf <-
  bestcost::get_risk_and_paf(input = input)


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

# Get attributable cases in YOA + 1 ####

pop_impact <-
  bestcost::get_pop_impact(
    lifetab_withPop = lifetable_withPop,
    year_of_analysis = year_of_analysis,
    paf = input_risk_paf[, c("ci", "paf")])


# Get YLL ####
yll <-
  bestcost::get_yll(
    pop_impact = pop_impact,
    year_of_analysis = year_of_analysis,
    min_age = min_age,
    max_age = max_age,
    meta = input_risk_paf,
    corrected_discount_rate = corrected_discount_rate)

# Apply disability weight ####
yll$total <- yll$total %>%
  mutate(impact = impact * disability_weight,
         impact_rounded = round(impact),
         impact_metric = "Years lived with disability"
  ) %>%
  select(impact, impact_rounded, impact_metric, everything())

# Compile output ####
output <-
  list(
    pop_impact = pop_impact,
    yld = yll[["total"]])

head(output$yld$impact_rounded)
# Reference impact (from "testing_Rpackage")
print("Reference impact: 40542 (high), 14641 (low), 27742 (mean)")
