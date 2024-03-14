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
min_age <- input_data_mortality$min_age[2]
max_age <- input_data_mortality$max_age[2]
corrected_discount_rate <- 0
#cases <- c(rep(0, 49), 100, rep(0, 50))
dw <- 0.5 # 0.133 = Asthma, uncontrolled
# duration <- # TBA

# Compile input data ####
# Convert NULL into NA in min_age and max_age
min_age <- ifelse(is.null(min_age), NA, min_age)
max_age <- ifelse(is.null(max_age), NA, max_age)

input_withPaf <-
  data.frame(
    info = info,
    rr = rr,
    rr_increment = rr_increment,
    erf_shape = erf_shape,
    # Assign mean, low and high rr values
    rr_ci = ifelse(rr %in% min(rr), "low",
                   ifelse(rr %in% max(rr), "high",
                          "mean")),
    cutoff = cutoff,
    # Information derived from input data
    approach_id = paste0("lifetable_", erf_shape),
    age_range = ifelse(!is.na(max_age), paste0("below", max_age + 1),
                       ifelse(!is.na(min_age), paste0("from", min_age),
                              NA))) %>%
  # In case of same value in mean and low or high, assign value randomly
  dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) %>% 
  # Add rr with a cross join to produce all likely combinations
  dplyr::cross_join(., data.frame(exp = exp,
                                  prop_pop_exp = prop_pop_exp)) %>% 
  # rescale rr's for PAF
  dplyr::mutate(
    rr_forPaf =
      bestcost::get_risk(rr = rr,
                         exp = exp,
                         cutoff = cutoff,
                         rr_increment = rr_increment,
                         erf_shape = {{erf_shape}}),
    #{{}} ensures that the
    # value from the function argument is used
    # instead of from an existing column
    # Add a column for the average exp (way to summarize exposure)
    exp_mean = mean(exp)) %>% 
  # Group by exp in case that there are different exposure categories
  dplyr::group_by(rr) %>% 
  dplyr::mutate(paf = bestcost::get_paf(rr_conc = rr_forPaf,
                                        prop_pop_exp = prop_pop_exp))

lifetable_withPop <- list(
  male =
    data.frame(
      age = seq(from = first_age_pop,
                to = last_age_pop,
                by = interval_age_pop),
      age_end = seq(from = first_age_pop + interval_age_pop,
                    to = last_age_pop + interval_age_pop, #to = last_age_pop,
                    by = interval_age_pop), #by = interval_age_pop + interval_age_pop),
      death_probability_natural = prob_natural_death_male,
      death_probability_total = prob_total_death_male,
      population = population_male),
  
  female =
    data.frame(
      age = seq(from = first_age_pop,
                to = last_age_pop,
                by = interval_age_pop),
      age_end = seq(from = first_age_pop + interval_age_pop,
                    to = last_age_pop + interval_age_pop, #to = last_age_pop,
                    by = interval_age_pop), #by = interval_age_pop + interval_age_pop),
      death_probability_natural = prob_natural_death_female,
      death_probability_total = prob_total_death_female,
      population = population_female))

# Get attributed cases in YOA + 1 ####

pop_impact <-
  bestcost::get_pop_impact(
    lifetab_withPop = lifetable_withPop,
    year_of_analysis = year_of_analysis,
    paf = input_withPaf[, c("ci", "paf")])


# Get YLL ####
yll <-
  bestcost::get_yll(
    pop_impact = pop_impact,
    year_of_analysis = year_of_analysis,
    min_age = min_age,
    max_age = max_age,
    meta = input_withPaf,
    corrected_discount_rate = corrected_discount_rate)

# Apply disability weight ####
yll$yll <- yll$yll %>% 
  mutate(impact = impact * dw,
         impact_rounded = round(impact),
         impact_metric = "Years lived with disability"
         ) %>% 
  select(impact, impact_rounded, impact_metric, everything())

# Compile output ####
output <-
  list(
    pop_impact = pop_impact,
    yld = yll[["yll"]])

head(output$yld$impact_rounded)
# Reference impact (from "testing_Rpackage")
print("Reference impact: 40542 (high), 14641 (low), 27742 (mean)")
