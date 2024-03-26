# Test function ####
#exp <- c(input_data_mortality$exp[2], input_data_mortality$exp[2]+5, input_data_mortality$exp[2]+10)
exp <- input_data_mortality$exp[2]
#prop_pop_exp <- c(0.2, 0.2, 0.6)
prop_pop_exp <- 1
cutoff <- input_data_mortality$cutoff[2] 
rr <- unlist(input_data_morbidities[12, c("rr_mean", "rr_lowci", "rr_highci")]) # rr for asthma
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
cases <- c(rep(0, 49), 100, rep(0, 50))
dw <- 0.133 # Asthma, uncontrolled

# Calculate PAF ####
# Convert NULL into NA in min_age and max_age
min_age <- ifelse(is.null(min_age), NA, min_age)
max_age <- ifelse(is.null(max_age), NA, max_age)


# Compile rr data to assign categories
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

# Only if exposure distribution (multiple exposure categories)
# then reduce the number of rows to keep the same number as in rr
if(length(exp)>1){
  input_withPaf <-
    input_withPaf %>%
    dplyr::mutate(
      # Add a column for the average exp (way to summarize exposure)
      exp_mean = mean(exp),
      # Replace the actual values with "multiple" to enable reduction of rows
      exp = paste(exp, collapse = ", "),
      prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
      rr_forPaf = paste(rr_forPaf, collapse = ", "))%>%
    # Keep only rows that are distinct
    dplyr::distinct(.)
}

# Compile life table ####
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

for (i in c("male", "female")){
  lifetable_withPop[[i]] <- lifetable_withPop[[i]] %>% 
    mutate(deaths = population * death_probability_natural, # TAKE NATURAL OR TOTAL p(DEATH)? ####
           person_years_lived = (population-deaths), # * 0.5, OR NOT? ####
           total_person_years_lived = NA)
  # Compute total_person_years_lived as the sum of all rows below + current row 
  for (n in 1:nrow(lifetable_withPop[[i]])){
    lifetable_withPop[[i]][n,"total_person_years_lived"] <- sum(lifetable_withPop[[i]][n:nrow(lifetable_withPop[[i]]),"person_years_lived"])
  }
  lifetable_withPop[[i]] <- lifetable_withPop[[i]] %>% 
    mutate(life_expectancy = total_person_years_lived / population,
           cases = cases,
           attributable_cases = cases * as.numeric(input_withPaf[input_withPaf$ci=="mean","paf"]),
           yld = attributable_cases * dw * life_expectancy) # attributable_burden anstatt yld
}

lifetable_withPop[["male"]][["yld"]]
head(lifetable_withPop$male)
head(lifetable_withPop$female)

# For validation of total person years lived column
# dat <- lifetable_withPopulation[["female"]] %>% 
#   dplyr::mutate(deaths = population_2019 * death_probability_total,
#                 person_years_lived = (population_2019-deaths) * 0.5,
#                 total_person_years_lived = NA)
# for (i in 1:nrow(dat)) {
#   dat[i,"total_person_years_lived"] <- sum(dat[i:nrow(dat),"person_years_lived"])
# }

# Attributable cases


