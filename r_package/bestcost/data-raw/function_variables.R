## code to prepare `function_variables` dataset goes here

usethis::use_data(function_variables, overwrite = TRUE)

crf_per <- 10
first_age_pop <- 0
last_age_pop <- 99
interval_age_pop <- 1
year_of_analysis <- 2019
corrected_discount_rate <- 0

usethis::use_data(crf_per,
                  first_age_pop,
                  last_age_pop,
                  interval_age_pop,
                  year_of_analysis,
                  corrected_discount_rate,
                  internal = TRUE)
