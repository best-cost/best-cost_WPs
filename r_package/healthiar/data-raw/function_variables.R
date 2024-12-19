## code to prepare `function_variables` data set goes here

erf_increment <- 10
erf_shape <- "loglinear"
first_age_pop <- 0
last_age_pop <- 99
interval_age_pop <- 1
year_of_analysis <- 2019
ci <- c("central", "lower", "upper")
sex <- c("female", "male")

usethis::use_data(erf_increment,
                  first_age_pop,
                  last_age_pop,
                  interval_age_pop,
                  year_of_analysis,
                  ci,
                  sex,
                  internal = TRUE,
                  overwrite = TRUE)
