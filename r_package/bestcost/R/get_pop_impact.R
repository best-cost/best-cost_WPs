# Title and description

#' Get population impact over time
#'
#' Get population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table
#' @param input_with_risk_and_pop_fraction \code{Tibble} showing the input data and the PAF (population attributable fraction) or PIF (population impact fraction)
#' @param outcome_metric \code{String} to define the outcome metric. Choose between "death", "yll" and "yld"
#'
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal

get_pop_impact <-
  function(year_of_analysis,
           input_with_risk_and_pop_fraction,
           outcome_metric,
           min_age){

    # AirQ+ APPROACH ###############################################################################
    if (outcome_metric %in% c("yll_airqplus")) {

      # FACTORS NEEDED FOR CALCULATIONS
      user_options <- options()
      options(digits = 15)
      input_with_risk_and_pop_fraction <-
        input_with_risk_and_pop_fraction %>%
        # Determine beta value: beta = ln(RR) / increment (R's log() takes the natural logarithm)
        dplyr::mutate(beta =
                 # as.numeric(format(
                   log(rr)/erf_increment,
                   # digits = 14)),
               .after = rr) %>%
        # Determine modification factor for determining survival probability in counterfactual scenario
        # Based on AirQ+ lifetable manual formula 7 on p 17): RR(x_1 - x_0) = exp( beta * (x_1 - x_0) )
        dplyr::mutate(modification_factor = exp(beta * (cutoff - exp)), .after = beta)

      ## POPULATION SETUP AND PROJECTION ###########################################################

      pop <- input_with_risk_and_pop_fraction %>%
        dplyr::mutate(
          lifetable_with_pop_nest =
            lifetable_with_pop_nest %>%
            purrr::map(
              .,
              function(.x){
                .x <- .x %>%
                  select(age, age_end, deaths, population) %>%
                  rename(!!paste0("population_",year_of_analysis) := population) %>%
                  mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
                  mutate(prob_survival = 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry"))), .after = deaths) %>% # probability of survival from start of year i to start of year i+1 (entry to entry)
                  mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths)
                }
              )
          )

      # Add the modification factor to the nested tibble "lifetable_with_pop_nest" using pmap()
      pop <- pop %>%
        mutate(lifetable_with_pop_nest =
                 pmap(
                   list(lifetable_with_pop_nest, modification_factor),
                   function(lifetable_with_pop_nest, modification_factor){
                     lifetable_with_pop_nest <- lifetable_with_pop_nest %>%
                       mutate(modification_factor = modification_factor)
                   }
                 )
               )

      # Define loop variables (to be used in both modelled and cutoff population projection)
      years <- c(year_of_analysis:(year_of_analysis + (nrow(pop[["lifetable_with_pop_nest"]][[1]]) - 1)))
      length_period <- length(years)

      # Calculate premature deaths directly & project "dead" population (like GeLuft)
      # using the difference in survival probabilities between modelled & cutoff populations
      pop <- pop %>%
        dplyr::mutate(
          pop_dead_nest =
            lifetable_with_pop_nest %>%
            purrr::map(
              .,
              function(.x){
                .x <- .x %>%
                  mutate(hazard_rate_modified = modification_factor * deaths / !!sym(paste0("population_",year_of_analysis)), .after = deaths) %>% # Hazard rate for calculating survival probabilities
                  mutate(prob_survival_modified = (2 - hazard_rate_modified) / (2 + hazard_rate_modified), .after = deaths) %>%
                  mutate(prob_survival_until_mid_year_modified = 1 - ((1 - prob_survival_modified) / 2), .after = deaths) %>%
                  mutate(prob_survival_difference = prob_survival_modified - prob_survival) %>%
                  mutate(prob_survival_until_mid_year_difference = prob_survival_until_mid_year_modified - prob_survival_until_mid_year) %>%
                  mutate(population_2020_entry = lag(population_2019 * prob_survival_difference)) %>%
                  mutate(population_2020 = population_2020_entry * prob_survival_until_mid_year)
              }
            )
        )

      pop <- pop %>%
        dplyr::mutate(
          pop_dead_nest =
            pop_dead_nest %>%
            purrr::map(
              .,
              function(.x){
                for (i in 2:(length_period - 1)) { # starts with 2; ends with 99

                  # print(i)

                  .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
                    .x[i:(length_period - 1), paste0("population_", years[i], "_entry")] * .x[i:(length_period - 1), "prob_survival"]

                  .x[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
                    .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * .x[(i + 1):length_period, "prob_survival_until_mid_year"]
                }

                .x <- .x %>%
                  select(-contains("entry")) %>%
                  select(contains("population"))
                  }
              )
          )

      ## Different computation depending on value of approach_exposure #############################
      if (unique(input_with_risk_and_pop_fraction$approach_exposure == "single_year")){

        pop <- pop %>%
          # Calculate entry population of year of analysis + 1 using observed survival probabilities
          mutate(pop_modelled_nest = lifetable_with_pop_nest %>%
                   purrr::map(
                     .,
                     function(.x){

                       .x <- .x %>%
                         mutate(!!paste0("population_",year_of_analysis+1,"_entry") :=
                                  lag(!!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival))

                     }
                   )
          ) %>%
          # Assign modified survival probabilities
          mutate(pop_modelled_nest = pop_modelled_nest %>%
                   purrr::map(
                     .,
                     function(.x){
                       .x <- .x %>%
                         mutate(hazard_rate = if_else(row_number() > min_age, modification_factor * deaths / !!sym(paste0("population_",year_of_analysis)), deaths / !!sym(paste0("population_",year_of_analysis))), .after = deaths) %>% # Hazard rate for calculating survival probabilities
                         mutate(prob_survival = if_else(row_number() > min_age, (2 - hazard_rate) / (2 + hazard_rate), prob_survival), .after = deaths) %>%
                         mutate(prob_survival_until_mid_year = if_else(row_number() > min_age, 1 - ((1 - prob_survival) / 2), prob_survival_until_mid_year), .after = deaths) # %>%
                         # select(-hazard_rate)
                     }
                   )
          ) %>%

          # Determine mid-year population of year yoa + 1 using modified survival probabilities
          mutate(pop_modelled_nest = pop_modelled_nest %>%
                   purrr::map(
                     .,
                     function(.x){

                       .x <- .x %>%
                         mutate(!!paste0("population_",year_of_analysis+1) :=
                                  !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year)

                     }
                   )
          )

        pop <- pop %>%
          # Project population from yoa+1 until end
          mutate(pop_modelled_nest = pop_modelled_nest %>%
                   purrr::map(
                     .,
                     function(.x){
                       for (i in 2:(length_period - 1)) { # starts with 2; ends with 99

                         # print(i)

                         .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
                           .x[i:(length_period - 1), paste0("population_", years[i], "_entry")] * .x[i:(length_period - 1), "prob_survival"]

                         .x[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
                           .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * .x[(i + 1):length_period, "prob_survival_until_mid_year"]
                       }

                       .x <- .x %>%
                         select(-contains("entry")) %>%
                         select(contains("population"))

                     }
                   )
          )

      } else {

      # Project population in modelled (= actually observed; reference) scenario
      pop <- pop %>%
        mutate(pop_modelled_nest = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 function(.x){
                    for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

                      # print(i)

                       .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
                         .x[i:(length_period - 1), paste0("population_", years[i], "_entry")] * .x[i:(length_period - 1), "prob_survival"]

                       .x[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
                         .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * .x[(i + 1):length_period, "prob_survival_until_mid_year"]
                       }

                     .x <- .x %>%
                       select(-contains("entry")) %>%
                       select(contains("population"))

                     }
                   )
               )
      }

      pop <- pop %>%
        # Calculate modified survival probability in cutoff (= counterfactual) scenario
        mutate(pop_cutoff_nest = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 function(.x){
                   .x <- .x %>%
                     mutate(hazard_rate = modification_factor * deaths / !!sym(paste0("population_",year_of_analysis)), .after = deaths) %>% # Hazard rate for calculating survival probabilities
                     mutate(prob_survival = (2 - hazard_rate) / (2 + hazard_rate), .after = deaths) %>%
                     mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths) %>%
                     select(-hazard_rate)
                   }
                 )
             ) %>%
        mutate(pop_cutoff_nest = pop_cutoff_nest %>%
                 purrr::map(
                   .,
                   function(.x){
                     .x %>%
                       # For all ages below min_age assign the unmodified survival probabilities
                       mutate(prob_survival = if_else(row_number() <= min_age, 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry"))), prob_survival)) %>%
                       mutate(prob_survival_until_mid_year = if_else(row_number() <= min_age, 1 - ((1 - prob_survival) / 2), prob_survival_until_mid_year)) %>%
                       # Re-calculate the "pop_2019_mid_year_total" using the modified survival rates
                       mutate(!!paste0("population_",year_of_analysis) := !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival_until_mid_year)
                   }
                 )
               )

      pop <- pop %>%
        mutate(
          pop_cutoff_nest = pop_cutoff_nest %>%
            purrr::map(
              .,
              function(.x){
                for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

                  # print(i)

                  .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
                    .x[i:(length_period - 1), paste0("population_", years[i], "_entry")] * .x[i:(length_period - 1), "prob_survival"]

                  .x[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
                    .x[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * .x[(i + 1):length_period, "prob_survival_until_mid_year"]
                }

                .x <- .x %>%
                  select(-contains("entry")) %>%
                  select(contains("population"))

              }
            )
          )

      # DETERMINE POPULATION IMPACT ################################################################
      pop <- pop %>%
        mutate(yll_nest = purrr::map2(
          pop_cutoff_nest, pop_modelled_nest,
          function(.x, .y){
            .x - .y
            }
          )
        ) %>%
        mutate(premature_deaths_nest = yll_nest %>%
                 purrr::map(
                   .,
                   function(.x){
                     .x * 2
                   }
                 )
               ) %>%
        mutate(pop_impact_nest = yll_nest %>%
                 purrr::map(
                 .,
                 function(.x){
                   .x %>%
                     mutate(age = 0:99, .before = !!paste0("population_",year_of_analysis)) %>%
                     mutate(age_end = 1:100, .after = age)}))

      # COMPILE OUTPUT #############################################################################

      joining_columns_pop_impact <-
        bestcost:::find_joining_columns(input_with_risk_and_pop_fraction,
                                        pop,
                                        except = "lifetable_with_pop_nest")

      pop_impact <-
        input_with_risk_and_pop_fraction %>%
        dplyr::right_join(., pop, by = joining_columns_pop_impact)
      # NOTE: After joining, there are two "lifetable_with_pop_nest" #####################################

      on.exit(options(user_options))
      return(pop_impact)
    }



    # GeLuft APPROACH ##############################################################################
    if ((outcome_metric %in% c("yll_airqplus") == FALSE)) {

    second_year <- year_of_analysis + 1

    ## DETERMINE POPULATION IMPACT #################################################################
    pop_impact <-
      input_with_risk_and_pop_fraction %>%
      dplyr::mutate(
        pop_impact_nest = purrr::map(
          lifetable_with_pop_nest,
          ~ dplyr::rename(
            .x,
            !!paste0("population_", year_of_analysis) := population))) %>%
      # Calculate the population the second year (first column after first year) considering the health effect of air pollution
      # And move column up one row: lead()
      dplyr::mutate(
        pop_impact_nest = purrr::map2(
          pop_impact_nest, pop_fraction,
          ~ dplyr::mutate(
            .x,
            "population_{second_year}" :=
              lead(
                dplyr::lag(!!as.symbol(paste0("population_",year_of_analysis))) *
                  dplyr::lag(prob_natural_death) * .y
                )
          )))

    if(outcome_metric %in% c("yll", "yld", "daly")){

      # Now calculate population over time for the rest of year starting with YOA without considering air pollution
      period <- c( (year_of_analysis + 1) :
                     ((year_of_analysis +
                         unique(purrr::map_int(input_with_risk_and_pop_fraction$lifetable_with_pop_nest,
                                               ~nrow(.x))) - 2)) )
      length_period <- length(period)
      population_period <- paste0("population_", period)


      pop_impact <-
        pop_impact %>%
        dplyr::mutate(
          pop_impact_nest = purrr::map(
            pop_impact_nest,
            function(.x) {
              # length_period minus 1 because year_of_analysis+1 is already calculated
              for (i in 0:(length_period-1)){
                current_year <- period[i+1]
                col_current <- paste0("population_", current_year)
                col_next <- paste0("population_", current_year + 1)
                # avoiding the later introduction of NAs in the right top corner:
                .x[1:(length_period-i), col_next] <-
                   .x[1:(length_period-i), col_current] * (1 - .x$prob_total_death[(i+2):(length_period+1)])


                # Alternative code
                # Simpler but it does not provide the right result
                # .x[[col_next]] <-
                #  .x[[col_current]] * (1 - .x[["prob_total_death"]])
              }
              return(.x)
            } ))

    # COMPILE OUTPUT ###############################################################################
      years <- 2020:2118
      pop_impact <-
        pop_impact %>%
        dplyr::mutate(
          pop_impact_nest = purrr::map(
            pop_impact_nest,
            function(.x) {
              # length_period minus 1 because year_of_analysis+1 is already calculated
              for (i in 1:99){
                current_year <- years[i]
                col_current <- paste0("population_", current_year)
                # avoiding the later introduction of NAs in the right top corner:
                .x[, col_current] <- lag(.x[,col_current], n = i)
              }
              return(.x)
            } ))


      #
      # %>%
      #
      #
      #
      #   # Empty the top-right half of the table (for the alternative code above)
      #   # They are people that were not born when the exposure happened
      #   # (year of analysis)
      #   dplyr::mutate(.,
      #     pop_impact_nest = purrr::map(
      #       pop_impact_nest,
      #       function(.x){
      #
      #         x_matrix <-
      #           .x %>%
      #           dplyr::select(., all_of(population_period))%>%
      #           as.matrix(.)
      #
      #         x_matrix[row(x_matrix)<=col(x_matrix)] <- NA
      #
      #         .x <-
      #           dplyr::bind_cols(
      #             dplyr::select(.x, -all_of(population_period)),
      #             as_tibble(x_matrix))
      #
      #         return(.x)
      #       }
      #     ))

          }

    return(pop_impact) }

  }

