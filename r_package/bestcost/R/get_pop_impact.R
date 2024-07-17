# Title and description

#' Get population impact over time
#'
#' Get population impact over time
#' @param lifetable_with_pop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table
#' @param pop_fraction \code{Data frame} showing the PAF (population attributable fraction) or PIF (population impact fraction) in three rows (central, lower bound and upper bound)
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
#' @import rlang
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal

get_pop_impact <-
  function(lifetable_with_pop,
           year_of_analysis,
           input_with_risk_and_pop_fraction,
           outcome_metric,
           min_age){

    # AirQ+ APPROACH ###############################################################################
    if (outcome_metric %in% c("yll_airqplus")) {

      # FACTORS NEEDED FOR CALCULATIONS
      user_options <- options()
      options(digits = 15)
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction %>%
        # Determine beta value: beta = ln(RR) / increment (R's log() takes the natural logarithm)
        mutate(beta =
                 # as.numeric(format(
                   log(rr)/erf_increment,
                   # digits = 14)),
               .after = rr) %>%
        # Determine modification factor for determining survival probability in counterfactual scenario
        # Based on AirQ+ lifetable manual formula 7 on p 17): RR(x_1 - x_0) = exp( beta * (x_1 - x_0) )
        mutate(modification_factor = exp(beta * (cutoff - exp)), .after = beta)

      ## POPULATION SETUP AND PROJECTION ###########################################################
      pop_total <- tibble(
        age = lifetable_with_pop$lifetable_with_pop_nest[[1]]$age,
        age_end = lifetable_with_pop$lifetable_with_pop_nest[[1]]$age_end,
        population =
          lifetable_with_pop$lifetable_with_pop_nest[[1]]$population +
          lifetable_with_pop$lifetable_with_pop_nest[[2]]$population,
        deaths = lifetable_with_pop$lifetable_with_pop_nest[[1]]$deaths +
          lifetable_with_pop$lifetable_with_pop_nest[[2]]$deaths)

      pop_total <- tibble(geo_id_raw = lifetable_with_pop$geo_id_raw[1],
                     sex = "total",
                     lifetable_with_pop_nest = list(pop_total))

      pop_ref <- bind_rows(lifetable_with_pop,
                        pop_total)

      pop <- pop_ref %>%
        mutate(lifetable_with_pop_nest = lifetable_with_pop_nest %>%
                 purrr::map(
                   .,
                   setup_lifetable(.x){
                     .x <- .x %>%
                       select(age, age_end, deaths, population) %>%
                       rename(!!paste0("population_",year_of_analysis) := population) %>%
                       mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
                       mutate(prob_survival = 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry"))), .after = deaths) %>% # probability of survival from start of year i to start of year i+1 (entry to entry)
                       mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths)
                            }
                   )
               )

      # Define loop variables (to be used in both modelled and cutoff population projection)
      years <- c(year_of_analysis:(year_of_analysis + (nrow(pop[["lifetable_with_pop_nest"]][[1]]) - 1)))
      length_period <- length(years)

      pop <- pop %>%
        mutate(pop_modelled_nest = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 project_population(.x){
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


      pop <- pop %>%
        mutate(pop_cutoff_nest = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 calculate_survival_probability_counterfactual(.x){
                   .x <- .x %>%
                     mutate(hazard_rate = input_with_risk_and_pop_fraction$modification_factor[1] * deaths / !!sym(paste0("population_",year_of_analysis)), .after = deaths) %>% # Hazard rate for calculating survival probabilities
                     mutate(prob_survival = (2 - hazard_rate) / (2 + hazard_rate), .after = deaths) %>%
                     mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths) %>%
                     select(-hazard_rate)
                   }
                 )
             ) %>%
        mutate(pop_cutoff_nest = pop_cutoff_nest %>%
                 purrr::map(
                   .,
                   adjust_survival_prob_below_min_age(.x){
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
              project_population(.x){
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
          get_difference_between_scenarios(.x, .y){
            .x - .y
            }
          )
        ) %>%
        mutate(premature_deaths_nest = yll_nest %>%
                 purrr::map(
                   .,
                   determine_premature_deaths_based_on_yll(.x){
                     .x * 2
                   }
                 )
               ) %>%
        mutate(pop_impact_nest = yll_nest %>%
                 purrr::map(
                 .,
                 add_age_columns(.x){
                   .x %>%
                     mutate(age = 0:99, .before = !!paste0("population_",year_of_analysis)) %>%
                     mutate(age_end = 1:100, .after = age)}))

      # COMPILE OUTPUT #############################################################################
      pop$erf_ci <- "central"
      pop_impact <- input_with_risk_and_pop_fraction %>%
        filter(erf_ci == "central") %>%
        right_join(pop, by = c("erf_ci", "geo_id_raw"))

      on.exit(options(user_options))
      return(pop_impact)
    }



    # GeLuft APPROACH ##############################################################################
    if ((outcome_metric %in% c("yll_airqplus") == FALSE)) {

    second_year <- year_of_analysis + 1

    ## DETERMINE POPULATION IMPACT #################################################################
    pop_impact <-
      dplyr::cross_join(
        input_with_risk_and_pop_fraction,
        lifetable_with_pop) %>%
      # Cross join duplicates the columns with the same name
      # adding a suffix .y and .x
      # Select only one column (avoiding duplicated columns)
      # and rename the columns
      dplyr::select(., -geo_id_raw.y) %>%
      dplyr::rename(., geo_id_raw = geo_id_raw.x) %>%
      # geo_id_aggregated is a volutary argument
      # Therefore, if()
      {if(any(grepl("geo_id_aggregated", names(.))))
        dplyr::select(., -geo_id_aggregated.y) %>%
          dplyr::rename(., geo_id_aggregated = geo_id_aggregated.x) else .} %>%
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
                  dplyr::lag(death_probability_natural) * .y
                )
          )))

    if(outcome_metric %in% c("yll", "yld", "daly")){
      
      # Now calculate population over time for the rest of year starting with YOA without considering air pollution
      period <- c( (year_of_analysis + 1) :
                     ((year_of_analysis +
                         unique(purrr::map_int(lifetable_with_pop$lifetable_with_pop_nest,
                                               ~nrow(.x))) - 2)) )
      length_period <- length(period)
      population_period <- paste0("population_", period)


      pop_impact <-
        pop_impact %>%
        dplyr::mutate(
          pop_impact_nest = purrr::map(
            pop_impact_nest,
            project_population(.x) {
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
            project_population(.x) {
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

