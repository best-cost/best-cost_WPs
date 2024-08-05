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


    # PREMATURE DEATHS
    # if(outcome_metric %in% c("deaths")){
    # # second_year <- year_of_analysis + 1
    #
    # ## DETERMINE POPULATION IMPACT
    # pop_impact <-
    #   input_with_risk_and_pop_fraction %>%
    #   dplyr::mutate(
    #     pop_impact_nest = purrr::map(
    #       lifetable_with_pop_nest,
    #       ~ dplyr::rename(
    #         .x,
    #         !!paste0("population_", year_of_analysis) := population))) %>%
    #   # Calculate the population the second year (first column after first year) considering the health effect of air pollution
    #   # And move column up one row: lead()
    #   dplyr::mutate(
    #     pop_impact_nest = purrr::map2(
    #       pop_impact_nest, pop_fraction,
    #       ~ dplyr::mutate(
    #         .x,
    #         "population_{year_of_analysis}" :=
    #           # lead(
    #           !!as.symbol(paste0("population_",year_of_analysis)) * prob_natural_death * .y
    #             # dplyr::lag(!!as.symbol(paste0("population_",year_of_analysis))) *
    #             #   dplyr::lag(prob_natural_death) * .y
    #           # )
    #       )))
    # return(pop_impact)
    # }

    # FACTORS NEEDED FOR CALCULATIONS
    user_options <- options()
    options(digits = 15)
    input_with_risk_and_pop_fraction <-
      input_with_risk_and_pop_fraction %>%
        dplyr::mutate(modification_factor = (1 / rr_conc), .after = rr)

    ## LIFETABLE SETUP ############################################################################

    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction %>%
      dplyr::relocate(lifetable_with_pop_nest, .before = 1) %>%
      dplyr::mutate(
        lifetable_with_pop_nest =
          lifetable_with_pop_nest %>%
          purrr::map(
            .,
            function(.x){
              .x <- .x %>%
                select(age, age_end, deaths, population) %>%
                rename(!!paste0("population_",year_of_analysis) := population) %>%
                # CALCULATE ENTRY POPULATION OF YOA
                mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
                # CALCULATE PROBABILITY OF SURVIVAL FROM START YEAR TO END YEAR & START YEAR TO MID YEAR
                mutate(prob_survival = ( !!sym(paste0("population_",year_of_analysis)) - ( deaths / 2 ) ) / (!!sym(paste0("population_",year_of_analysis)) + ( deaths / 2 ) ), .after = deaths) %>% # probability of survival from start of year i to start of year i+1 (entry to entry)
                mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths)
            }
          )
      )

    # ADD THE MODIFICATION FACTOR TO THE NESTED TIBBLE "LIFETABLE_WITH_POP_NEST" USING FCT PMAP()
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction %>%
      mutate(lifetable_with_pop_nest =
               pmap(
                 list(lifetable_with_pop_nest, modification_factor),
                 function(lifetable_with_pop_nest, modification_factor){
                   lifetable_with_pop_nest <- lifetable_with_pop_nest %>%
                     mutate(modification_factor = modification_factor)
                 }
               )
      )

    ## DETERMINE POPULATIONS IN YOA AND YOA+1 ######################################################
    # YOA = YEAR OF ANALYSIS

    ### BASELINE SCENARIO ##########################################################################

    # DETERMINE ENTRY POPULATION OF YOA+1 IN BASELINE SCENARIO
    pop <- input_with_risk_and_pop_fraction %>%
      mutate(pop_baseline_scenario = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 function(.x){

                   .x <- .x %>%
                     mutate(!!paste0("population_",year_of_analysis,"_end") :=
                              !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival) %>%
                     mutate(!!paste0("population_",year_of_analysis+1,"_entry") :=
                              lag(!!sym(paste0("population_",year_of_analysis,"_end"))))

                 }
               )
      , .after = lifetable_with_pop_nest)

    ### IMPACTED SCENARIO ##########################################################################

    # CALCULATE MODIFIED SURVIVAL PROBABILITIES
    pop <- pop %>%
      mutate(pop_impacted_scenario = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 function(.x){
                   .x <- .x %>%
                     # For all ages min_age and higher calculate modified survival probabilities
                     mutate(hazard_rate = if_else(row_number() > min_age, modification_factor * deaths / !!sym(paste0("population_",year_of_analysis)), deaths / !!sym(paste0("population_",year_of_analysis))), .after = deaths) %>% # Hazard rate for calculating survival probabilities
                     mutate(prob_survival_mod = if_else(row_number() > min_age, (2 - hazard_rate) / (2 + hazard_rate), prob_survival), .after = deaths) %>%
                     mutate(prob_survival_until_mid_year_mod = if_else(row_number() > min_age, 1 - ((1 - prob_survival_mod) / 2), prob_survival_until_mid_year), .after = deaths) # %>%
                   # select(-hazard_rate)
                 }
               )
             , .after = pop_baseline_scenario)

    # CALCULATE MID-YEAR POPULATION OF YOA USING MODIFIED SURVIVAL PROBABILITIES
    pop <- pop %>%
      mutate(pop_impacted_scenario = pop_impacted_scenario %>%
               purrr::map(
                 .,
                 function(.x){
                   .x <- .x %>%
                     mutate(!!paste0("population_",year_of_analysis) :=
                              !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival_until_mid_year_mod)
                 }
               )
             )

    # CALCULATE ENTRY POPULATION OF YOA+1 USING MODIFIED SURVIVAL PROBABILITIES
    pop <- pop %>%
      mutate(pop_impacted_scenario = pop_impacted_scenario %>%
               purrr::map(
                 .,
                 function(.x){

                   .x <- .x %>%
                     mutate(!!paste0("population_",year_of_analysis,"_end") :=
                              !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival_mod) %>%
                     mutate(!!paste0("population_",year_of_analysis+1,"_entry") :=
                              lag(!!sym(paste0("population_",year_of_analysis,"_end"))))

                 }
               )
      )

    ## DETERMINE PREMATURE DEATHS IN YOA ###########################################################
    # premature deaths = impacted scenario YOA end-of-year population - baseline scenario YOA end-of-year pop
    pop <- pop %>%
      mutate(premature_deaths_nest = purrr::map2(
        pop_impacted_scenario, pop_baseline_scenario,
        ~ tibble(premature_deaths = .x$population_2019_end - .y$population_2019_end)),
        .after = pop_impacted_scenario)

    ## PROJECT POPULATIONS #########################################################################

    # Define loop variables (to be used in both modelled and cutoff population projection)
    # years <- c((year_of_analysis+1):(year_of_analysis + (nrow(pop[["lifetable_with_pop_nest"]][[1]]))))
    # length_period <- length(years)

    project_pop <- function(df, prob_survival, prob_survival_until_mid_year){

      years <- c((year_of_analysis+1):(year_of_analysis + (nrow(pop[["lifetable_with_pop_nest"]][[1]]))))
      length_period <- length(years)

      for (i in 1:(length_period - 2)) { # starts with 1; ends with 98 (years 2020 - 2118)

        # print(i)

        df[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
          df[i:(length_period - 1), paste0("population_", years[i], "_entry")] * prob_survival[i:(length_period - 1)]

        df[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          df[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * prob_survival_until_mid_year[(i + 1):length_period]
      }

      df <- df #%>%
      # select(-contains("entry")) %>%
      # select(contains("population"))

      return(df)

    }

    ### SINGLE YEAR EXPOSURE #######################################################################
    if (unique(input_with_risk_and_pop_fraction$approach_exposure == "single_year")){

      # DETERMINE MID-YEAR POPULATION OF YOA+1 IN BOTH IMPACTED AND BASELINE SCENARIO
      # USING UNMODIFIED SURVIVAL PROBABILITIES

      pop <- pop %>%
        mutate(pop_baseline_scenario = pop_baseline_scenario %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )

      pop <- pop %>%
        mutate(pop_impacted_scenario = pop_impacted_scenario %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )

      # PROJECT POPULATIONS IN BOTH IMPACTED AND BASELINE SCENARIO FROM YOA+1 UNTIL THE END
      # USING UNMODIFIED SURVIVAL PROBABILITIES (BECAUSE AFTER YOA THERE IS NO MORE AIR POLLUTION)

      pop <- pop %>%
        mutate(pop_baseline_scenario = pop_baseline_scenario %>%
                 purrr::map(
                   .,
                   function(.x){
                     project_pop(df = .x,
                                 prob_survival = .x$prob_survival,
                                 prob_survival_until_mid_year = .x$prob_survival_until_mid_year)
                   }
                 )
        )

      pop <- pop %>%
        mutate(pop_impacted_scenario = pop_impacted_scenario %>%
                 purrr::map(
                   .,
                   function(.x){
                     project_pop(df = .x,
                                 prob_survival = .x$prob_survival,
                                 prob_survival_until_mid_year = .x$prob_survival_until_mid_year)
                   }
                 )
        )

      ### CONSTANT EXPOSURE ########################################################################

    } else {

      # DETERMINE MID-YEAR POPULATION OF YOA+1 ...
      # ... USING UNMODIFIED SURVIVAL PROBABILITIES IN BASELINE SCENARIO
      pop <- pop %>%
        mutate(pop_baseline_scenario = pop_baseline_scenario %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )
      # ... USING MODIFIED SURVIVAL PROBABILITIES IN IMPACTED SCENARIO
      pop <- pop %>%
        mutate(pop_impacted_scenario = pop_impacted_scenario %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year_mod,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )

      # PROJECT POPULATION IN BASELINE SCENARIO
      pop <- pop %>%
        mutate(pop_baseline_scenario = pop_baseline_scenario %>%
                 purrr::map(
                   .,
                   function(.x){
                     project_pop(df = .x,
                                 prob_survival = .x$prob_survival,
                                 prob_survival_until_mid_year = .x$prob_survival_until_mid_year)
                   }
                 )
        )

      # PROJECT POPULATION IN IMPACTED SCENARIO
      pop <- pop %>%
        mutate(
          pop_impacted_scenario = pop_impacted_scenario %>%
            purrr::map(
              .,
              function(.x){
                project_pop(df = .x,
                            prob_survival = .x$prob_survival_mod,
                            prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
              }
            )
        )

    }

    ## DETERMINE POPULATION IMPACT #################################################################
    pop <- pop %>%
      mutate(yll_nest = purrr::map2(
        pop_impacted_scenario, pop_baseline_scenario,
        function(.x, .y){
        x <- .x %>%
            select(-contains("entry"), -population_2019_end) %>%
            select(contains("population"))
        y <- .y %>%
            select(-contains("entry"), -population_2019_end) %>%
            select(contains("population"))
        diff <- x - y
        return(diff)
        }
      )
      , .before = 1)

    pop <- pop %>%
      mutate(premature_deaths_nest = yll_nest %>%
               purrr::map(
                 .,
                 function(.x){
                   .x * 2
                 }
               )
      , .before = 1)

    ## COMPILE OUTPUT ##############################################################################
    if (outcome_metric == "deaths"){
    pop <- pop %>%
      mutate(pop_impact_nest = premature_deaths_nest %>%
               purrr::map(
                 .,
                 function(.x){
                   .x <- .x %>%
                     mutate(age = 0:99, .before = !!paste0("population_",year_of_analysis)) %>%
                     mutate(age_end = 1:100, .after = age)})
             , .before = 1) %>%
      select(-lifetable_with_pop_nest) # Remove from pop, as already present in input_with_risk_...
    }

    if ((outcome_metric == "yll") | (outcome_metric == "yld") | (outcome_metric == "daly")){
      pop <- pop %>%
        mutate(pop_impact_nest = yll_nest %>%
                 purrr::map(
                   .,
                   function(.x){
                     .x <- .x %>%
                       mutate(age = 0:99, .before = !!paste0("population_",year_of_analysis)) %>%
                       mutate(age_end = 1:100, .after = age)})
               , .before = 1) %>%
        select(-lifetable_with_pop_nest) # Remove from pop, as already present in input_with_risk_...
    }



    joining_columns_pop_impact <-
      bestcost:::find_joining_columns(input_with_risk_and_pop_fraction,
                                      pop,
                                      except = "lifetable_with_pop_nest")

    pop_impact <-
      input_with_risk_and_pop_fraction %>%
      dplyr::right_join(., pop, by = joining_columns_pop_impact)

    on.exit(options(user_options))
    return(pop_impact)

  }

  # # GeLuft APPROACH ##############################################################################

  # second_year <- year_of_analysis + 1
  #
  # ## DETERMINE POPULATION IMPACT
  # pop_impact <-
  #   input_with_risk_and_pop_fraction %>%
  #   dplyr::mutate(
  #     pop_impact_nest = purrr::map(
  #       lifetable_with_pop_nest,
  #       ~ dplyr::rename(
  #         .x,
  #         !!paste0("population_", year_of_analysis) := population))) %>%
  #   # Calculate the population the second year (first column after first year) considering the health effect of air pollution
  #   # And move column up one row: lead()
  #   dplyr::mutate(
  #     pop_impact_nest = purrr::map2(
  #       pop_impact_nest, pop_fraction,
  #       ~ dplyr::mutate(
  #         .x,
  #         "population_{second_year}" :=
  #           lead(
  #             dplyr::lag(!!as.symbol(paste0("population_",year_of_analysis))) *
  #               dplyr::lag(prob_natural_death) * .y
  #           )
  #       )))
  #
  # if(outcome_metric %in% c("yll", "yld", "daly")){
  #
  #   # Now calculate population over time for the rest of year starting with YOA without considering air pollution
  #   period <- c( (year_of_analysis + 1) :
  #                  ((year_of_analysis +
  #                      unique(purrr::map_int(input_with_risk_and_pop_fraction$lifetable_with_pop_nest,
  #                                            ~nrow(.x))) - 2)) )
  #   length_period <- length(period)
  #   population_period <- paste0("population_", period)
  #
  #
  #   pop_impact <-
  #     pop_impact %>%
  #     dplyr::mutate(
  #       pop_impact_nest = purrr::map(
  #         pop_impact_nest,
  #         function(.x) {
  #           # length_period minus 1 because year_of_analysis+1 is already calculated
  #           for (i in 0:(length_period-1)){
  #             current_year <- period[i+1]
  #             col_current <- paste0("population_", current_year)
  #             col_next <- paste0("population_", current_year + 1)
  #             # avoiding the later introduction of NAs in the right top corner:
  #             .x[1:(length_period-i), col_next] <-
  #               .x[1:(length_period-i), col_current] * (1 - .x$prob_total_death[(i+2):(length_period+1)])
  #
  #
  #             # Alternative code
  #             # Simpler but it does not provide the right result
  #             # .x[[col_next]] <-
  #             #  .x[[col_current]] * (1 - .x[["prob_total_death"]])
  #           }
  #           return(.x)
  #         } ))
  #
  #   # COMPILE OUTPUT
  #   years <- 2020:2118
  #   pop_impact <-
  #     pop_impact %>%
  #     dplyr::mutate(
  #       pop_impact_nest = purrr::map(
  #         pop_impact_nest,
  #         function(.x) {
  #           # length_period minus 1 because year_of_analysis+1 is already calculated
  #           for (i in 1:99){
  #             current_year <- years[i]
  #             col_current <- paste0("population_", current_year)
  #             # avoiding the later introduction of NAs in the right top corner:
  #             .x[, col_current] <- lag(.x[,col_current], n = i)
  #           }
  #           return(.x)
  #         } ))
  #
  # }
  # return(pop_impact)
  # }



