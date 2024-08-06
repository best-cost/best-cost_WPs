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

    user_options <- options()
    options(digits = 15)

    # LIFETABLE SETUP ############################################################################

    input_with_risk_and_pop_fraction <-
      input_with_risk_and_pop_fraction %>%
        dplyr::mutate(modification_factor = (1 / rr_conc), .after = rr)

    # ADD THE MODIFICATION FACTOR TO THE NESTED TIBBLE "LIFETABLE_WITH_POP_NEST" USING FCT PMAP()
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction %>%
      dplyr::mutate(lifetable_with_pop_nest =
               purrr::pmap(
                 list(lifetable_with_pop_nest, modification_factor),
                 function(lifetable_with_pop_nest, modification_factor){
                   lifetable_with_pop_nest <- lifetable_with_pop_nest %>%
                     dplyr::mutate(modification_factor = modification_factor)
                 }
               )
      )

    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction %>%
      dplyr::relocate(lifetable_with_pop_nest, .before = 1) %>%
      dplyr::mutate(
        lifetable_with_pop_nest =
          lifetable_with_pop_nest %>%
          purrr::map(
            .,
            function(.x){
              .x <- .x %>%
                dplyr::select(age, age_end, deaths, population, modification_factor) %>%
                dplyr::rename(!!paste0("population_",year_of_analysis) := population) %>%
                # CALCULATE ENTRY POPULATION OF YOA
                dplyr::mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
                # CALCULATE PROBABILITY OF SURVIVAL FROM START YEAR TO END YEAR & START YEAR TO MID YEAR
                dplyr::mutate(prob_survival = ( !!sym(paste0("population_",year_of_analysis)) - ( deaths / 2 ) ) / (!!sym(paste0("population_",year_of_analysis)) + ( deaths / 2 ) ), .after = deaths) %>% # probability of survival from start of year i to start of year i+1 (entry to entry)
                dplyr::mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths)
            }
          )
      )

    # DETERMINE POPULATIONS IN YOA AND YOA+1 #######################################################
    # YOA = YEAR OF ANALYSIS

    ## BASELINE SCENARIO ###########################################################################

    # DETERMINE ENTRY POPULATION OF YOA+1 IN BASELINE SCENARIO
    pop <- input_with_risk_and_pop_fraction %>%
      mutate(pop_baseline_scenario_nest = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 function(.x){

                   # Entry population YOA+1 = ( Entry pop YOA ) * ( survival probability )
                   .x <- .x %>%
                     # Calculate end-of-year population in YOA to later determine premature deaths
                     mutate(!!paste0("population_",year_of_analysis,"_end") :=
                              !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival) %>%
                     # Shift end-of-year population one row down to get the diagonal shift from year to year
                     # End-of-year population YOA = entry pop YOA+1
                     mutate(!!paste0("population_",year_of_analysis+1,"_entry") :=
                              lag(!!sym(paste0("population_",year_of_analysis,"_end"))))
                 }
               )
             , .after = lifetable_with_pop_nest)

    ## IMPACTED SCENARIO ###########################################################################

    # CALCULATE MODIFIED SURVIVAL PROBABILITIES
    pop <- pop %>%
      mutate(pop_impacted_scenario_nest = lifetable_with_pop_nest %>%
               purrr::map(
                 .,
                 function(.x){
                   .x <- .x %>%
                     # For all ages min_age and higher calculate modified survival probabilities
                     # Calculate modified hazard rate = modification factor * hazard rate = mod factor * (deaths / mid-year pop)
                     mutate(hazard_rate = if_else(row_number() > min_age, modification_factor * deaths / !!sym(paste0("population_",year_of_analysis)), deaths / !!sym(paste0("population_",year_of_analysis))), .after = deaths) %>% # Hazard rate for calculating survival probabilities

                     # Calculate modified survival probability = ( 2 - modified hazard rate ) / ( 2 + modified hazard rate )
                     mutate(prob_survival_mod = if_else(row_number() > min_age, (2 - hazard_rate) / (2 + hazard_rate), prob_survival), .after = deaths) %>%
                     mutate(prob_survival_until_mid_year_mod = if_else(row_number() > min_age, 1 - ((1 - prob_survival_mod) / 2), prob_survival_until_mid_year), .after = deaths)
                 }
               )
             , .after = pop_baseline_scenario_nest)

    # CALCULATE MID-YEAR POPULATION OF YOA USING MODIFIED SURVIVAL PROBABILITIES
    pop <- pop %>%
      mutate(pop_impacted_scenario_nest = pop_impacted_scenario_nest %>%
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
      mutate(pop_impacted_scenario_nest = pop_impacted_scenario_nest %>%
               purrr::map(
                 .,
                 function(.x){

                   .x <- .x %>%
                     # Calculate end-of-year population in YOA to later determine premature deaths
                     mutate(!!paste0("population_",year_of_analysis,"_end") :=
                              !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival_mod) %>%

                     # Shift end-of-year population one row down to get the diagonal shift from year to year
                     # End-of-year population YOA = entry pop YOA+1
                     mutate(!!paste0("population_",year_of_analysis+1,"_entry") :=
                              lag(!!sym(paste0("population_",year_of_analysis,"_end"))))
                 }
               )
      )

    ## DETERMINE PREMATURE DEATHS IN YOA ###########################################################
    # Premature deaths = impacted scenario YOA end-of-year population - baseline scenario YOA end-of-year pop

    pop <- pop %>%
      mutate(premature_deaths_nest = purrr::map2(
        pop_impacted_scenario_nest, pop_baseline_scenario_nest,
        ~ tibble(premature_deaths = .x$population_2019_end - .y$population_2019_end)),
        .after = pop_impacted_scenario_nest)

    ## PROJECT POPULATIONS #########################################################################

    # DEFINE FUNCTION FOR POPULATION PROJECTION
    project_pop <- function(df, prob_survival, prob_survival_until_mid_year){

    # Define loop variables (to be used in both modelled and cutoff population projection)
      years <- c((year_of_analysis+1):(year_of_analysis + (nrow(pop[["lifetable_with_pop_nest"]][[1]]))))
      length_period <- length(years)

      for (i in 1:(length_period - 2)) { # starts with 1; ends with 98 (years 2020 - 2118)

        # print(i)

        # ENTRY POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY )
        df[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
          df[i:(length_period - 1), paste0("population_", years[i], "_entry")] * prob_survival[i:(length_period - 1)]

        # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY FROM START OF YEAR TO MID YEAR)
        df[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          df[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * prob_survival_until_mid_year[(i + 1):length_period]
      }

      df <- df

      return(df)

    }

    ### SINGLE YEAR EXPOSURE #######################################################################
    # # Determine YLLs for baseline and impacted scenario's in the single year exposure case

    # This code is for the single year exposure case
    if (unique(input_with_risk_and_pop_fraction$approach_exposure == "single_year")){

      # DETERMINE MID-YEAR POPULATION OF YOA+1 IN BOTH IMPACTED AND BASELINE SCENARIO
      # USING UNMODIFIED SURVIVAL PROBABILITIES
      pop <- pop %>%
        mutate(pop_baseline_scenario_nest = pop_baseline_scenario_nest %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY FROM START OF YEAR TO MID YEAR)
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )

      pop <- pop %>%
        mutate(pop_impacted_scenario_nest = pop_impacted_scenario_nest %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY FROM START OF YEAR TO MID YEAR)
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )

      # PROJECT POPULATIONS IN BOTH IMPACTED AND BASELINE SCENARIO FROM YOA+1 UNTIL THE END
      # USING UNMODIFIED SURVIVAL PROBABILITIES (BECAUSE AFTER YOA THERE IS NO MORE AIR POLLUTION)

      pop <- pop %>%
        mutate(pop_baseline_scenario_nest = pop_baseline_scenario_nest %>%
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
        mutate(pop_impacted_scenario_nest = pop_impacted_scenario_nest %>%
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
      # Determine YLLs for baseline and impacted scenario's in the constant exposure case

    } else {

      # DETERMINE MID-YEAR POPULATION OF YOA+1 ...
      # ... USING UNMODIFIED SURVIVAL PROBABILITIES IN BASELINE SCENARIO
      pop <- pop %>%
        mutate(pop_baseline_scenario_nest = pop_baseline_scenario_nest %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY FROM START OF YEAR TO MID YEAR)
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )
      # ... USING MODIFIED SURVIVAL PROBABILITIES IN IMPACTED SCENARIO
      pop <- pop %>%
        mutate(pop_impacted_scenario_nest = pop_impacted_scenario_nest %>%
                 purrr::map(
                   .,
                   function(.x){

                     .x <- .x %>%
                       # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA ) * ( MODIFIED SURVIVAL PROBABILITY FROM START OF YEAR TO MID YEAR)
                       mutate(!!paste0("population_",year_of_analysis+1) :=
                                !!sym(paste0("population_",year_of_analysis+1,"_entry")) * prob_survival_until_mid_year_mod,
                              .after = !!paste0("population_",year_of_analysis+1,"_entry"))
                   }
                 )
        )

      # PROJECT POPULATION IN BASELINE SCENARIO
      pop <- pop %>%
        mutate(pop_baseline_scenario_nest = pop_baseline_scenario_nest %>%
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
          pop_impacted_scenario_nest = pop_impacted_scenario_nest %>%
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

    # DETERMINE POPULATION IMPACT ##################################################################
    # YLL and premature deaths attributable to exposure are calculated

    pop <- pop %>%
      mutate(yll_nest = purrr::map2(
        pop_impacted_scenario_nest, pop_baseline_scenario_nest,
        function(.x, .y){
        x <- .x %>%
            select(-contains("entry"), -population_2019_end) %>%
            select(contains("population"))
        y <- .y %>%
            select(-contains("entry"), -population_2019_end) %>%
            select(contains("population"))
        # Difference in mid-year populations of baseline and impacted scenario equals attributable YLL
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
                   # Determine premature deaths
                   # For every YLL, two people have died at mid year --> YLL * 2 = premature deaths
                   .x * 2
                 }
               )
      , .before = 1)

    ## COMPILE OUTPUT ##############################################################################
    # Data wrangling to get the results in the needed format

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
               , .before = 1)
    }

    pop <- pop %>%
      select(-lifetable_with_pop_nest) # Remove from pop, as already present in input_with_risk_...

    joining_columns_pop_impact <-
      bestcost:::find_joining_columns(input_with_risk_and_pop_fraction,
                                      pop,
                                      except = "lifetable_with_pop_nest")

    pop_impact <-
      input_with_risk_and_pop_fraction %>%
      dplyr::right_join(., pop, by = joining_columns_pop_impact) %>%
      relocate(contains("nest"), .before = 1)

    on.exit(options(user_options))

    return(pop_impact)

  }
