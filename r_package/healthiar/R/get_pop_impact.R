#' Get population impact over time

#' @description Get population impact over time
#' @inheritParams attribute
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
#' @importFrom stringr str_replace
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal

get_pop_impact <-
  function(input_with_risk_and_pop_fraction,
           outcome_metric
           ){

    user_options <- options()
    options(digits = 15)

    if ((outcome_metric == "yld") | (outcome_metric == "daly")){
      # If there are disability weights or duration in the input (i.e. if it's a YLD calculation),
      # the lifetable calculations will only be done for the rows where the
      # column "dw_ci" & "duration_ci" has the value "central" (to improve performance).
      # The resulting (nested) lifetable tibbles will be left_join()'ed with "input_backup"
      # at the end of the script.
      input_backup <- input_with_risk_and_pop_fraction
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        filter(dw_ci == "central") |>
        filter(duration_ci == "central")
    }

    # LIFETABLE SETUP ##############################################################################

    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(modification_factor = 1 - pop_fraction, .after = rr) |> # WORKS WITH BOTH SINGLE EXPOSURE VALUE AND EXPOSURE DISTRIBUTION AS INPUTS
      # ADD THE MODIFICATION FACTOR TO THE NESTED TIBBLE "LIFETABLE_WITH_POP_NEST" USING FCT PMAP()
      dplyr::mutate(lifetable_with_pop_nest =
                      purrr::pmap(
                        list(lifetable_with_pop_nest, modification_factor),
                        function(lifetable_with_pop_nest, modification_factor){
                          lifetable_with_pop_nest <- lifetable_with_pop_nest |>
                            dplyr::mutate(modification_factor = modification_factor)
                        }
                      )
      )
# browser()
    # Store variables for population_year
    # Year Of Analysis (YOA)
    population_yoa <- paste0("population_", input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first())
    population_yoa_entry <- paste0(population_yoa,"_entry")
    population_yoa_plus_1_entry <- paste0("population_", input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first()+1,"_entry")
    population_yoa_end <- paste0("population_", input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first(),"_end")
    deaths_yoa <- paste0("deaths_", input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first())


    # ADD ENTRY POPULATION OF YOA & SURVIVAL PROBABILITIES
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::relocate(lifetable_with_pop_nest, .before = 1) |>
      dplyr::mutate(
        lifetable_with_pop_nest =
          purrr::map(
            .x = lifetable_with_pop_nest,
            function(.x){

              .x <- .x |>
                dplyr::select(age, age_end, deaths, population, modification_factor) |>
                dplyr::rename(!!population_yoa := population) |>

                # CALCULATE ENTRY POPULATION OF YEAR OF ANALYSIS (YOA)
                dplyr::mutate(
                  !!population_yoa_entry := !!sym(population_yoa) + (deaths / 2),
                  .before = !!population_yoa) |>

                # CALCULATE PROBABILITY OF SURVIVAL FROM START YEAR TO END YEAR & START YEAR TO MID YEAR
                # probability of survival from start of year i to start of year i+1 (entry to entry)
                dplyr::mutate(
                  prob_survival =
                    (!!sym(population_yoa) - (deaths / 2)) /
                    (!!sym(population_yoa) + (deaths / 2) ),
                  .after = deaths) |>

                # Probability of survival from start to midyear
                # For example entry_pop = 100, prob_survival = 0.8 then end_of_year_pop = 100 * 0.8 = 80.
                # mid_year_pop = 100 - (20/2) = 90.
                dplyr::mutate(
                  prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2),
                  .after = deaths) |>

                # Hazard rate for calculating survival probabilities
                dplyr::mutate(
                  hazard_rate = deaths / !!sym(population_yoa),
                  .after = deaths)
            }
          )
      )

    # CALCULATE MODIFIED SURVIVAL PROBABILITIES
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(
        lifetable_with_pop_nest =
          purrr::map(
            .x = lifetable_with_pop_nest,
            function(.x){
              .x <- .x |>
              # For all ages min_age and higher calculate modified survival probabilities
              # Calculate modified hazard rate = modification factor * hazard rate = mod factor * (deaths / mid-year pop)
              dplyr::mutate(
                hazard_rate_mod =
                  dplyr::if_else(age_end > c(rep_len(input_with_risk_and_pop_fraction |>  pull(min_age) |> first(), length.out = length(age_end))), # This makes sure comparators are of same length
                                 modification_factor * hazard_rate,
                                 hazard_rate),
                .after = deaths) |>

              # Calculate modified survival probability =
              # ( 2 - modified hazard rate ) / ( 2 + modified hazard rate )
              dplyr::mutate(
                prob_survival_mod =
                  dplyr::if_else(age_end > c(rep_len(input_with_risk_and_pop_fraction |>  pull(min_age) |> first(), length.out = length(age_end))), # This makes sure comparators are of same length
                                 (2 - hazard_rate_mod) / (2 + hazard_rate_mod),
                                 prob_survival),
                .after = deaths) |>

              dplyr::mutate(
                prob_survival_until_mid_year_mod =
                  dplyr::if_else(age_end > c(rep_len(input_with_risk_and_pop_fraction |>  pull(min_age) |> first(), length.out = length(age_end))), # This makes sure comparators are of same length
                                 1 - ((1 - prob_survival_mod) / 2),
                                 prob_survival_until_mid_year),
                .after = deaths)
            }

               )
      )

    ## BASELINE SCENARIO ###########################################################################
    # The baseline scenario is the scenario of "business as usual"
    # i.e. the scenario with the exposure to the environmental stressor as (currently) measured

    # DETERMINE ENTRY POPULATION OF YOA+1 IN BASELINE SCENARIO
    pop <- input_with_risk_and_pop_fraction |>
      dplyr:::mutate(
        pop_baseline_scenario_nest =
          purrr::map(
            .x = lifetable_with_pop_nest,
            function(.x){
              .x <- .x |>
                # End-of-year population YOA = entry pop YOA * ( survival probability )
                dplyr::mutate(
                  !!population_yoa_end :=
                    !!sym(population_yoa_entry) * prob_survival) |>

                # Deaths YOA = End pop YOA - Entry pop YOA
                dplyr::mutate(
                  !!deaths_yoa :=
                    !!sym(population_yoa_entry) - !!sym(population_yoa_end),
                  .after =  !!sym(population_yoa)) |>

                # Entry population YOA+1 = lag ( End-of-year population YOA )
                dplyr::mutate(
                  !!population_yoa_plus_1_entry :=
                    lag(!!sym(population_yoa_end)))
              }
            )
        , .after = lifetable_with_pop_nest)

    ## IMPACTED SCENARIO ###########################################################################
    # The impacted scenario is the scenario without any exposure to the environmental stressor


    # CALCULATE YOA MID-YEAR POPOULATION,
    # YOA END-OF-YEAR POPULATION, YOA DEATHS AND
    # YOA+1 ENTRY POPULATION USING MODIFIED SURVIVAL PROBABILITIES
    pop <- pop |>
      dplyr::mutate(
        pop_impacted_scenario_nest =
          purrr::map(
            .x = lifetable_with_pop_nest ,
            function(.x){
              .x <- .x |>

              # MID-YEAR POP = (ENTRY POP) * ( survival probability until mid year )
              dplyr::mutate(!!population_yoa :=
                              !!sym(population_yoa_entry) * prob_survival_until_mid_year_mod) |>

              # Calculate end-of-year population in YOA to later determine premature deaths
              dplyr::mutate(!!population_yoa_end :=
                              !!sym(population_yoa_entry) * prob_survival_mod) |>

              # Deaths YOA = End pop YOA - Entry pop YOA
              dplyr::mutate(!!deaths_yoa :=
                              !!sym(population_yoa_entry) - !!sym(population_yoa_end),
                            .after =  !!sym(population_yoa)) |>

              # Entry population YOA+1 = lag ( End-of-year population YOA )
              dplyr::mutate(!!population_yoa_plus_1_entry :=
                              lag(!!sym(population_yoa_end)))

           }
         )
       , .after = pop_baseline_scenario_nest
      )

    # PREMATURE DEATHS (SINGLE YEAR EXPOSURE) ######################################################
    # YOA = YEAR OF ANALYSIS
    if (outcome_metric == "deaths" &
        unique(input_with_risk_and_pop_fraction |> select(contains("approach_exposure"))== "single_year")[1]) {

      pop <- pop |>
        # Premature deaths = ( impacted scenario YOA end-of-year population ) - ( baseline scenario YOA end-of-year pop )
        dplyr::mutate(
          premature_deaths_nest =
            purrr::map2(
              .x = pop_impacted_scenario_nest,
              .y = pop_baseline_scenario_nest,
              ~ dplyr::tibble(
                age = .x$age,
                age_end = .x$age_end,
                deaths_2019 = .x$population_2019_end - .y$population_2019_end)),
          .after = pop_impacted_scenario_nest)

    }

    # YLL & PREMATURE DEATHS (CONSTANT EXPOSURE) ####################################################

    if ((outcome_metric %in% c("yll", "yld", "daly") |
         (unique(input_with_risk_and_pop_fraction |> select(contains("approach_exposure")) == "constant")[1] & outcome_metric == "deaths"))) {

      ## PROJECT POPULATIONS #########################################################################

      ### DEFINE FUNCTION FOR POPULATION PROJECTION ##################################################

      project_pop <- function(df, prob_survival, prob_survival_until_mid_year) {
        # Store useful variables such number_years
        # The number_years argument defines for how many years the population should be projected;
        # might be easier to have two arguments "start year" and "end year"
        number_years <-
          nrow(df) - 1

        # Define the years based on number_years
        # e.g. 2020 to 2118
        years_projection <-
          (input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first() + 1) : (input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first() + number_years)

        # Initialize matrices for entry population, mid-year population, and deaths
        pop_entry <- matrix(NA, nrow = 100, ncol = number_years)
        # Provide column names (population_year)
        # e.g. population_2020 to population_2118
        colnames(pop_entry) <-
          paste0("population_", years_projection , "_entry")

        # Same for mid-year population
        pop_mid <- matrix(NA, nrow = 100, ncol = number_years)
        colnames(pop_mid) <-
          paste0("population_", years_projection)

        # Same for deaths
        deaths <- matrix(NA, nrow = 100, ncol = number_years)
        colnames(deaths) <-
          paste0("deaths_", years_projection)

        # Set initial population for the first year (2020)
        pop_entry[, 1] <- df[[paste0("population_", input_with_risk_and_pop_fraction |>  pull(year_of_analysis) |> first() + 1, "_entry")]]
        pop_mid[, 1] <- pop_entry[, 1] * prob_survival_until_mid_year
        deaths[, 1] <- pop_entry[, 1] * (1 - prob_survival)

        # Loop across years
        # E.g. starts with 1 and ends with 98;
        # i (index in the number of years) is used to select both the rows and the columns

        for (i in 1:(number_years-1)) {

          # ENTRY POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY YOA )
          pop_entry[(i + 2):(number_years + 1), i + 1] <-
            pop_entry[(i + 1):(number_years), i] * prob_survival[(i + 1):(number_years)]

          # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA+1) * ( SURVIVAL PROBABILITY FROM START OF YOA+1 TO MID YEAR YOA+1)
          pop_mid[(i + 2):(number_years+1), i + 1] <-
            pop_entry[(i + 2):(number_years + 1), i + 1] * prob_survival_until_mid_year[(i + 2):(number_years + 1)]

          # DEATHS IN YOA+1 <- ( ENTRY POP YOA+1 ) * (1 - SURVIVAL PROBABILITY YOA+1 )
          deaths[(i + 2):(number_years+1), i + 1] <-
            pop_entry[(i + 2):(number_years + 1), i + 1] * ( 1 - prob_survival[(i + 2):(number_years + 1)] )

        }

        # Column bin matrices to input data frame
        # Remove first column of pop_entry, because it exists already in input data frame
        df <- df |>
          dplyr::bind_cols(pop_mid) |>
          dplyr::bind_cols(pop_entry[, -1]) |>
          dplyr::bind_cols(deaths)

        return(df)
      }

      ### SINGLE YEAR EXPOSURE #######################################################################
      # Determine YLLs for baseline and impacted scenario's in the single year exposure case

      if (unique(input_with_risk_and_pop_fraction |> select(contains("approach_exposure")) == "single_year")[1]){

        # PROJECT POPULATIONS IN BOTH IMPACTED AND BASELINE SCENARIO FROM YOA+1 UNTIL THE END
        # USING MODIFIED SURVIVAL PROBABILITIES (BECAUSE AFTER YOA THERE IS NO MORE AIR POLLUTION)
        pop <- pop |>
          dplyr::mutate(
            pop_baseline_scenario_nest =
              purrr::map(
                .x = pop_baseline_scenario_nest,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival_mod,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
                  }
                )
            )

        pop <- pop |>
          dplyr::mutate(
            pop_impacted_scenario_nest =
              purrr::map(
                .x = pop_impacted_scenario_nest,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival_mod,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
                  }
                )
            )

        ### CONSTANT EXPOSURE ########################################################################
        # Determine YLLs for baseline and impacted scenario's in the constant exposure case

      } else {

        # PROJECT POPULATION IN BASELINE SCENARIO
        pop <- pop |>
          dplyr::mutate(
            pop_baseline_scenario_nest =
              purrr::map(
                .x = pop_baseline_scenario_nest,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year)
                  }
                )
            )

        # PROJECT POPULATION IN IMPACTED SCENARIO
        pop <- pop |>
          dplyr::mutate(
            pop_impacted_scenario_nest =
              purrr::map(
                .x = pop_impacted_scenario_nest,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival_mod,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
                }
              )
          )
      }

      ###  DETERMINE IMPACT (YLL, PREMATURE DEATHS (CONSTANT EXPOSURE))  ###########################
      # YLL and premature deaths attributable to exposure are calculated

      pop <- pop |>
        dplyr::mutate(
          yll_nest = purrr::map2(
            .x = pop_impacted_scenario_nest,
            .y = pop_baseline_scenario_nest,

            function(.x, .y){

              ages <- .x |>
                dplyr::select(age, age_end)

              pop_impacted <- .x |>
                dplyr::select(contains("population"),
                              -population_2019_end,
                              -contains("entry"))
              pop_baseline <- .y |>
                dplyr::select(contains("population"),
                              -population_2019_end,
                              -contains("entry"))

              # Difference in mid-year populations of baseline and impacted scenario equals attributable YLL
              pop_diff <-
                pop_impacted - pop_baseline

              # Add ages (in other pipeline because it does not work in one)
              pop_diff <-
                dplyr::bind_cols(ages, pop_diff)

                return(pop_diff)
            }

          ),
          .before = 1)




      pop <- pop |>
       dplyr::mutate(
         premature_deaths_nest = purrr::map2(
            .x = pop_baseline_scenario_nest,
            .y = pop_impacted_scenario_nest,
            function(.x, .y){

              pop_baseline <- .x |>
                dplyr::select(contains("deaths"),
                              -deaths)
              pop_impacted <- .y |>
                dplyr::select(contains("deaths"),
                              -deaths)

              ages <- .x |>
                dplyr::select(age, age_end)

              # Calculate difference in deaths
              # Baseline scenario minus impacted scenario
              pop_diff <- pop_baseline - pop_impacted

              # Add ages (in other pipeline because it does not work in one)
              pop_diff <-
                dplyr::bind_cols(ages, pop_diff)



            return(pop_diff)
          }
        )
        , .before = 1)


      # Rename column names
      pop <- pop |>
        dplyr::mutate(
          premature_deaths_nest =
            purrr::map(
              .x = premature_deaths_nest,
              ~.x |>
                # replace "deaths" with "population"
                # dplyr::rename_with(~ stringr::str_replace(., "deaths", "population"))
                dplyr::rename_with(~ gsub("population", "deaths", .), contains("population"))
            ))

      ## NEWBORNS #################################################################

      if (unique(input_with_risk_and_pop_fraction |> select(contains("approach_newborns")) == "with_newborns")[1]) {

        fill_right_of_diag <- function(tbl) {
          for (i in seq_len(nrow(tbl))) {
            # Extract the diagonal value
            diag_value <- tbl[i, i, drop = TRUE]
            # Replace NAs to the right of the diagonal with the diagonal value
            tbl[i, (i+1):ncol(tbl)] <- diag_value
          }
          tbl <- tbl |>
            dplyr::select(-ncol(tbl))
          return(tbl)
        }

        pop <- pop |>
          dplyr::mutate(
            yll_nest = purrr::map(
              .x = yll_nest,
              function(.x){
                .x[, setdiff(names(.x), c("age", "age_end"))] <- fill_right_of_diag(.x[, setdiff(names(.x), c("age", "age_end"))])
                return(.x)
              }
              )
            , .before = 1)

        pop <- pop |>
          dplyr::mutate(premature_deaths_nest = purrr::map(
            .x = premature_deaths_nest,
            function(.x){
              .x[, setdiff(names(.x), c("age", "age_end"))] <- fill_right_of_diag(.x[, setdiff(names(.x), c("age", "age_end"))])
              return(.x)
            }
          )
          , .before = 1)

      }

    }

    # COMPILE OUTPUT ##############################################################################
    # Data wrangling to get the results in the needed format
    pop <- pop |>
      dplyr::mutate(
        pop_impact_nest =
          if(outcome_metric == "deaths") premature_deaths_nest else yll_nest)

    # Remove from pop, as already present in input_with_risk_...
    pop <- pop |>
      dplyr::select(-lifetable_with_pop_nest)

    if (outcome_metric != "yld" & outcome_metric != "daly"){ # YLL & premature deaths case

      joining_columns_pop_impact <-
        healthiar:::find_joining_columns(input_with_risk_and_pop_fraction,
                                        pop,
                                        except = "lifetable_with_pop_nest")

      pop_impact <-
        input_with_risk_and_pop_fraction |>
        dplyr::right_join(pop, by = joining_columns_pop_impact) |>
        relocate(contains("nest"), .before = 1)

    } else { # YLD case

      pop <- pop |>
        select(geo_id_raw, contains("exp"), contains("prop_pop_exp"), rr, erf_ci, sex, # Variables to merge by
               -contains("_2"), # Remove all "..._2" variables (e.g. "exp_2"); relevant in "compare_..." function calls
               contains("_nest"),
               -contains("approach_exposure"),
               -contains("exposure_dimension"),
               -contains("exposure_type"),
               -contains("exp_ci"))

      if( is_empty((grep("_1", names(pop))))){
        pop_impact <- input_backup |>
        dplyr::left_join(pop, by = c("geo_id_raw", "exp", "prop_pop_exp", "rr", "erf_ci", "sex", "exposure_name"))
        }else{
          pop_impact <- input_backup |>
          # attribute_... cases
          dplyr::left_join(pop, by = c("geo_id_raw", "exp_1", "prop_pop_exp_1", "rr", "erf_ci", "sex", "exposure_name")) # compare_... cases
        }

    }


    on.exit(options(user_options))

    return(pop_impact)

  }
