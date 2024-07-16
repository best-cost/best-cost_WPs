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
           outcome_metric){

    # AirQ+ ########################################################################################
    if (outcome_metric %in% c("yll_airqplus")) {

      # FACTOR TO DETERMINE PROBABILITY OF DYING IN COUNTERFACTUAL SCENARIO
      ### NOTE: ADD HERE THE CALCULATION OF THE BETA VALUE BASED ON THE RR #########################
      MODIFICATION_FACTOR <- exp(0.0111541374732907 * (5 - 8.85)) # Based on AirQ+ lifetable manual formula 7 on p 17)
      # Formula 7: RR(x_1 - x_0) = exp( beta * (x_1 - x_0) )

      # MALE POPULATION ############################################################################
      pop_male <- lifetable_with_pop %>%
        filter(sex == "male") %>%
        select(lifetable_with_pop_nest) %>%
        unnest(cols = c(lifetable_with_pop_nest)) %>%
        select(age,
               deaths,
               !!paste0("population_",year_of_analysis) := population)

      pop_modelled_male <- pop_male %>%
        mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
        mutate(prob_survival = 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry"))), .after = deaths) %>% # probability of survival from start of year i to start of year i+1 (entry to entry)
        mutate(prob_survival_until_mid_year = 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry")) / 2), .after = deaths)

      pop_cutoff_male <- pop_male %>%
        mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
        mutate(hazard_rate = MODIFICATION_FACTOR * deaths / !!sym(paste0("population_",year_of_analysis)), .after = deaths) %>% # Hazard rate for calculating survival probabilities
        mutate(prob_survival = (2 - hazard_rate) / (2 + hazard_rate), .after = deaths) %>%
        mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths) %>%
        select(-hazard_rate)

      pop_cutoff_male[1:20, "prob_survival"] <- pop_modelled_male[1:20, "prob_survival"]
      pop_cutoff_male[1:20, "prob_survival_until_mid_year"] <- pop_modelled_male[1:20, "prob_survival_until_mid_year"]

      pop_cutoff_male <- pop_cutoff_male %>%
        # Re-calculate the "pop_2019_mid_year_total" using the modified survival rates
      mutate(!!paste0("population_",year_of_analysis) := !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival_until_mid_year)

      years <- c(year_of_analysis:(year_of_analysis + (nrow(pop_modelled_male) - 1)))
      length_period <- length(years)

      # PROJECT MODELLED POPULATION
      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_modelled_male[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
          pop_modelled_male[i:(length_period - 1), paste0("population_", years[i], "_entry")] * pop_modelled_male[i:(length_period - 1), "prob_survival"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_modelled_male[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_modelled_male[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * pop_modelled_male[(i + 1):length_period, "prob_survival_until_mid_year"]

      }

      # PROJECT CUTOFF POPULATION
      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_cutoff_male[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
          pop_cutoff_male[i:(length_period - 1), paste0("population_", years[i], "_entry")] * pop_cutoff_male[i:(length_period - 1), "prob_survival"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_cutoff_male[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_cutoff_male[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * pop_cutoff_male[(i + 1):length_period, "prob_survival_until_mid_year"]
      }

      # FEMALE POPULATION ##########################################################################
      pop_female <- lifetable_with_pop %>%
        filter(sex == "female") %>%
        select(lifetable_with_pop_nest) %>%
        unnest(cols = c(lifetable_with_pop_nest)) %>%
        select(age,
               deaths,
               !!paste0("population_",year_of_analysis) := population)

      pop_modelled_female <- pop_female %>%
        mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
        mutate(prob_survival = 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry"))), .after = deaths) %>% # probability of survival from start of year i to start of year i+1 (entry to entry)
        mutate(prob_survival_until_mid_year = 1 - (deaths / !!sym(paste0("population_",year_of_analysis,"_entry")) / 2), .after = deaths)

      pop_cutoff_female <- pop_female %>%
        mutate(!!paste0("population_",year_of_analysis,"_entry") := !!sym(paste0("population_",year_of_analysis)) + (deaths / 2), .before = !!paste0("population_",year_of_analysis)) %>%
        mutate(hazard_rate = MODIFICATION_FACTOR * deaths / !!sym(paste0("population_",year_of_analysis)), .after = deaths) %>% # Hazard rate for calculating survival probabilities
        mutate(prob_survival = (2 - hazard_rate) / (2 + hazard_rate), .after = deaths) %>%
        mutate(prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2), .after = deaths) %>%
        select(-hazard_rate)

      pop_cutoff_female[1:20, "prob_survival"] <- pop_modelled_female[1:20, "prob_survival"]
      pop_cutoff_female[1:20, "prob_survival_until_mid_year"] <- pop_modelled_female[1:20, "prob_survival_until_mid_year"]

      pop_cutoff_female <- pop_cutoff_female %>%
        # Re-calculate the "pop_2019_mid_year_total" using the modified survival rates
      mutate(!!paste0("population_",year_of_analysis) := !!sym(paste0("population_",year_of_analysis,"_entry")) * prob_survival_until_mid_year)

      years <- c(year_of_analysis:(year_of_analysis + (nrow(pop_modelled_female) - 1)))
      length_period <- length(years)

      # PROJECT MODELLED POPULATION
      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_modelled_female[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
          pop_modelled_female[i:(length_period - 1), paste0("population_", years[i], "_entry")] * pop_modelled_female[i:(length_period - 1), "prob_survival"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_modelled_female[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_modelled_female[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * pop_modelled_female[(i + 1):length_period, "prob_survival_until_mid_year"]

      }

      # PROJECT CUTOFF POPULATION
      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_cutoff_female[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] <-
          pop_cutoff_female[i:(length_period - 1), paste0("population_", years[i], "_entry")] * pop_cutoff_female[i:(length_period - 1), "prob_survival"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_cutoff_female[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_cutoff_female[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry")] * pop_cutoff_female[(i + 1):length_period, "prob_survival_until_mid_year"]
      }

      # TOTAL POPULATION ###########################################################################
      # Prepare pop_male for setup total population
      pop_male <- pop_male %>%
        rename(deaths_male = deaths) %>%
        rename(population_mid_year_male = !!sym(paste0("population_",year_of_analysis))) %>%
        select(-age) # Remove "age" here but keep in df "pop_female"

      # Prepare pop_female for setup total population
      pop_female <- pop_female %>%
        rename(deaths_female = deaths) %>%
        rename(population_mid_year_female = !!sym(paste0("population_",year_of_analysis)))

      pop_modelled_total <- cbind(pop_male, pop_female) %>%
        select(age,
               population_mid_year_male,
               deaths_male,
               population_mid_year_female,
               deaths_female) %>%
        mutate(!!paste0("population_",year_of_analysis) := population_mid_year_male + population_mid_year_female,
               deaths_total = deaths_male + deaths_female) %>%
        mutate(!!paste0("population_",year_of_analysis,"_entry_total") := !!sym(paste0("population_",year_of_analysis)) + (deaths_total / 2)) %>%
        # Calculate hazard rates (absolute and percent)
        mutate(hazard_rate_total = deaths_total / !!sym(paste0("population_",year_of_analysis))) %>%
        mutate(hazard_rate_percent_total = hazard_rate_total * 100) %>%
        # Calculate survival probabilities (unrounded)
        mutate(prob_survival_total = 1 - (deaths_total / !!sym(paste0("population_",year_of_analysis,"_entry_total")))) %>%
        mutate(prob_survival_until_mid_year_total = 1 - (deaths_total / !!sym(paste0("population_",year_of_analysis,"_entry_total")) / 2)) %>%
        select(age,
               deaths_total,
               hazard_rate_total,
               hazard_rate_percent_total,
               prob_survival_total,
               prob_survival_until_mid_year_total,
               !!paste0("population_",year_of_analysis,"_entry_total"),
               !!paste0("population_",year_of_analysis),
               -c(population_mid_year_male, population_mid_year_female, deaths_male, deaths_female))

      pop_cutoff_total <- cbind(pop_male, pop_female) %>%
        select(age,
               population_mid_year_male,
               deaths_male,
               population_mid_year_female,
               deaths_female) %>%
        mutate(!!paste0("population_",year_of_analysis) := population_mid_year_male + population_mid_year_female,
               deaths_total = deaths_male + deaths_female) %>%
        mutate(!!paste0("population_",year_of_analysis,"_entry_total") := !!sym(paste0("population_",year_of_analysis)) + (deaths_total / 2)) %>%
        # Calculate hazard rates (absolute and percent)
        mutate(hazard_rate_total = deaths_total * MODIFICATION_FACTOR / !!sym(paste0("population_",year_of_analysis)) ) %>%
        mutate(hazard_rate_percent_total = hazard_rate_total * 100) %>%
        # Calculate modified survival probabilities
        ## Based on AirQ+ lifetable manual formula 4. on p 14: s_i = (2 - h_i) / (2 + h_i)
        ## In the population (without pollution effects) projections, a modified survival probability is calculated as a function of (modified) hazards
        mutate(prob_survival_total = (2 - hazard_rate_total) / (2 + hazard_rate_total)) %>%
        mutate(prob_survival_until_mid_year_total = 1 - ((1 - prob_survival_total) / 2))

      # NOTE: DO ASSIGNMENT BELOW WITH VARIABLE min_age ####
      pop_cutoff_total[1:20, "prob_survival_total"] <- pop_modelled_total[1:20, "prob_survival_total"]
      pop_cutoff_total[1:20, "prob_survival_until_mid_year_total"] <- pop_modelled_total[1:20, "prob_survival_until_mid_year_total"]

      pop_cutoff_total <- pop_cutoff_total %>%
        # Now re-calculate the "pop_2019_mid_year_total" using the modified survival rates
        mutate(!!paste0("population_",year_of_analysis) := !!sym(paste0("population_",year_of_analysis,"_entry_total")) * prob_survival_until_mid_year_total) %>%
        # Relocate and rename
        select(age,
               deaths_total,
               hazard_rate_total,
               hazard_rate_percent_total,
               prob_survival_total,
               prob_survival_until_mid_year_total,
               !!paste0("population_",year_of_analysis,"_entry_total"),
               !!paste0("population_",year_of_analysis),
               -c(population_mid_year_male, population_mid_year_female, deaths_male, deaths_female))

      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_modelled_total[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry_total")] <-
          pop_modelled_total[i:(length_period - 1), paste0("population_", years[i], "_entry_total")] * pop_modelled_total[i:(length_period - 1), "prob_survival_total"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_modelled_total[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_modelled_total[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry_total")] * pop_modelled_total[(i + 1):length_period, "prob_survival_until_mid_year_total"]

      }

      years <- c(year_of_analysis:(year_of_analysis + (nrow(pop_cutoff_total) - 1)))
      length_period <- length(years)

      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_cutoff_total[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry_total")] <-
          pop_cutoff_total[i:(length_period - 1), paste0("population_", years[i], "_entry_total")] * pop_cutoff_total[i:(length_period - 1), "prob_survival_total"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_cutoff_total[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_cutoff_total[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry_total")] * pop_cutoff_total[(i + 1):length_period, "prob_survival_until_mid_year_total"]
      }

      # MALE IMPACT ################################################################################
      yll_male <- pop_cutoff_male %>% select(age)
      yll_male$yll <- round(pop_cutoff_male[[paste0("population_",year_of_analysis)]]) - round(pop_modelled_male[[paste0("population_",year_of_analysis)]])
      premature_deaths <- pop_cutoff_male %>% select(age)
      premature_deaths$premature_deaths <- round((pop_cutoff_male[[paste0("population_",year_of_analysis)]] - pop_modelled_male[[paste0("population_",year_of_analysis)]]) * 2)

      pop_cutoff_mid_year_male <- pop_cutoff_male %>%
        select(-age,
               -deaths,
               -prob_survival,
               -prob_survival_until_mid_year,
               -contains("entry"))

      pop_modelled_mid_year_male <- pop_modelled_male %>%
        select(-age,
               -deaths,
               -prob_survival,
               -prob_survival_until_mid_year,
               -contains("entry"))

      pop_impact_nest_male <- pop_cutoff_mid_year_male - pop_modelled_mid_year_male
      pop_impact_nest_male$age <- pop_cutoff_male$age
      pop_impact_nest_male <- pop_impact_nest_male %>%
        relocate(age, .before = population_2019) %>%
        mutate(age_end = age + 1, .after = age)

      # FEMALE IMPACT ##############################################################################
      yll_female <- pop_cutoff_female %>% select(age)
      yll_female$yll <- round(pop_cutoff_female[[paste0("population_",year_of_analysis)]]) - round(pop_modelled_female[[paste0("population_",year_of_analysis)]])
      premature_deaths <- pop_cutoff_female %>% select(age)
      premature_deaths$premature_deaths <- round((pop_cutoff_female[[paste0("population_",year_of_analysis)]] - pop_modelled_female[[paste0("population_",year_of_analysis)]]) * 2)

      pop_cutoff_mid_year_female <- pop_cutoff_female %>%
        select(-age,
               -deaths,
               -prob_survival,
               -prob_survival_until_mid_year,
               -contains("entry"))

      pop_modelled_mid_year_female <- pop_modelled_female %>%
        select(-age,
               -deaths,
               -prob_survival,
               -prob_survival_until_mid_year,
               -contains("entry"))

      pop_impact_nest_female <- pop_cutoff_mid_year_female - pop_modelled_mid_year_female
      pop_impact_nest_female$age <- pop_cutoff_female$age
      pop_impact_nest_female <- pop_impact_nest_female %>%
        relocate(age, .before = population_2019) %>%
        mutate(age_end = age + 1, .after = age)

      # TOTAL IMPACT ###############################################################################
      yll_total <- pop_cutoff_total %>% select(age)
      yll_total$yll <- round(pop_cutoff_total[[paste0("population_",year_of_analysis)]]) - round(pop_modelled_total[[paste0("population_",year_of_analysis)]])
      premature_deaths_total <- pop_cutoff_total %>% select(age)
      premature_deaths_total$premature_deaths <- round((pop_cutoff_total[[paste0("population_",year_of_analysis)]] - pop_modelled_total[[paste0("population_",year_of_analysis)]]) * 2)

      pop_cutoff_mid_year_total <- pop_cutoff_total %>%
        select(-age,
               -deaths_total,
               -hazard_rate_total,
               -hazard_rate_percent_total,
               -prob_survival_total,
               -prob_survival_until_mid_year_total,
               -contains("entry"))

      pop_modelled_mid_year_total <- pop_modelled_total %>%
        select(-age,
               -deaths_total,
               -hazard_rate_total,
               -hazard_rate_percent_total,
               -prob_survival_total,
               -prob_survival_until_mid_year_total,
               -contains("entry"))

      pop_impact_nest_total <- pop_cutoff_mid_year_total - pop_modelled_mid_year_total
      pop_impact_nest_total$age <- pop_cutoff_total$age
      pop_impact_nest_total <- pop_impact_nest_total %>%
        relocate(age, .before = population_2019) %>%
        mutate(age_end = age + 1, .after = age)

      # COMPILE OUTPUT #############################################################################
      ## Save the sex-specific population projections to "lifetable_with_pop" the pop_impact_nest data frame in "input_with_risk_and_pop_fraction"
      lifetable_with_pop$pop_impact_nest <- list(pop_impact_nest_male, # NOTE: automate by paste0(selecting pop_impact_nest_, lifetable_with_pop[1, "sex"]
                                   pop_impact_nest_female)
      lifetable_with_pop$erf_ci <- "central"

      pop_impact <- input_with_risk_and_pop_fraction %>%
        filter(erf_ci == "central") %>%
        right_join(lifetable_with_pop, by = c("erf_ci", "geo_id_raw"))

      return(pop_impact)

    }


    # GeLuft #######################################################################################
    if ((outcome_metric %in% c("yll_airqplus") == FALSE)) {

    second_year <- year_of_analysis + 1

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

    if(outcome_metric %in% c("yll", "yld")){
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
            function(.x) {
              # length_period minus 1 because year_of_analysis+1 is already calculated
              for (i in 0:(length_period-1)){
                current_year <- period[i+1]
                col_current <- paste0("population_", current_year)
                col_next <- paste0("population_", current_year + 1)
                # avoiding the later introduction of NAs in the right top corner:
                .x[1:(length_period-i), col_next] <-
                   .x[1:(length_period-i), col_current] * (1 - .x$death_probability_total[(i+2):(length_period+1)])


                # Alternative code
                # Simpler but it does not provide the right result
                # .x[[col_next]] <-
                #  .x[[col_current]] * (1 - .x[["death_probability_total"]])
              }
              return(.x)
            } ))

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

