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


    if (outcome_metric %in% c("yll_airqplus")) {

      pop_male <- lifetable_with_pop %>%
        filter(sex == "male") %>%
        select(lifetable_with_pop_nest) %>%
        unnest(cols = c(lifetable_with_pop_nest)) %>%
        select(population_mid_year_male = population,
               deaths_male = deaths)

      pop_female <- lifetable_with_pop %>%
        filter(sex == "female") %>%
        select(lifetable_with_pop_nest) %>%
        unnest(cols = c(lifetable_with_pop_nest)) %>%
        select(age,
               population_mid_year_female = population,
               deaths_female = deaths)

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

      mod_factor <- exp(0.0111541374732907 * (5 - 8.85)) # Based on AirQ+ lifetable manual formula 7 on p 17)
      # Formula 7: RR(x_1 - x_0) = exp( beta * (x_1 - x_0) )

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
        mutate(hazard_rate_total = deaths_total * mod_factor / !!sym(paste0("population_",year_of_analysis)) ) %>%
        mutate(hazard_rate_percent_total = hazard_rate_total * 100) %>%
        # Calculate modified survival probabilities ####
        ## Based on AirQ+ lifetable manual formula 4. on p 14: s_i = (2 - h_i) / (2 + h_i)
        ## In the population (without pollution effects) projections, a modified survival probability is calculated as a function of (modified) hazards
        mutate(prob_survival_total = (2 - hazard_rate_total) / (2 + hazard_rate_total)) %>%
        mutate(prob_survival_until_mid_year_total = 1 - ((1 - prob_survival_total) / 2))

      # NOTE: DO ASSIGNMENT BELOW WITH VARIABLE min_age ####
      pop_cutoff_total[1:20, "prob_survival_total"] <- pop_modelled_total[1:20, "prob_survival_total"]
      pop_cutoff_total[1:20, "prob_survival_until_mid_year_total"] <- pop_modelled_total[1:20, "prob_survival_until_mid_year_total"]

      pop_cutoff_total <- pop_cutoff_total %>%
        # Now re-calculate the "pop_2019_mid_year_total" using the modified survival rates ####
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

      years <- c(year_of_analysis:(year_of_analysis + (nrow(pop_modelled_total) - 1)))
      length_period <- length(years)

      for (i in 1:(length_period - 1)) { # starts with 1; ends with 99

        # Determine year i+1 entry population using year i entry population
        pop_modelled_total[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry_total")] <-
          pop_modelled_total[i:(length_period - 1), paste0("population_", years[i], "_entry_total")] * pop_modelled_total[i:(length_period - 1), "prob_survival_total"]

        # Determine year i+1 mid-year population using year i+1 entry population
        pop_modelled_total[(i + 1):length_period, paste0("population_", years[i] + 1)] <-
          pop_modelled_total[(i + 1):length_period, paste0("population_", years[i] + 1, "_entry_total")] * pop_modelled_total[(i + 1):length_period, "prob_survival_until_mid_year_total"]

        # Shift column up

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

      yll <- pop_cutoff_total %>% select(age)
      yll$yll <- round(pop_cutoff_total[[paste0("population_",year_of_analysis)]]) - round(pop_modelled_total[[paste0("population_",year_of_analysis)]])
      premature_deaths <- pop_cutoff_total %>% select(age)
      premature_deaths$premature_deaths <- round((pop_cutoff_total[[paste0("population_",year_of_analysis)]] - pop_modelled_total[[paste0("population_",year_of_analysis)]]) * 2)

      # Save the two life tables, the yll and the premature_deaths data frame in "input_with_risk_and_pop_fraction"
      pop_impact <- input_with_risk_and_pop_fraction %>%
        dplyr::mutate(pop_modelled_total_nest = list(pop_modelled_total),
                      pop_cutoff_total_nest = list(pop_cutoff_total),
                      yll_nest = list(yll),
                      premature_deaths_nest = list(premature_deaths))

      return(pop_impact)

    }



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

