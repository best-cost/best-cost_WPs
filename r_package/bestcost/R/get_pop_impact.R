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
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal

get_pop_impact <-
  function(lifetable_with_pop, year_of_analysis, input_risk_pop_fraction, outcome_metric){

    pop_impact <- list()
    lifetable_with_pop_backup <- lifetable_with_pop

    for(s in names(lifetable_with_pop)){
      lifetable_with_pop <- lifetable_with_pop_backup
      for(v in input_risk_pop_fraction$erf_ci){

        lifetable_with_pop <- lifetable_with_pop_backup[[s]]
        pop_fraction <- input_risk_pop_fraction$pop_fraction[input_risk_pop_fraction$erf_ci %in% v]
        second_year <- year_of_analysis + 1

        lifetable_with_pop <- lifetable_with_pop %>%
          dplyr::rename(!!paste0("population_", year_of_analysis) := population) %>%
          # Calculate the population the second year (first column after first year) considering the health effect of air pollution
          dplyr::mutate("population_{second_year}" :=
                          dplyr::lag(!!as.symbol(paste0("population_",year_of_analysis))) * dplyr::lag(death_probability_natural) * pop_fraction) %>%
          # Move column up one row
          dplyr::mutate("population_{second_year}" := lead(!!as.symbol(paste0("population_", second_year))))

        if(outcome_metric %in% c("yll", "yld")){
          # Now calculate population over time for the rest of year starting with YOA without considering air pollution

          years <- c( (year_of_analysis + 1) : ((year_of_analysis + nrow(lifetable_with_pop) - 2)) )
          length_period <- length(years)

          for (i in 0:(length_period-1)){ # i in 0:97
            YEAR <- years[i+1]
            lifetable_with_pop[1:((length_period)-i), paste0("population_", YEAR+1)] <-
              lifetable_with_pop[1:((length_period)-i), paste0("population_", YEAR)] * (1 - lifetable_with_pop$death_probability_total[(i+2):(length_period+1)])
          }
        }
       pop_impact[[s]][[paste0("erf_ci_", v)]] <- lifetable_with_pop
      }
    }

    output <-
      list(pop_fraction = pop_fraction,
           pop_impact = pop_impact)

    return(output)

  }
