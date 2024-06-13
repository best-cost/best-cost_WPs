# Title and description

#' Get population impact over time
#'
#' Get population impact over time
#' @param lifetable_withPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
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
#' @export
get_pop_impact_copy <-
  function(lifetab_withPop, year_of_analysis, pop_fraction, outcome_metric){

    lifetable_withPop <- lifetab_withPop # To be done: change function argument name to "lifetable_withPop
    # popOverTime <- list()
    pop_impact <- list()

    lifetable_withPop_backup <- lifetable_withPop

    for(s in names(lifetable_withPop)){
      lifetable_withPop <- lifetable_withPop_backup
      # print(s)
      for(v in pop_fraction$erf_ci){
        # print(v)

        lifetable_withPop = lifetable_withPop_backup[[s]]
        paf = pop_fraction$paf[pop_fraction$erf_ci %in% v]


        #### project_pop code #################################################################################

        # Add the first year of the life table to the column name of population
        lifetable_withPop <- lifetable_withPop %>%
          dplyr::rename(!!paste0("population_", year_of_analysis) := population)

        ##### project_pop_withExp ##############################################################################
        second_year <- year_of_analysis + 1

        lifetable_withPop <-
          lifetable_withPop %>%
          # Calculate the population the second year (first column after first year) considering the health effect of air pollution
          dplyr::mutate("population_{second_year}" := dplyr::lag(!!as.symbol(paste0("population_",
                                                                                    year_of_analysis))) * dplyr::lag(death_probability_natural) * paf) %>%
          # Move column up one row
          dplyr::mutate("population_{second_year}" := lead(!!as.symbol(paste0("population_",
                                                                              second_year))))

        #### project_pop code ##############################################################################

        if(outcome_metric %in% c("yll", "yld")){
          # Now calculate population over time (for the rest of years)
          # without considering air pollution

          lifetable_withPop = lifetable_withPop
          year_of_analysis = year_of_analysis

          ##### project_pop_noExp #######################################################################
          lifetable_withPop <-
            lifetable_withPop

          years <- c( (year_of_analysis + 1) : ((year_of_analysis + nrow(lifetable_withPop) - 2)) )
          length_period <- length(years)

          for (i in 0:(length_period-1)){ # i in 0:97
            YEAR <- years[i+1]
            # print(YEAR+1)
            # print(i)
            lifetable_withPop[1:((length_period)-i), paste0("population_", YEAR+1)] <-
              lifetable_withPop[1:((length_period)-i), paste0("population_", YEAR)] * (1 - lifetable_withPop$death_probability_total[(i+2):(length_period+1)])
          }


        }

        #### project_pop ###################################################################################

        ### get_pop_impact #################################################################################
        # popOverTime[[s]][[paste0("erf_ci_",v)]] <- lifetable_withPop
        # pop_impact[[s]][[paste0("erf_ci_",v)]] <- popOverTime[[s]][[paste0("erf_ci_",v)]]# New code replacing the call of move_rows_up
        pop_impact[[s]][[paste0("erf_ci_",v)]] <- lifetable_withPop
      }
    }
    output <-
      list(paf = pop_fraction,
           # popOverTime = popOverTime,
           pop_impact = pop_impact)

    return(output)

  }
