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
#' @keywords internal

get_pop_impact <-
  function(lifetab_withPop, year_of_analysis, pop_fraction, outcome_metric){


    # Get popOvertime
    popOverTime <- list()

    for(s in names(lifetab_withPop)){
      for(v in pop_fraction$erf_ci){
        popOverTime[[s]][[paste0("erf_ci_",v)]] <-
          bestcost:::project_pop(
            lifetab_withPop = lifetab_withPop[[s]],
            year_of_analysis = year_of_analysis,
            paf = pop_fraction$paf[pop_fraction$erf_ci %in% v],
            outcome_metric = outcome_metric)
      }
    }




    # Get pop_impact

    pop_impact <- list()

    for(s in names(lifetab_withPop)){
      for(v in pop_fraction$erf_ci){
        pop_impact[[s]][[paste0("erf_ci_",v)]] <-

          bestcost:::move_rows_up(popOTime = popOverTime[[s]][[paste0("erf_ci_",v)]],
                                 year_of_analysis = year_of_analysis)
      }
    }


    output <-
      list(paf = pop_fraction,
           popOverTime = popOverTime,
           pop_impact = pop_impact)


    return(output)

  }
