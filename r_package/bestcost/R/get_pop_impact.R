# Title and description

#' Get population impact over time
#'
#' Get population impact over time
#' @param lifetable_withPop_male \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table
#' @param paf \code{Data frame} with three rows (mean, lower bound and upper bound)
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
get_pop_impact <-
  function(lifetab_withPop, year_of_analysis, paf){

    rr_ci <- c("central", "lower", "upper") # variable used in code
    sex <- c("female","male")

    # Get popOvertime
    popOverTime <- list()

    for(s in c("female", "male")){
      for(v in rr_ci){
        popOverTime[[s]][[v]] <-
          bestcost::project_pop(
            lifetab_withPop = lifetab_withPop[[s]],
            year_of_analysis = year_of_analysis,
            paf = paf$paf[paf$rr_ci %in% v])
      }
    }


    # Get pop_impact

    pop_impact <- list()

    for(s in c("female", "male")){
      for(v in rr_ci){
        pop_impact[[s]][[v]] <-

          bestcost::move_rows_up(popOTime = popOverTime[[s]][[v]],
                                 year_of_analysis = year_of_analysis)
      }
    }

    output <-
      list(paf = paf,
           popOverTime = popOverTime,
           pop_impact = pop_impact)


    return(output)

  }
