# Title and description

#' Get population difference
#'
#' Get population difference
#' @param popOverTime_AP \code{Data frame}
#' @param popOverTime_noAP \code{Data frame}
#' #' This function returns a \code{data.frame} containing the difference in population between the reference and the counterfactual scenarios.
#' @import dplyr
#' @import tibble
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#'
get_pop_diff <- function(popOverTime_AP, popOverTime_noAP)
  {
  output <-
    tibble::column_to_rownames(  # Add age to rowname
    dplyr::select(popOverTime_AP, age, contains("population_")),
    "age") -
    tibble::column_to_rownames(
      dplyr::select(popOverTime_noAP, age, contains("population_")),
      "age")
  popOverTime_diff <-
    tibble::rownames_to_column(popOverTime_diff, "age")
  return(output)
}
