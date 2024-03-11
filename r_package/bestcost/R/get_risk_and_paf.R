# Title and description

#' Get input data and PAF

#' Calculates the population attributable fraction (PAF) based on the input data and puts the results in additional columns joined to the input data frame.
#' @param input \code{Data frame} with the input data
#' @return
#' This function returns a \code{data.frame} with the input data adding a column for the population attributable fraction
#' Moreover, the data frame includes columns such as:
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
get_risk_and_paf <-
  function(input){

    # Calculate health impact attributable to exposure ####
    input_and_risk <-
      input %>%
      dplyr::mutate(
        rr_forPaf =
          bestcost::get_risk(rr = rr,
                             exp = exp,
                             cutoff = cutoff,
                             rr_increment = rr_increment,
                             erf_shape = unique(erf_shape)
                             ))

    # Calculate population attributable fraction (PAF) ####
    input_risk_paf <-
      input_and_risk %>%
      # Group by exp in case that there are different exposure categories
      dplyr::group_by(rr) %>%
      dplyr::summarize(paf = bestcost::get_paf(rr_conc = rr_forPaf,
                                               prop_pop_exp = prop_pop_exp))%>%
      # Join the input table with paf values
      dplyr::left_join(., input_and_risk,
                       by = "rr")

    # Data wrangling ####
    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    if(length(unique(input_and_risk$exp))>1){
      input_risk_paf <-
        input_risk_paf %>%
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_mean = mean(exp),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp = paste(exp, collapse = ", "),
          prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
          rr_forPaf = paste(rr_forPaf, collapse = ", ")) %>%
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    return(input_risk_paf)
  }
