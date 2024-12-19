#' Get input data and PIF

#' @description Calculates the population impact fraction (PIF) based on the input data and puts the results in additional columns joined to the input data frame.
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
#' @keywords internal

get_risk_and_pif <-
  function(input){

    # Calculate health impact attributable to exposure ####
    input_and_risk <-
      input |>
      dplyr::rowwise() |>
      dplyr::mutate(
        rr_conc_1 =
          healthiar::get_risk(rr = rr,
                             exp = exp_1,
                             cutoff_central = cutoff_central,
                             erf_increment = erf_increment,
                             erf_shape = unique(erf_shape)
                             ),
        rr_conc_2 =
          healthiar::get_risk(rr = rr,
                             exp = exp_2,
                             cutoff_central = cutoff_central,
                             erf_increment = erf_increment,
                             erf_shape = unique(erf_shape)
          )) |>
      dplyr::ungroup()

    # Calculate population impact fraction (PIF) ####
    input_risk_pif <-
      input_and_risk |>
      # Group by exp in case that there are different exposure categories
      dplyr::group_by(erf_ci) |>
      dplyr::summarize(pif = healthiar::get_pif(rr_conc_1 = rr_conc_1,
                                               rr_conc_2 = rr_conc_2,
                                               prop_pop_exp_1 = prop_pop_exp_1,
                                               prop_pop_exp_2 = prop_pop_exp_2))|>
      # Join the input table with pif values
      dplyr::left_join(., input_and_risk,
                       by = "erf_ci")

    # Data wrangling ####
    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    if(length(unique(input_and_risk$exp_1))>1){
      input_risk_pif <-
        input_risk_pif |>
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_1_mean = mean(exp_1),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp_1 = paste(exp_1, collapse = ", "),
          prop_pop_exp_1 = paste(prop_pop_exp_1, collapse = ", "),
          rr_conc_1 = paste(rr_conc_1, collapse = ", ")) |>
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    if(length(unique(input_and_risk$exp_2))>1){
      input_risk_pif <-
        input_risk_pif |>
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_2_mean = mean(exp_2),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp_2 = paste(exp_2, collapse = ", "),
          prop_pop_exp_2 = paste(prop_pop_exp_2, collapse = ", "),
          rr_conc_2 = paste(rr_conc_2, collapse = ", ")) |>
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    return(input_risk_pif)
  }
