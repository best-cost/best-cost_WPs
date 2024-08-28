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
#' @keywords internal

get_risk_and_pop_fraction <-
  function(input,
           pop_fraction_type){

    # Calculate health impact attributable to exposure ####
    input_with_risk_and_pop_fraction <-
      input %>%
      # Add input
      dplyr::mutate(pop_fraction_type = pop_fraction_type) %>%
      # For the calculations below rowwise approach is needed
      dplyr::rowwise(.) %>%
      # Obtain the relative risk for the relevant concentration
      {if({{pop_fraction_type}} == "paf")
        dplyr::mutate(.,
                      rr_conc =
                        bestcost::get_risk(rr = rr,
                                           exp = exp,
                                           cutoff = cutoff,
                                           erf_increment = erf_increment,
                                           erf_shape = erf_shape,
                                           erf_c = erf_c))
        else
          dplyr::mutate(.,
                        rr_conc_1 =
                          bestcost::get_risk(rr = rr,
                                             exp = exp_1,
                                             cutoff = cutoff,
                                             erf_increment = erf_increment,
                                             erf_shape = erf_shape,
                                             erf_c = erf_c),
                        rr_conc_2 =
                          bestcost::get_risk(rr = rr,
                                             exp = exp_2,
                                             cutoff = cutoff,
                                             erf_increment = erf_increment,
                                             erf_shape = erf_shape,
                                             erf_c = erf_c))}%>%



      # Calculate population (attributable or impact) fraction (PAF or PIF) ####
      # Group by exp in case that there are different exposure categories
      group_by(geo_id_raw, exp_ci, erf_ci) %>%
      # Alternative coding if one of the grouping variables is NULL
      # dplyr::group_by(across(all_of(intersect(c("geo_id_raw", "exp_ci", "erf_ci"), names(input))))) %>%
      # Alternative coding with if statement within group_by
      # Using if statement as below because otherwise (e.g. with if_else or case_when)
      # the FALSE condition is evaluate and results in an error because the names do not match
      # dplyr::group_by(if("geo_id_raw" %in% names(input)){geo_id_raw}, exp_ci, erf_ci)%>%

      {if({{pop_fraction_type}} == "paf")
        dplyr::mutate(.,
                      pop_fraction =
                        bestcost::get_pop_fraction(rr_conc_1 = rr_conc,
                                                   rr_conc_2 = 1,
                                                   prop_pop_exp_1 = prop_pop_exp,
                                                   prop_pop_exp_2 = prop_pop_exp))
        else
          dplyr::mutate(.,
                        pop_fraction =
                          bestcost::get_pop_fraction(rr_conc_1 = rr_conc_1,
                                                     rr_conc_2 = rr_conc_2,
                                                     prop_pop_exp_1 = prop_pop_exp_1,
                                                     prop_pop_exp_2 = prop_pop_exp_2))}%>%

      dplyr::ungroup(.)

    # Data wrangling ####
    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    if(unique(input$exposure_type) == "exposure_distribution"){
      input_with_risk_and_pop_fraction <-
        input_with_risk_and_pop_fraction %>%
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_mean = mean(exp),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp = paste(exp, collapse = ", "),
          exposure_dimension = paste("1:", max(exposure_dimension)),
          prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
          rr_conc = paste(rr_conc, collapse = ", ")) %>%
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    return(input_with_risk_and_pop_fraction)
  }
