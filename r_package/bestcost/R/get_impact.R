#' Attributable health cases based on relative risk

#' @description Calculates the health impacts for each uncertainty and geo area.
#' @param input \code{Data frame} containing all input data.
#' @param method \code{String} showing the calculation method: "relative_risk" or "absolute_risk".
#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
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
#' @inherit attribute_deaths_lifetable_rr note
#' @export
get_impact <-
  function(input,
           method){

    if(method == "relative_risk"){
      # Get PAF and added to the input data frame
      output_raw <-
        bestcost::get_risk_and_paf(input = input) %>%
        # Build the result table adding the paf to the input_risk_paf table
        dplyr::mutate(impact = paf * bhd,
                      impact_rounded = round(impact, 0)) %>%
        # Order columns
        dplyr::select(exp_ci, bhd_ci, erf_ci,
                      paf, impact, impact_rounded,
                      everything())
    }

    if(method == "absolute_risk"){

      # Calculate absolute risk for each exposure category ####
      output_raw <-
        input %>%
        dplyr::mutate(
          absolute_risk_as_percent = bestcost::get_risk(exp = exp, erf_c = erf_c, erf_full = TRUE) ,
          impact = absolute_risk_as_percent/100 * pop_exp,
          impact_rounded = round(impact, 0))


    }

    return(output_raw)

  }





