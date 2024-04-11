#' Attributable health cases based on relative risk

#' @description Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @inheritParams attribute_deaths_lifetable_rr
#' @param bhd \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population).
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
attribute_health_singlebhd_rr <-
  function(exp,
           prop_pop_exp = 1,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           erf_c = NULL,
           bhd,
           info = NULL){

    # Compile input data and calculate paf putting all into a data frame
    input <-
      bestcost::compile_input(
        exp = exp,
        prop_pop_exp = prop_pop_exp,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        bhd = bhd,
        min_age = NULL,
        max_age = NULL,
        info = info,
        method = "singlebhd_rr",
        disability_weight = NULL,
        duration = NULL)

    # Get PAF and added to the input data frame
    calculation <-
      bestcost::get_risk_and_paf(input = input) %>%
      # Build the result table adding the paf to the input_risk_paf table
        dplyr::mutate(impact = paf * bhd,
                      impact_rounded = round(impact, 0)) %>%
        # Order columns
        dplyr::select(exp, cutoff, bhd,
                      rr, rr_conc, rr_increment, rr_ci, erf_shape,
                      paf, impact, impact_rounded,
                      everything())

   output <- list(total = calculation)

    return(output)
  }

