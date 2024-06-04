#' Attributable health cases based on relative risk

#' @description Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @inheritParams attribute_deaths_lifetable_rr
#' @param bhd_central \code{Numeric value} showing the only or central estimate (if confidence interval) of the baseline health data (e.g. incidence of the health outcome in the population).
#' @param bhd_upper \code{Numeric value} showing the lower bound estimate (confidence interval) of the baseline health data.
#' @param bhd_lower \code{Numeric value} showing the upper bound estimate (confidence interval) of the baseline health data.
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
  function(exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           bhd_central, bhd_lower = NULL, bhd_upper = NULL,
           geo_id_raw = NULL, geo_id_aggregated = NULL,
           info = NULL){

    # Compile input data and calculate paf putting all into a data frame
    input <-
      bestcost::compile_input(
        exp_central = exp_central, exp_lower =  exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_c_central = erf_c_central, erf_c_lower = erf_c_lower, erf_c_upper = erf_c_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        geo_id_raw = geo_id_raw,
        geo_id_aggregated = geo_id_aggregated,
        info = info,
        method = "singlebhd_rr")

    # Get PAF and added to the input data frame
    calculation <-
      bestcost::get_risk_and_paf(input = input) %>%
      # Build the result table adding the paf to the input_risk_paf table
        dplyr::mutate(impact = paf * bhd,
                      impact_rounded = round(impact, 0)) %>%
        # Order columns
        dplyr::select(exp_ci, bhd_ci, erf_ci,
                      paf, impact, impact_rounded,
                      everything())

    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is defined
    if(!is.null(geo_id_aggregated)){
      calculation <-
        calculation %>%
        # Group by higher geo level
        dplyr::group_by(geo_id_aggregated, exp_ci, bhd_ci, erf_ci) %>%
        dplyr::summarise(impact = sum(impact),
                         impact_rounded = round(impact),
                         .groups = "drop")%>%
        dplyr::bind_rows(calculation, .)
    }



    # Filter for total list element
    # Keep only exp_ci = central and bhd_ci=central
    calculation_exp_bhd_central <-
      calculation %>%
      dplyr::filter(exp_ci %in% "central", bhd_ci %in% "central")


   output <- list(main = calculation_exp_bhd_central,
                  detailed = calculation)

    return(output)
  }

