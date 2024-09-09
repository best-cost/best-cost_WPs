# Title and description

#' Determine years lived with disability (YLD) attributable to the incidence of a specific morbidity health outcome

#' Calculates the YLDs using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bounds of the morbidity impact based on the confidence interval of the exposure-response function. Assumption: cases happen at the start of the year.
#' @inheritParams attribute
#'
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval.
#' The YLDs are listed in the columns:
#' \itemize{
#'  \item yld
#'  \item yld_rounded
#'  }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Axel Luyten
#' @note Experimental function
#' @export
attribute_yld_from_prevalence <-
  function(exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           erf_increment,
           erf_shape,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           bhd_central, bhd_lower = NULL, bhd_upper = NULL,
           dw_central, dw_lower = NULL, dw_upper = NULL,
           valuation = NULL,
           info = NULL){


    output <-
      bestcost::attribute(
        health_metric = "yld_from_prevalence",
        approach_risk = "relative_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        pop_exp = NULL,
        cutoff = cutoff,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        erf_increment = erf_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_lower,
        first_age_pop = NULL, last_age_pop = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        year_of_analysis = NULL,
        min_age = NULL, max_age = NULL,
        corrected_discount_rate = NULL,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration = NULL,
        geo_id_raw = NULL , geo_id_aggregated = NULL,
        valuation = valuation,
        info = info)


    return(output)

  }
