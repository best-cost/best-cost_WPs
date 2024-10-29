#' Get DALY
#'
#' @description Get attributable disability-adjusted lived years applying a lifetable approach
#' @param outcome_metric \code{String} with the metric of the health outcome to be assessed. Options: "deaths", "yll" or "yld".
#' @param pop_impact \code{Data frame} with projected population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min \code{Numeric value}  with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max \code{Numeric value}  with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data
#' @param input_with_risk_and_pop_fraction \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @param corrected_discount_rate \code{Numeric value}  with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
#' @param duration \code{Numeric value} showing the duration (in years) of the morbidity health outcome
#' @return
#' This function returns a \code{List}
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @keywords internal
#'
#'
#'
get_daly <-
  function(outcome_metric,
           pop_impact,
           year_of_analysis,
           min_age = NULL,
           max_age = NULL,
           input_with_risk_and_pop_fraction,
           corrected_discount_rate = NULL){





    # Create list to iterate
    impact_yll_yld_raw <- list()


    for(o in c("yll", "yld" )){
      impact_yll_yld_raw[[o]] <-
        healthiar:::get_deaths_yll_yld(
          outcome_metric = "yll",
          pop_impact = pop_impact,
          year_of_analysis = year_of_analysis,
          min_age = min_age,
          max_age = max_age,
          corrected_discount_rate = corrected_discount_rate,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)
    }

    # Identify the common and identical columns (joining columns)
    # Remove impact (it can never be a joining column)
    joining_columns_yll_yld <-
      healthiar:::find_joining_columns(
        df1 = impact_yll_yld_raw[["yll"]][["main"]],
        df2 = impact_yll_yld_raw[["yld"]][["main"]],
        except = "impact")

    # New table containing yll and yld results
    impact_daly_main_raw <-
      # Join yll and yld tables
      dplyr::full_join(impact_yll_yld_raw[["yll"]][["main"]],
                       impact_yll_yld_raw[["yld"]][["main"]],
                       by = joining_columns_yll_yld,
                       suffix = c("_yll", "_yld")) |>
      # Sum yll and yld
      dplyr::mutate(impact = impact_yll + impact_yld)

    # Create compile main and detailed results
    impact_raw <- list(
      main = impact_daly_main_raw,
      detailed = list(impact_yll_yld_raw = impact_yll_yld_raw)
    )

    return(impact_raw)


  }
