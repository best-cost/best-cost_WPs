#' Get DALY
#'
#' @description Get attributable disability-adjusted lived years applying a lifetable approach
#' @inheritParams attribute
#' @param outcome_metric \code{String} with the metric of the health outcome to be assessed. Options: "deaths", "yll" or "yld".
#' @return
#' This function returns a \code{List}
#' @import dplyr
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
           time_horizon,
           # min_age = NULL,
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
          # min_age = min_age,
          max_age = max_age,
          time_horizon = time_horizon,
          corrected_discount_rate = corrected_discount_rate,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)
    }

    # Identify the common and identical columns (joining columns)
    # Remove impact (it can never be a joining column)
    joining_columns_yll_yld <-
      healthiar:::find_joining_columns(
        df1 = impact_yll_yld_raw[["yll"]],
        df2 = impact_yll_yld_raw[["yld"]],
        except = "impact")

    # New table containing yll and yld results
    impact_raw <-
      # Join yll and yld tables
      dplyr::full_join(impact_yll_yld_raw[["yll"]],
                       impact_yll_yld_raw[["yld"]],
                       by = joining_columns_yll_yld,
                       suffix = c("_yll", "_yld")) |>
      # Sum yll and yld
      dplyr::mutate(impact = impact_yll + impact_yld)

   return(impact_raw)


  }
