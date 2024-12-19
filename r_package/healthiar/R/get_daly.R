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
  function(
    pop_impact,
    input_with_risk_and_pop_fraction
    ) {

    ## Create list to iterate
    impact_yll_yld_raw <- list()

    for(o in c("yll", "yld" )){

      impact_yll_yld_raw[[o]] <-
        healthiar:::get_deaths_yll_yld(
          pop_impact = pop_impact,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction |> mutate(health_metric = paste0(o, "_from_lifetable")))
    }

    ## Identify the common and identical columns (joining columns)
    ## Remove impact (it can never be a joining column)
    joining_columns_yll_yld <-
      healthiar:::find_joining_columns(
        df1 = impact_yll_yld_raw[["yll"]],
        df2 = impact_yll_yld_raw[["yld"]],
        except = "impact")

    ## New table containing yll and yld results
    impact_raw <-
      # Join yll and yld tables
      dplyr::full_join(impact_yll_yld_raw[["yll"]],
                       impact_yll_yld_raw[["yld"]],
                       by = joining_columns_yll_yld,
                       suffix = c("_yll", "_yld")) |>
      ## Sum yll and yld
      dplyr::mutate(impact = impact_yll + impact_yld)

   return(impact_raw)


  }
