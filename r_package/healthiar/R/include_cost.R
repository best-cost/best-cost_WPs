#' include_cost

#' @description Monetize health impacts
#'
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results.
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).
#' @param valuation \code{Numberic value} referring to unit value of a health impact
#' @param time_period \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @param valuation \code{Numeric value} showing the value of statistical life which will be used in the health impact monetization
#' @inheritParams attribute
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_cost <- function(output = NULL,
                         impact = NULL,
                         valuation,
                         corrected_discount_rate = NULL,
                         time_period = 1,
                         approach_discount = NULL) {


  # Identify the relevant columns for monetization that are in the output
  relevant_columns <-
    c("info", "geo_id_raw", "geo_id_aggregated",
      "impact", "valuation", "corrected_discount_rate",
      "cost_without_discount", "cost", "cost_rounded")

  if(!is.null(output) & is.null(impact)){
    # Duplicate output to work with costs
    cost_output <-
      output

    # Apply the function in main and detailed results
    cost_output[["cost_main"]] <-
      healthiar:::add_monetized_impact(df = output[["health_main"]],
                                       valuation = valuation,
                                       corrected_discount_rate = corrected_discount_rate,
                                       time_period = time_period,
                                       approach_discount = approach_discount) |>
      # Keep only relevant columns for monetization
      dplyr::select(
        # The columns containing "_ci" are the uncertainties that define the rows
        contains("_ci"),
        # Use any_of() instead of all_of() because depending on the calculation pathway
        # there might not be any of the relevant_columns
        any_of(relevant_columns))

    cost_output[["cost_detailed"]]<-
      healthiar:::add_monetized_impact(df = output[["health_detailed"]][["raw"]],
                                       valuation = valuation,
                                       corrected_discount_rate = corrected_discount_rate,
                                       time_period = time_period,
                                       approach_discount = approach_discount) |>
      # Keep only relevant columns for monetization
      dplyr::select(any_of(relevant_columns), contains("_ci"))
  }

  else if(!is.null(impact) & is.null(output)){
    # Apply the function in main and detailed results
    cost_output <-
      healthiar:::add_monetized_impact(df = data.frame(impact = impact),
                                       valuation = valuation,
                                       corrected_discount_rate = corrected_discount_rate,
                                       time_period = time_period,
                                       approach_discount = approach_discount)
  }




  return(cost_output)

}
