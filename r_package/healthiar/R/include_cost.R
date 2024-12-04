#' include_cost

#' @description Monetize health impacts
#'
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param valuation \code{Numeric value} showing the value of statistical life which will be used in the health impact monetization
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_cost <- function(output,
                         valuation) {

  # Define a function to add the monetized impacts (rounded and not rounded)
  add_monetized_impact <-
    function(df){
      df |>
        dplyr::mutate(valuation = valuation,
                      impact_monetized = as.numeric(valuation) * as.numeric(impact),
                      .after = impact) |>
        dplyr::mutate(impact_rounded_monetized = round(impact_monetized),
                      .after = impact_rounded)
    }

  # Identify the relevant columns for monetization that are in the output
  relevant_columns <-
    c("info", "geo_id_raw", "geo_id_aggregated",
      "impact", "valuation", "corrected_discount_rate",
      "impact_monetized", "impact_rounded_monetized")


  # Apply the function in main and detailed results
  output[["cost_main"]] <-
    add_monetized_impact(output[["health_main"]]) |>
    # Keep only relevant columns for monetization
    dplyr::select(
      # The columns containing "_ci" are the uncertainties that define the rows
      contains("_ci"),
      # Use any_of() instead of all_of() because depending on the calculation pathway
      # there might not be any of the relevant_columns
      any_of(relevant_columns))

  output[["cost_detailed"]]<-
    add_monetized_impact(output[["health_detailed"]][["raw"]]) |>
    # Keep only relevant columns for monetization
    dplyr::select(any_of(relevant_columns), contains("_ci"))

  return(output)

}
