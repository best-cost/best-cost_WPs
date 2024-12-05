#' include_cost

#' @description Monetize health impacts
#'
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param time_period \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @param valuation \code{Numeric value} showing the value of statistical life which will be used in the health impact monetization
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_cost <- function(output,
                         valuation,
                         corrected_discount_rate = NULL,
                         time_period = NULL,
                         approach_discount = NULL) {

  # Define a function to add the monetized impacts (rounded and not rounded)
  add_monetized_impact <-
    function(df){
      df_with_cost <-
        df |>
        # Add columns for input data in the table
        dplyr::mutate(corrected_discount_rate = {{corrected_discount_rate}},
                      time_period = {{time_period}},
                      approach_discount = {{approach_discount}}) |>
        # Calculate discount factor
        # If any of the columns "time_period" and "approach_discount" are not present
        # it is because their argument are NULL
        # So no discount (i.e. discount_factor=1)
        #dplyr::rowwise() |>
        dplyr::mutate(
          discount_factor =
            ifelse(
              any(is.null({{corrected_discount_rate}}),
                  is.null({{time_period}}),
                  is.null({{approach_discount}})),
              healthiar::get_discount_factor(
                corrected_discount_rate = corrected_discount_rate,
                time_period = time_period,
                approach_discount = approach_discount),
              1),
          # Add column for valuation
          valuation = valuation,
          # Calculate monetized impact
          impact_monetized = impact * discount_factor * valuation,
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
