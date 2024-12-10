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

  # Define a function to add the monetized impacts (rounded and not rounded)
  add_monetized_impact <-
    function(df){
      df_with_input <-
        df |>
        # Add columns for input data in the table
        dplyr::mutate(corrected_discount_rate = {{corrected_discount_rate}},
                      time_period = {{time_period}},
                      approach_discount = {{approach_discount}})

      df_with_discount_factor <-
        dplyr::cross_join(x = df_with_input,
                          y = dplyr::tibble(year = 1:{{time_period}})) |>
        # rowwise() because time_period becomes a vecto below 1:time_period
        # otherwise vectors from columns and vectors from time_period cannot be digested
        # better step by step
        dplyr::rowwise() |>
        # Calculate discount factor
        # If any arguments "corrected_discount_rate" and "approach_discount" are NULL,
        # no discount (i.e. discount_factor=1)
        dplyr::mutate(
          discount_factor =
            healthiar::get_discount_factor(
              corrected_discount_rate = corrected_discount_rate,
              time_period = year,
              approach_discount = approach_discount))

      sum_of_discount_factors <-
        df_with_discount_factor |>
          # Sum across years of time period
          # The grouping id here is the impact
          dplyr::group_by(impact) |>
          dplyr::summarise(discount_factor_overtime = sum(discount_factor),
                           .groups = "keep")

      df_with_cost <-
        # Join the sum of discount factors with the original data
        dplyr::left_join(
          df_with_input,
          sum_of_discount_factors,
          by = "impact" ) |>
        # Add columns
        dplyr::mutate(
        # Add column for valuation
        valuation = valuation,
        # Calculate monetized impact
        # Sum across the different discount factors
        # (one for each year of the period)
        # The default value 1 for time period enables that the calculation below
        # is not affected if no discount is demanded by the user
        cost_without_discount = impact * valuation,
        cost = sum(impact/time_period * discount_factor_overtime) * valuation,
        .after = impact) |>

        # Round costs
        dplyr::mutate(cost_rounded = round(cost),
                      .after = cost)
    }

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
      add_monetized_impact(output[["health_main"]]) |>
      # Keep only relevant columns for monetization
      dplyr::select(
        # The columns containing "_ci" are the uncertainties that define the rows
        contains("_ci"),
        # Use any_of() instead of all_of() because depending on the calculation pathway
        # there might not be any of the relevant_columns
        any_of(relevant_columns))

    cost_output[["cost_detailed"]]<-
      add_monetized_impact(output[["health_detailed"]][["raw"]]) |>
      # Keep only relevant columns for monetization
      dplyr::select(any_of(relevant_columns), contains("_ci"))
  }

  else if(!is.null(impact) & is.null(output)){
    # Apply the function in main and detailed results
    cost_output <-
      add_monetized_impact(data.frame(impact = impact))
  }




  return(cost_output)

}
