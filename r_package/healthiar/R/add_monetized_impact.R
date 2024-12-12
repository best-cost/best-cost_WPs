#' add_monetized_impact

#' @description Function to calculate and add the cost of the health impacts
#'
#' @param df \code{Data frame} including the column "impact" (health impact)
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).
#' @param valuation \code{Numberic value} referring to unit value of a health impact
#' @param time_period \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @param valuation \code{Numeric value} showing the value of statistical life which will be used in the health impact monetization
#' @inheritParams include_cost
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
add_monetized_impact  <- function(df,
                                  valuation,
                                  corrected_discount_rate,
                                  time_period,
                                  discount_shape) {

  df_with_input <-
    df |>
    # Add columns for input data in the table
    # Use {{}} to clarify the it refers to the argument and not to the column
    # with the same name
    dplyr::mutate(corrected_discount_rate = {{corrected_discount_rate}},
                  time_period = {{time_period}},
                  discount_shape = {{discount_shape}})

  df_with_discount_factor <-
    dplyr::cross_join(x = df_with_input,
                      y = dplyr::tibble(year = 1:{{time_period}})) |>
    # rowwise() because time_period becomes a vecto below 1:time_period
    # otherwise vectors from columns and vectors from time_period cannot be digested
    # better step by step
    dplyr::rowwise() |>
    # Calculate discount factor
    # If any arguments "corrected_discount_rate" and "discount_shape" are NULL,
    # no discount (i.e. discount_factor=1)
    dplyr::mutate(
      discount_factor =
        healthiar::get_discount_factor(
          corrected_discount_rate = corrected_discount_rate,
          time_period = year,
          discount_shape = discount_shape))

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
      # Calculate impact after discounting
      impact_before_discount = impact,
      impact_after_discount = sum(impact/time_period * discount_factor_overtime),
      impact = impact_after_discount,
      # Add column for valuation
      valuation = valuation,
      # Calculate monetized impact
      # Sum across the different discount factors
      # (one for each year of the period)
      # The default value 1 for time period enables that the calculation below
      # is not affected if no discount is demanded by the user
      cost_before_discount = impact_before_discount * valuation,
      cost_after_discount = impact_after_discount * valuation,
      cost = cost_after_discount,
      .after = impact) |>

    # Round costs
    dplyr::mutate(
      cost_before_discount_rounded = round(cost_before_discount),
      cost_after_discount_rounded = round(cost_after_discount),
      cost_rounded = round(cost),
      .after = cost)

  return(df_with_cost)

}

