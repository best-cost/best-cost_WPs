#' include_cost

#' @description Monetize health impacts
#'
#' @param output \code{List} produced by \code{bestcost::attribute()} or \code{bestcost::compare()} as results
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
        dplyr::mutate(impact_monetized = as.numeric(valuation) * as.numeric(impact),
                      .after = impact) |>
        dplyr::mutate(impact_rounded_monetized = round(impact_monetized),
                      .after = impact_rounded)
    }

  # Apply the function in main and detailed results
  output[["main"]] <-
    add_monetized_impact(output[["main"]])

  output[["detailed"]][["raw"]] <-
    add_monetized_impact(output[["detailed"]][["raw"]])

  return(output)

}
