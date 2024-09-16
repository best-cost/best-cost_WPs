#' include_social
#'
#' Consider socio-economic aspects in the results
#'
#' @param output \code{List} produced by \code{bestcost::attribute()} or \code{bestcost::compare()} as results
#' @param deprivation_index \code{Numeric value} showing the value of an indicator of economic wealth
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_social <- function(output,
                           deprivation_index) {

  # Define a function to add the monetized impacts (rounded and not rounded)
  add_monetized_impact <-
    function(df){
      df %>%
        dplyr::mutate(impact_deprivation_weighted =
                        as.numeric(impact) * as.numeric(deprivation_index),
                      .after = impact) %>%
        dplyr::mutate(impact_rounded_deprivation_weighted =
                        round(impact_deprivation_weighted),
                      .after = impact_rounded)
    }

  # Apply the function in main and detailed results
  output[["main"]] <-
    add_monetized_impact(output[["main"]])

  output[["detailed"]][["raw"]] <-
    add_monetized_impact(output[["detailed"]][["raw"]])

  return(output)

}
