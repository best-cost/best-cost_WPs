#' monetize
#'
#' Monetize health impacts (internal function)
#'
#' @param impact_raw
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
monetize <- function(output) {

  output[["main"]] <- output[["main"]] %>%
    dplyr::mutate(impact_monetized = as.numeric(valuation) * as.numeric(impact),
                  .after = impact) %>%
    dplyr::mutate(impact_monetized_rounded = round(impact_monetized),
                  .after = impact_rounded)

  return(output)

}
