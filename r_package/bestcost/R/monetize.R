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
monetize <- function(impact_raw) {

  impact_raw[["main"]] <- impact_raw[["main"]] %>%
    mutate(impact_monetized = valuation * impact, .after = impact) %>%
    mutate(impact_monetized_rounded = round(impact_monetized), .after = impact_rounded)

  return(impact_raw)

}
