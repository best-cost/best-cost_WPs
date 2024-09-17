#' include_social
#'
#' Consider socio-economic aspects in the results
#' @param output \code{List} produced by \code{bestcost::attribute()} or \code{bestcost::compare()} as results
#' @param deprivation_weighting \code{Vector} with numeric values showing the deprivation index (indicator of economic wealth) of the fine geographical area (it should match with those used in \code{attribute} or \code{compare})
#' @inheritParams attribute
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_social <- function(output,
                           deprivation_weight,
                           geo_id_raw
                           #TODO Add method (if multiple)
                           ) {

  output_social <- list()

  # Re-calculate output with the social aspects inside using output_raw

  output_social[["main"]] <-
    output[["detailed"]][["raw"]] %>%
    dplyr::left_join(
      .,
      dplyr::tibble(geo_id_raw = geo_id_raw,
                    deprivation_weight = deprivation_weight),
      by = "geo_id_raw")%>%
    dplyr::mutate(impact_deprivation_weighted =
                    as.numeric(impact) * as.numeric(deprivation_weight),
                  .after = impact) %>%
    dplyr::mutate(impact_rounded_deprivation_weighted =
                    round(impact_deprivation_weighted),
                  .after = impact_rounded)


  # Based on the new output_raw that includes social aspects
  # Recalculate output
  output_social <- bestcost:::get_output(output_social)

  # Calculate the weighting


  return(output_social)

}
