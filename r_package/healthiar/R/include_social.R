#' include_social

#' @description Consider socio-economic aspects in the results
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param deprivation_weighting \code{Vector} with numeric values showing the deprivation index (indicator of economic wealth) of the fine geographical area (it should match with those used in \code{attribute} or \code{compare})
#' @inheritParams attribute
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_social <- function(output,
                           deprivation_index,
                           geo_id_raw,
                           approach = "multiplicative") {

  output_social <- list()

  if(approach == "multiplicative"){


  # Re-calculate output with the social aspects inside using output_raw
  output_social[["main"]] <-
    output[["detailed"]][["raw"]] |>

    dplyr::left_join(
      x =_,
      y = dplyr::tibble(geo_id_raw = geo_id_raw,
                    deprivation_index = deprivation_index),
      by = "geo_id_raw")|>

    dplyr::mutate(impact_social =
                    as.numeric(impact) * as.numeric(deprivation_index),
                  .after = impact) |>
    dplyr::mutate(impact_rounded_social =
                    round(impact_social),
                  .after = impact_rounded_social)


  # Based on the new output_raw that includes social aspects
  # Recalculate output
  output_social <- healthiar:::get_output(output_social)

  }


  return(output_social)

}
