#' include_social

#' @description Consider socio-economic aspects in the results
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param deprivation_index \code{Vector} with numeric values showing the deprivation index (indicator of economic wealth) of the fine geographical area (it should match with those used in \code{attribute} or \code{compare})
#' @param population code{Vector} with numeric values referring to the population in the geographical unit
#' @param approach code{String} referring the approach to include the social aspects. To choose between "decile" and "multiplicative"
#' @inheritParams attribute
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_social <- function(output,
                           geo_id_raw,
                           deprivation_index,
                           population,
                           approach = "multiplicative") {



  output_social <-
    # Add deprivation index to detailed output
    output[["detailed"]][["raw"]] |>
    dplyr::left_join(
      x = _,
      y = dplyr::tibble(geo_id_raw = geo_id_raw,
                        deprivation_index = deprivation_index,
                        population = population),
      by = "geo_id_raw")


    if(approach == "multiplicative"){
      # Re-calculate output with the social aspects inside using output_raw

    output_social[["main"]] <-
      output_social[["main"]] |>
      dplyr::mutate(impact_social =
                      as.numeric(impact) * as.numeric(deprivation_index),
                    .after = impact) |>
      dplyr::mutate(impact_rounded_social =
                      round(impact_social),
                    .after = impact_rounded_social)

  # Based on the new output_raw that includes social aspects
  # Recalculate output
    output_social <- healthiar:::get_output(list(main = output_social))

    }


  if(approach == "decile"){

    ## exposure and attributable burden per deprivation decile

    social_per_decile <-
      output_social |>
      # Remove NAs
      filter(!is.na(deprivation_index)) |>
      # Add ranking of deprivation index and deciles
      dplyr::mutate(
        ranking = min_rank(desc(deprivation_index)),
        decile = ntile(deprivation_index, n = 10))

    output_social_summary <-
      social_per_decile |>
      # Group by geo_id to ensure that you get one result per geo_id
      # keeping uncertainties
      dplyr::group_by(geo_id_raw) |>
      dplyr::summarise(
        exposure_mean = tapply(exp_central, decile, mean),
        mort_attr = tapply(impact, decile, sum) * 1e5 / tapply(population, decile, sum),
        mort_total = tapply(bhd_central, decile, sum) * 1e5 /  tapply(population, decile, sum))



    ## inequalities

    social_results <-
      tidyr::tibble(
        exp_abs_diff = social_per_decile[["exposure_mean"]][1] - social_per_decile[["exposure_mean"]][10], ## absolute diff,
        exp_rel_diff = 100 * ( (social_per_decile[["exposure_mean"]][1] / social_per_decile[["exposure_mean"]][10]) - 1 ), ## relative diff
        exp_paf_di = 100 * (mean(social_data$PM25_MEAN) - social_per_decile[["exposure_mean"]][10]) / mean(social_data$PM25_MEAN), # PAF
        mort_abs_diff = social_per_decile[["mort_attr"]][1] - social_per_decile[["mort_attr"]][10], ## absolute diff
        mort_rel_diff = 100 * ( (social_per_decile[["mort_attr"]][1] / social_per_decile[["mort_attr"]][10]) - 1 ), ## relative diff
        mort_mean = (1e5 * sum(social_data$MORTALITY_ATTR) / sum(social_data$POPULATION))) |>
      dplyr::mutate(
        mort_paf_di = 100 * (mort_mean - social_per_decile[["mort_attr"]][10]) / mort_mean # PAF
      )


  }


  return(output_social)

}
