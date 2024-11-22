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

    output_social <-
      output_social |>
      dplyr::mutate(impact_social =
                      as.numeric(impact) * as.numeric(deprivation_index),
                    .after = impact) |>
      dplyr::mutate(impact_rounded_social =
                      round(impact_social),
                    .after = impact_rounded_social)

  # Based on the new output_raw that includes social aspects
  # Recalculate output
    output <- healthiar:::get_output(list(main = output_social))

    }


  if(approach == "decile"){

    ## exposure and attributable burden per deprivation decile

    output_social <-
      output_social |>
      # Remove NAs
      filter(!is.na(deprivation_index)) |>
      # Add ranking of deprivation index and deciles
      dplyr::mutate(
        ranking = min_rank(desc(deprivation_index)),
        decile = dplyr::ntile(desc(deprivation_index), n = 10))

    # Basic statistic to be used below
    # Mean exposure in all geographical units (without stratification by decile)
    exp_mean_overall = mean(output_social$exp, na.rm = TRUE)
    # Impact rate in all geographical units (without stratification by decile)
    impact_rate_overall = sum(output_social$impact, na.rm = TRUE) * 1e5 /
                           sum(output_social$population, na.rm = TRUE)

    # Stratification by Deciles
    output_social_per_decile <-
      output_social |>
      # Group by geo_id to ensure that you get one result per geo_id
      # keeping uncertainties
      dplyr::group_by(decile) |>
      dplyr::summarize(
        exposure_mean = mean(exp, na.rm = TRUE),
        impact_mean = mean(impact, na.rm = TRUE),
        bhd_mean = mean(bhd, na.rm = TRUE),
        impact_sum = sum(impact, na.rm = TRUE),
        bhd_sum = sum(bhd, na.rm = TRUE),
        population_sum = sum(population, na.rm = TRUE),
        impact_rate = impact_sum * 1e5 / population_sum,
        bhd_rate = bhd_sum * 1e5 / population_sum)



    ## inequalities
    social_results <-
      output_social_per_decile |>

      dplyr::summarize(
        # Exposure

        ## In comparison with top decile
        ### absolute difference
        exp_abs_decile = first(exposure_mean) - last(exposure_mean),
        ### relative difference
        exp_rel_decile = exp_abs_decile / last(exposure_mean),

        ## In comparison with overall
        ### absolute difference
        exp_abs_overall = exp_mean_overall - last(exposure_mean),
        # Attributable fraction of exposure in comparison with top decile
        exp_rel_overall =  exp_abs_overall / exp_mean_overall,

        # Impact
        ## In comparison with overall
        ### absolute difference
        impact_abs_decile = first(impact_rate) - last(impact_rate),
        ## relative difference
        impact_rel_decile = impact_abs_decile / last(impact_rate) ,

        ### absolute difference
        impact_abs_overall = impact_rate_overall - last(impact_rate),
        # Attributable fraction of impact
        impact_rel_overall = impact_abs_overall / impact_rate_overall) |>

      # Long instead of wide layout
      tidyr::pivot_longer(
        cols = everything(),
        names_to = c("parameter", "difference", "compared_with"),
        values_to = "value",
        names_sep = "_") |>

      # Replace "decile" with "top_decile"
      dplyr::mutate(compared_with = gsub("decile", "top_decile", compared_with))



    output[["detailed"]][["social"]] <- social_results


  }



  return(output)

}
