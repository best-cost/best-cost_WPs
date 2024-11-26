#' include_social

#' @description Consider socio-economic aspects in the results
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param deprivation_score \code{Vector} with numeric values showing the deprivation score (indicator of economic wealth) of the fine geographical area (it should match with those used in \code{attribute} or \code{compare})
#' @param population code{Vector} with numeric values referring to the population in the geographical unit
#' @param n_quantile code{Numeric value} referring the number of groups in the quantile
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
                           deprivation_score,
                           population,
                           n_quantile = 10, # by default: decile
                           approach = "quantile") {



  output_social <-
    # Add deprivation score to detailed output
    output[["detailed"]][["raw"]] |>
    dplyr::left_join(
      x = _,
      y = dplyr::tibble(geo_id_raw = geo_id_raw,
                        deprivation_score = deprivation_score,
                        population = population),
      by = "geo_id_raw")



  if(approach == "quantile"){

    ## exposure and attributable burden per deprivation quantile

    output_social <-
      output_social |>
      # Remove NAs
      filter(!is.na(deprivation_score)) |>
      # Add ranking of deprivation score and quantiles
      dplyr::mutate(
        ranking = min_rank(desc(deprivation_score)),
        quantile = dplyr::ntile(desc(deprivation_score), n = n_quantile))

    # Basic statistic to be used below
    # Mean exposure in all geographical units (without stratification by quantile)
    exp_mean_overall = mean(output_social$exp, na.rm = TRUE)
    ## Absolute impact and population (sum across all geo units)
    impact_sum_overall = sum(output_social$impact, na.rm = TRUE)
    population_sum_overall = sum(output_social$population, na.rm = TRUE)
    ## Impact rate in all geographical units (without stratification by quantile)
    ## per 100'000 inhabitants
    impact_rate_overall = (impact_sum_overall / population_sum_overall) * 1e5

    # Stratification by quantiles
    output_social_by_quantile <-
      output_social |>
      # Group by geo_id to ensure that you get one result per geo_id
      # keeping uncertainties
      dplyr::group_by(quantile) |>
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
      output_social_per_quantile |>

      dplyr::summarize(
        # Exposure

        ## In comparison with bottom quantile
        ### absolute difference
        exp_abs_quantile = first(exposure_mean) - last(exposure_mean),
        ### relative difference
        exp_rel_quantile = exp_abs_quantile / last(exposure_mean),

        ## In comparison with overall
        ### absolute difference
        exp_abs_overall = exp_mean_overall - last(exposure_mean),
        # Attributable fraction of exposure in comparison with bottom quantile
        exp_rel_overall =  exp_abs_overall / exp_mean_overall,

        # Impact
        ## In comparison with overall
        ### absolute difference
        impact_abs_quantile = first(impact_rate) - last(impact_rate),
        ## relative difference
        impact_rel_quantile = impact_abs_quantile / last(impact_rate) ,

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

      dplyr::mutate(
        # Replace "quantile" with "bottom_quantile"
        compared_with = gsub("quantile", "bottom_quantile", compared_with),
        # Flag attributable fraction
        comment =
          ifelse(difference == "rel" & compared_with == "overall",
                 "Theoretical attributable fraction from deprivation",
                 "")
        )

    output[["detailed"]][["social"]] <- social_results




    if(approach == "multiplicative"){
      # Re-calculate output with the social aspects inside using output_raw

      output_social <-
        output_social |>
        dplyr::mutate(impact_social =
                        as.numeric(impact) * as.numeric(deprivation_score),
                      .after = impact) |>
        dplyr::mutate(impact_rounded_social =
                        round(impact_social),
                      .after = impact_rounded_social)

      # Based on the new output_raw that includes social aspects
      # Recalculate output
      output <- healthiar:::get_output(list(main = output_social))

    }


  }



  return(output)

}
