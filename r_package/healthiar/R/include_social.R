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
    overall <-
      output_social |>
      dplyr::summarize(
        # Sum of baseline health data in the overall data set
        bhd_sum = sum(bhd, na.rm = TRUE),
        # Sum of population in the overall data set
        population_sum = sum(population, na.rm = TRUE),
        # Baseline health data per 100k inhab.
        bhd_rate = bhd_sum * 1e5 / population_sum,
        # Average baseline health data in the overall data set
        bhd_mean = mean(bhd, na.rm = TRUE),
        # Average exposure in the overall data set
        exp_mean = mean(exp, na.rm = TRUE),
        # Average attributable fraction in the overall data set
        pop_fraction_mean = mean(pop_fraction, na.rm = TRUE),
        # Average impact in the overall data set
        impact_mean = mean(impact, na.rm = TRUE),
        ## Absolute impact and population (sum across all geo units),
        impact_sum = sum(impact, na.rm = TRUE),
        ## Impact rate in all geographical units (without stratification by quantile)
        ## per 100'000 inhabitants
        impact_rate = (impact_sum / population_sum) * 1e5) |>
      # Pivot longer to prepare data to be joined below
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "parameter",
        values_to = "overall")


    # Stratification by quantiles
    output_social_by_quantile <-
      output_social |>
      # Group by geo_id to ensure that you get one result per geo_id
      # keeping uncertainties
      dplyr::group_by(quantile) |>
      dplyr::summarize(
        bhd_sum = sum(bhd, na.rm = TRUE),
        population_sum = sum(population, na.rm = TRUE),
        bhd_rate = bhd_sum * 1e5 / population_sum,
        bhd_mean = mean(bhd, na.rm = TRUE),
        exp_mean = mean(exp, na.rm = TRUE),
        pop_fraction_mean = mean(pop_fraction, na.rm = TRUE),
        impact_mean = mean(impact, na.rm = TRUE),
        impact_sum = sum(impact, na.rm = TRUE),
        impact_rate = impact_sum * 1e5 / population_sum)



    ## inequalities
    social_calculation <-
      output_social_by_quantile |>
      # Pivot longer to prepare the data and have a column for parameter
      tidyr::pivot_longer(cols = contains("_"),
                          names_to = "parameter",
                          values_to = "difference_value") |>
      # Put column parameter first
      dplyr::select(parameter, everything()) |>
      # Order columns by parameter
      dplyr::arrange(parameter) |>
      # Obtain the first (most deprived) and last (least deprived) values
      # for each parameter
      dplyr::group_by(parameter) |>
      dplyr::summarize(
        first = first(difference_value),
        last = last(difference_value)) |>
      # Add the overall sums and means (not by quantile)
      dplyr::left_join(
        x = _,
        y = overall,
        by = "parameter") |>
      # Caculate absolute and relative differences
      dplyr::mutate(
        absolute_quantile = first - last,
        relative_quantile = absolute_quantile / last,
        absolute_overall = overall - last,
        relative_overall = absolute_overall / overall)


    social_results <-
      social_calculation |>
      # Remove rows that are not relevant in the output
      dplyr::filter(parameter %in%
                      c("exp_mean", "bhd_rate",
                        "pop_fraction_mean",
                        "impact_rate")) |>
    # Long instead of wide layout
      tidyr::pivot_longer(
        cols = contains("_"),
        names_to = c("difference_type", "difference_compared_with"),
        values_to = "difference_value",
        names_sep = "_") |>

      dplyr::mutate(
        # Replace "quantile" with "bottom_quantile"
        difference_compared_with = gsub("quantile", "bottom_quantile", difference_compared_with),
        # Flag attributable fraction
        comment =
          ifelse(difference_type == "relative" & difference_compared_with == "overall",
                 "Theoretical attributable fraction from deprivation",
                 ""))

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
