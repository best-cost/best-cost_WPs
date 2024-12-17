#' include_social

#' @description Consider socio-economic aspects in the results
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param social_indicator \code{Vector} with numeric values showing the deprivation score (indicator of economic wealth) of the fine geographical area (it should match with those used in \code{attribute} or \code{compare})
#' @param n_quantile code{Numeric value} referring the number of groups in the quantile
#' @param approach code{String} referring the approach to include the social aspects. To choose between "quantile" and ?
#' @inheritParams attribute
#'
#' @return Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.
#'
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_social <- function(output,
                           geo_id_raw,
                           social_indicator,
                           n_quantile = 10, # by default: decile
                           approach = "quantile") {

  output_social <-
    # Add deprivation score to detailed output
    output[["health_detailed"]][["raw"]] |>
    dplyr::left_join(
      x = _,
      y = dplyr::tibble(geo_id_raw = geo_id_raw,
                        social_indicator = social_indicator),
      by = "geo_id_raw")



  if ( approach == "quantile" ) {

    ## Exposure and attributable burden per deprivation quantile

    output_social <-
      output_social |>
      ## Remove NAs
      filter(!is.na(social_indicator)) |>
      ## Add ranking of deprivation score and quantiles
      dplyr::mutate(
        ranking = min_rank(desc(social_indicator)),
        quantile = dplyr::ntile(desc(social_indicator), n = n_quantile))

    ## Basic statistic to be used below
    ## Mean exposure in all geographical units (without stratification by quantile)

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
        ## Standard deviation of exposure
        exp_sd = sd(exp, na.rm = TRUE),
        ## Average attributable fraction in the overall data set
        pop_fraction_mean = mean(pop_fraction, na.rm = TRUE),
        ## Average impact in the overall data set
        impact_mean = mean(impact, na.rm = TRUE),
        ## Absolute impact and population (sum across all geo units),
        impact_sum = sum(impact, na.rm = TRUE),
        ## Impact rate in all geographical units (without stratification by quantile)
        ## per 100'000 inhabitants
        impact_rate = (impact_sum / population_sum) * 1e5) |>
      ## Pivot longer to prepare data to be joined below
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "parameter",
        values_to = "overall")


    ## Stratify by quantiles
    output_social_by_quantile <-
      output_social |>
      ## Group by geo_id to ensure that you get one result per geo_id
      ## keeping uncertainties
      dplyr::group_by(quantile) |>
      dplyr::summarize(
        bhd_sum = sum(bhd, na.rm = TRUE),
        population_sum = sum(population, na.rm = TRUE),
        bhd_rate = bhd_sum * 1e5 / population_sum,
        bhd_mean = mean(bhd, na.rm = TRUE),
        exp_mean = mean(exp, na.rm = TRUE),
        exp_sd = sd(exp, na.rm = TRUE),
        pop_fraction_mean = mean(pop_fraction, na.rm = TRUE),
        impact_mean = mean(impact, na.rm = TRUE),
        impact_sum = sum(impact, na.rm = TRUE),
        impact_rate = impact_sum * 1e5 / population_sum)



    ## Determine differences
    ## in bhd, exp, attributable health impact mean pop fraction between quantiles
    social_calculation <-
      output_social_by_quantile |>
      ## Pivot longer to prepare the data and have a column for parameter
      tidyr::pivot_longer(cols = contains("_"),
                          names_to = "parameter",
                          values_to = "difference_value") |>
      ## Put column parameter first
      dplyr::select(parameter, everything()) |>
      ## Order columns by parameter
      dplyr::arrange(parameter) |>
      ## Obtain the first (most deprived) and last (least deprived) values
      ## for each parameter
      dplyr::group_by(parameter) |>
      dplyr::summarize(
        first = first(difference_value),
        last = last(difference_value)) |>
      ## Add the overall (not by quantile) sums and means
      dplyr::left_join(
        x = _,
        y = overall,
        by = "parameter") |>
      ## Caculate absolute and relative differences
      dplyr::mutate(
        absolute_quantile = first - last,
        relative_quantile = absolute_quantile / last,
        absolute_overall = overall - last,
        relative_overall = absolute_overall / overall)


    ## Prepare main output table

    ### ALTERNATIVE
    social_results_alt <-
      social_calculation |>
      ## Filter for relevant rows
      dplyr::filter(
        parameter %in% c("exp_mean",
                         "bhd_rate",
                         "pop_fraction_mean",
                         "impact_rate")) |>
      dplyr::rename(
        first_quantile = first,
        last_quantile = last,
        first_minus_last = absolute_quantile,
        first_minus_last_then_divided_by_overall = relative_quantile,
        overall_minus_last = absolute_overall,
        overall_minus_last_then_divided_by_overall = relative_overall
        )



    ### ORIGINAL
    social_results <-
      social_calculation |>
      ## Filter for relevant rows
      dplyr::filter(
        parameter %in% c("exp_mean",
                         "bhd_rate",
                        "pop_fraction_mean",
                        "impact_rate")) |>
      ## Long instead of wide layout
      tidyr::pivot_longer(
        cols = contains("_"),
        names_to = c("difference_type", "difference_compared_with"),
        values_to = "difference_value",
        names_sep = "_") |>
      ## Change and add columns
      dplyr::mutate(
        parameter_string =
          dplyr::case_when(
            grepl("exp_", parameter) ~ "exposure",
            grepl("bhd_", parameter) ~ "baseline health data",
            grepl("pop_fraction_", parameter) ~ "population attributable fraction",
            grepl("impact_", parameter) ~ "impact"),
        ## Replace "quantile" with "bottom_quantile"
        difference_compared_with =
          gsub("quantile", "bottom_quantile", difference_compared_with),
        ## Flag attributable fraction
        is_paf_from_deprivation =
          difference_type == "relative" & difference_compared_with == "overall",
        is_attributable_from_deprivation =
          difference_type == "absolute" & difference_compared_with == "overall",
        ## Add comment to clarify
        comment =
          dplyr::case_when(
            is_paf_from_deprivation ~
              paste0("It can be interpreted as fraction attributable to deprivation"),
            is_attributable_from_deprivation ~
              paste0("It can be interpreted as ", parameter_string, " attributable to deprivation"))) |>
      ## Remove columns that are not needed anymore
      dplyr::select(-is_paf_from_deprivation, -is_attributable_from_deprivation,
                    -parameter_string)

    output[["social_main"]][["original_layout"]] <-
      social_results |>
      ## Keep only impact as parameter
      ## This is the most relevant result.
      ## The other paramenters can be stored in detailed
      ## (just in case some users have interest on this)
      dplyr::filter(parameter == "impact_rate")
    output[["social_main"]][["alternative_layout"]] <-
      social_results_alt |>
      dplyr::filter(parameter == "impact_rate")

    output[["social_detailed"]][["results_detailed"]] <- social_results
    output[["social_detailed"]][["results_detailed_alternative_layout"]] <- social_results_alt
    output[["social_detailed"]][["overview_quantiles"]] <- output_social_by_quantile

  }



  return(output)

}
