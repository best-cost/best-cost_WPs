#' Attributable health cases based on relative risk

#' @description Distributes and store outputs by level of detail by aggregating or filtering impacts.
#' @param impact_raw \code{Data frame} containing all input data and the calculation of health impacts.
#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @keywords internal
get_output <-
  function(impact_raw){

    # Store output so far
    # The main will change below that we give a first value
    output <-
      impact_raw

    output[["detailed"]][["raw"]] <- impact_raw[["main"]]

    output_last <- output[["main"]]


    if(unique(impact_raw[["main"]]$approach_risk) == "absolute_risk") {

      output[["detailed"]][["agg_exp_cat"]] <-
        output_last |>
        # Remove all impact rounded because
        # we have to round final results
        # not summing rounded results ("too rounded")
        dplyr::select(-all_of(intersect(paste0("impact_rounded", c("", "_1", "_2")),
                                        names(output_last)))) |>
        dplyr::group_by(geo_id_raw) |>
        # Collapse the exposure categories to have only a vector
        dplyr::mutate(across(all_of(intersect(c(paste0("exp", c("", "_1", "_2")),
                                                paste0("pop_exp", c("", "_1", "_2")),
                                                paste0("prop_pop_exp", c("", "_1", "_2")),
                                                "exposure_dimension"),
                                              names(output_last))),
                             ~ paste(., collapse = ", ")))

      output[["detailed"]][["agg_exp_cat"]] <-
        output[["detailed"]][["agg_exp_cat"]] |>
        # Sum columns to summarize
        dplyr::group_by(
          across(all_of(setdiff(names(output[["detailed"]][["agg_exp_cat"]]),
                                c("geo_id_raw",
                                  paste0("exp", c("", "_1", "_2")),
                                  paste0("population", c("", "_1", "_2")),
                                  paste0("prop_pop_exp", c("", "_1", "_2")),
                                  paste0("pop_exp", c("", "_1", "_2")),
                                  paste0("rr_conc", c("", "_1", "_2")),
                                  paste0("pop_fraction", c("", "_1", "_2")),
                                  paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                                  paste0("impact", c("", "_1", "_2")),
                                  paste0("impact_per_100k_inhab", c("", "_1", "_2"))))))) |>
        dplyr::summarize(
          across(all_of(intersect(c(paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                                    paste0("impact", c("", "_1", "_2")),
                                    "impact_social"),
                                  names(output[["detailed"]][["agg_exp_cat"]]))),
                 ~sum(.x, na.rm = TRUE)),
          .groups = "drop") |>
        # Round impact
        dplyr::mutate(impact_rounded = round(impact, 0))


      output_last <- output[["detailed"]][["agg_exp_cat"]]

    }

    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is defined
    if("geo_id_aggregated" %in% names(output_last)){

      output[["detailed"]][["agg_geo"]]  <-
        output_last |>
        # Group by higher geo level
        dplyr::group_by(across(all_of(intersect(
                          c("exposure_name",
                            "geo_id_aggregated",
                            "erf_ci", "exp_ci", "bhd_ci", "dw_ci"),
                          names(output_last)))))

        if (!"population" %in% names(output_last)) {
          output[["detailed"]][["agg_geo"]]  <- output[["detailed"]][["agg_geo"]] |>
          dplyr::summarise(impact = sum(impact),
                           impact_rounded = round(impact),
                           .groups = "drop")

        } else {
          output[["detailed"]][["agg_geo"]]  <- output[["detailed"]][["agg_geo"]] |>
          dplyr::summarise(impact = sum(impact),
                           impact_rounded = round(impact),
                           population = sum(population),
                           impact_per_100k_inhab = impact/population,
                           .groups = "drop")
        }


      output_last <- output[["detailed"]][["agg_geo"]]

    }

    # Aggregate results across pollutants (exposures)
    # if approach_multiexposure == "additive"
    if(length(unique(impact_raw[["main"]]$exposure_name)) > 1){

      output[["detailed"]][["agg_exp_names"]]  <-
        output_last |>
        dplyr::mutate(
          exposure_name = paste(unique(exposure_name), collapse = ", ")) |>
        # Group by higher geo level
        dplyr::group_by(across(all_of(intersect(c("geo_id_aggregated", "exp_ci",
                                                     "bhd_ci", "erf_ci","dw_ci", "cutoff_ci", "duration_ci"),
                                                   names(output_last)))))|>
        dplyr::summarise(impact = sum(impact),
                         impact_rounded = round(impact),
                         .groups = "drop")

      output_last <- output[["detailed"]][["agg_exp_names"]]


    }

    # Keep only exp_ci = central and bhd_ci = central in main output ###########
    output[["main"]] <-
      output_last |>
      dplyr::filter(exp_ci %in% c("central"))

    if("bhd_ci" %in% names(output[["main"]])) {

      output[["main"]] <- output[["main"]] |>
        dplyr::filter(bhd_ci %in% c("central"))}

    if("cutoff_ci" %in% names(output[["main"]])) {

      output[["main"]] <- output[["main"]] |>
        dplyr::filter(cutoff_ci %in% c("central"))}

    if(unique(impact_raw[["main"]]$health_metric) %in%
       c("yld_from_prevalence", "yld_from_lifetable")) {

      output[["main"]] <- output[["main"]] |>
        dplyr::filter(dw_ci %in% "central")}

    # Order columns ############################################################
    # putting first (on the left) those that determine different results across rows

    id_columns <- c("geo_id_aggregated", "geo_id_raw",
                    "sex",
                    "erf_ci","exp_ci", "bhd_ci", "cutoff_ci", "dw_ci", "duration_ci")

    put_first_cols <-
      function(x, cols){
        dplyr::select(x,
                      all_of(intersect(cols,
                                       names(x))),
                      everything())
      }

    put_first_cols_recursive <-
      function(x, cols){

        # If x is a data.frame
        if(is.data.frame(x)){
          put_first_cols(x, cols)

        # If x is list and all list elements are data frames (and not lists)
        }else if (is.list(x) & all(purrr::map_lgl(x, is.data.frame))){
          purrr::map(
            .x = x,
            .f = ~ put_first_cols(.x, cols))

        }else{x}

      }

    output <-
      purrr:::map(
        .x = output,
        .f = ~ put_first_cols_recursive(x = .x,
                                        cols = id_columns))




    return(output)
  }
