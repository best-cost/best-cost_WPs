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
    # The main will chage below that we give a first value
    output <-
      impact_raw

    output[["detailed"]][["raw"]] <- impact_raw[["main"]]

    # # If lifetable approach --> store interim results of population impact by age
    # if(!is.null(impact_raw[["detailed"]])){
    #   output[["detailed"]][["detailed"]][["interim_lifetable"]] <-
    #     impact_raw_
    # }

    output_last <-
      output[["main"]]


    if(unique(impact_raw[["main"]]$approach_risk) == "absolute_risk") {

      output[["detailed"]][["agg_exp_cat"]] <-
        output_last %>%
        # Remove all impact rounded because
        # we have to round final results
        # not summing rounded results ("too rounded")
        dplyr::select(., -all_of(intersect(paste0("impact_rounded", c("", "_1", "_2")),
                                        names(.)))) %>%
        # Collapse the exposure categories to have only a vector
        dplyr::mutate(., across(all_of(intersect(c(paste0("exp", c("", "_1", "_2")),
                                                paste0("pop_exp", c("", "_1", "_2")),
                                                "exposure_dimension"),
                                              names(.))),
                             ~ paste(., collapse = ", ")))%>%
        # Sum columns to summarize
        dplyr::group_by(.,
          across(all_of(setdiff(names(.),
                                c("geo_id_raw",
                                  "pop_exp",
                                  paste0("exp", c("", "_1", "_2")),
                                  paste0("pop_exp", c("", "_1", "_2")),
                                  paste0("rr_conc", c("", "_1", "_2")),
                                  paste0("pop_fraction", c("", "_1", "_2")),
                                  paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                                  paste0("impact", c("", "_1", "_2"))))))) %>%
        dplyr::summarize(.,
          across(all_of(intersect(c(paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                                    paste0("impact", c("", "_1", "_2")),
                                    "impact_social"),
                                  names(.))),
                 ~sum(.x, na.rm = TRUE)),
          .groups = "drop") %>%
        # Round impact
        dplyr::mutate(impact_rounded = round(impact, 0))


      output_last <- output[["detailed"]][["agg_exp_cat"]]

    }

    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is defined
    if("geo_id_aggregated" %in% names(output_last)){

      output[["detailed"]][["agg_geo"]]  <-
        output_last %>%
        # Group by higher geo level
        dplyr::group_by(., across(all_of(intersect(c("exposure_name",
                                                     "geo_id_aggregated", "exp_ci",
                                                     "bhd_ci", "erf_ci","dw_ci"),
                                                names(.)))))%>%
        {if(!"impact_deprivation_weighted" %in% names(output_last))
          dplyr::summarise(.,
                           impact = sum(impact),
                           impact_rounded = round(impact),
                           .groups = "drop")
          else
            dplyr::summarise(.,
                             impact = sum(impact),
                             impact_rounded = round(impact),
                             impact_deprivation_weighted = sum(impact_deprivation_weighted),
                             impact_deprivation_weighted_rounded = round(impact_deprivation_weighted),
                             .groups = "drop")
          }


      output_last <- output[["detailed"]][["agg_geo"]]

    }

    # Aggregate results across pollutants (exposures)
    # if approach_multiexposure == "additive"
    if(length(unique(impact_raw[["main"]]$exposure_name)) > 1){

      output[["detailed"]][["agg_exp_names"]]  <-
        output_last %>%
        dplyr::mutate(
          exposure_name = paste(unique(exposure_name), collapse = ", ")) %>%
        # Group by higher geo level
        dplyr::group_by(., across(all_of(intersect(c("geo_id_aggregated", "exp_ci",
                                                     "bhd_ci", "erf_ci","dw_ci"),
                                                   names(.)))))%>%
        dplyr::summarise(impact = sum(impact),
                         impact_rounded = round(impact),
                         .groups = "drop")

      output_last <- output[["detailed"]][["agg_exp_names"]]


    }

    # Filter for total list element
    # Keep only exp_ci = central and bhd_ci = central
    output[["main"]] <-
      output_last %>%
      dplyr::filter(exp_ci %in% c("central")) %>%
      {if("bhd_ci" %in% names(.))
        dplyr::filter(., bhd_ci %in% c("central")) else .} %>%
      {if(unique(impact_raw[["main"]]$health_metric) %in%
          c("yld_from_prevalence", "yld_from_lifetable"))
        dplyr::filter(., dw_ci %in% "central") else .}


    return(output)
  }
