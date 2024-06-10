#' Attributable health cases based on relative risk

#' @description Distributes and store outputs by level of detail by aggregating or filtering impacts.
#' @param output_raw \code{Data frame} containing all input data and the calculation of health impacts.
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
#' @inherit attribute_deaths_lifetable_rr note
#' @export
get_output <-
  function(output_raw){

    output <- list(main = output_raw[["main"]],
                   detailed = list(raw = output_raw[["main"]],
                                   interim = output_raw[["detailed"]]))


    output_last <- output_raw[["main"]]

    if(unique(output_last$method) == "absolute_risk"){

      output[["detailed"]][["agg_exp_cat"]] <-
        output_raw[["main"]] %>%
        dplyr::mutate(exp = paste(exp, collapse = ", ")) %>%
        dplyr::group_by(
          across(c(-pop_exp, -exp, -absolute_risk_as_percent, -impact, -impact_rounded))) %>%
        dplyr::summarize(
          across(c(pop_exp, absolute_risk_as_percent, impact),
                 sum),
          .groups = "drop") %>%
        dplyr::mutate(impact_rounded = round(impact, 0))


      output_last <- output[["detailed"]][["agg_exp_cat"]]

    }

    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is defined
    if("geo_id_aggregated" %in% names(output_last)){

      output[["detailed"]][["agg_geo"]] <-
        output_last %>%
        # Group by higher geo level
        dplyr::group_by(geo_id_aggregated, exp_ci, bhd_ci, erf_ci, method) %>%
        dplyr::summarise(impact = sum(impact),
                         impact_rounded = round(impact),
                         .groups = "drop")%>%
        dplyr::bind_rows(output_last, .)

      output_last <- output[["detailed"]][["agg_geo"]]

    }


    # Filter for total list element
    # Keep only exp_ci = central and bhd_ci=central
    output[["main"]] <-
      output_last %>%
      dplyr::filter(!exp_ci %in% c("lower", "upper"))%>%
      {if("bhd_ci" %in% names(.))
        dplyr::filter(., !bhd_ci %in% c("lower", "upper")) else .}





    return(output)
  }

