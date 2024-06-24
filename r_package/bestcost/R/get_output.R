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
#' @keywords internal
get_output <-
  function(output_raw){

    output <- list(main = output_raw[["main"]],
                   detailed = list(raw = output_raw[["main"]],
                                   interim = output_raw[["detailed"]]))


    output_last <- output_raw[["main"]]

    if(unique(output_last$risk_method) == "absolute_risk"){

      output[["detailed"]][["agg_exp_cat"]] <-
        output_raw[["main"]] %>%
        # Remove all impact rounded because
        # we have to round final results
        # not summing rounded results ("too rounded")
        dplyr::select(-all_of(intersect(paste0("impact_rounded", c("", "_1", "_2")),
                                        names(.)))) %>%
        # Collapse the exposure categories to have only a vector
        dplyr::mutate(across(all_of(intersect(paste0("exp", c("", "_1", "_2")),
                                              names(.))),
                             ~ paste(., collapse = ", ")))%>%
        # Sum columns to summarize
        dplyr::group_by(
          across(all_of(setdiff(names(.),
                                c("geo_id_raw",
                                  "pop_exp",
                                  paste0("exp", c("", "_1", "_2")),
                                  paste0("rr_conc", c("", "_1", "_2")),
                                  paste0("pop_fraction", c("", "_1", "_2")),
                                  paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                                  paste0("impact", c("", "_1", "_2"))))))) %>%
        dplyr::summarize(
          across(all_of(intersect(c("pop_exp",
                                    paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                                    paste0("impact", c("", "_1", "_2"))),
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
        dplyr::group_by(across(all_of(intersect(c("geo_id_aggregated", "exp_ci",
                                                  "bhd_ci", "erf_ci"),
                                                names(.)))))%>%
        dplyr::summarise(impact = sum(impact),
                         impact_rounded = round(impact),
                         .groups = "drop")


      output_last <- output[["detailed"]][["agg_geo"]]

    }


    # Filter for total list element
    # Keep only exp_ci = central and bhd_ci = central
    output[["main"]] <-
      output_last %>%
      dplyr::filter(!exp_ci %in% c("lower", "upper"))%>%
      {if("bhd_ci" %in% names(.))
        dplyr::filter(., !bhd_ci %in% c("lower", "upper")) else .}





    return(output)
  }
