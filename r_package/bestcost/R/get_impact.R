#' Attributable health cases based on relative risk

#' @description Calculates the health impacts for each uncertainty and geo area.
#' @param input \code{Data frame} containing all input data.
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
get_impact <-
  function(input,
           year_of_analysis = NULL,
           min_age = NULL,
           max_age = NULL,
           corrected_discount_rate = NULL,
           duration = NULL,
           pop_fraction_type){

    if(unique(input$approach_risk) == "relative_risk"){
      # Get pop_fraction and add to the input data frame
      input_with_risk_and_pop_fraction <-
        bestcost:::get_risk_and_pop_fraction(input = input,
                                             pop_fraction_type = pop_fraction_type)

      if(unique(input$health_metric) %in% "same_input_output") {

        # Get pop_fraction and add it to the input data frame
        impact_raw_main <-
          input_with_risk_and_pop_fraction %>%
          # Build the result table adding the impact to the input table
          dplyr::mutate(impact = pop_fraction * bhd) %>%
          # Order columns
          dplyr::select(exp_ci, bhd_ci, erf_ci,
                        pop_fraction, impact,
                        everything())

        impact_raw = list(main = impact_raw_main)

      } else if (unique(input$health_metric) %in% "yld_from_prevalence") {

        # Add impact
        impact_raw_main <-
          # impact_raw_main %>% # Line for commented out code above
          input_with_risk_and_pop_fraction %>%
          dplyr::mutate(impact = pop_fraction * bhd) %>%
          dplyr::mutate(., impact = impact * dw)  %>%
          # Order columns
          dplyr::select(exp_ci, bhd_ci, erf_ci,
                        pop_fraction, impact,
                        everything())

        impact_raw = list(main = impact_raw_main)

        } else if (unique(input$health_metric) %in% c("deaths_from_lifetable",
                                                      "yll_from_lifetable",
                                                      "yld_from_lifetable")) {
          outcome_metric <-
            gsub("_from_lifetable", "", unique(input$health_metric))


          # Get population impact ####
          pop_impact <-
            bestcost:::get_pop_impact(
              year_of_analysis = year_of_analysis,
              input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction,
              outcome_metric = outcome_metric,
              min_age = min_age)


          impact_raw <-
            bestcost:::get_deaths_yll_yld(
              outcome_metric = outcome_metric,
              pop_impact = pop_impact,
              year_of_analysis = year_of_analysis,
              min_age = min_age,
              max_age = max_age,
              corrected_discount_rate = corrected_discount_rate,
              duration = duration,
              input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

    } else if (unique(input$health_metric) %in% "daly_from_lifetable"){

      pop_impact <-
        bestcost:::get_pop_impact(
          year_of_analysis = year_of_analysis,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction,
          outcome_metric = "daly",
          min_age = min_age)


      impact_raw <-
        bestcost:::get_daly(
          outcome_metric = outcome_metric,
          pop_impact = pop_impact,
          year_of_analysis = year_of_analysis,
          min_age = min_age,
          max_age = max_age,
          corrected_discount_rate = corrected_discount_rate,
          duration = duration,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

    }

    } else if(unique(input$approach_risk) == "absolute_risk" &
              unique(input$health_metric) == "same_input_output"){

      # Calculate absolute risk for each exposure category ####
      impact_raw_main <-
        input %>%
        dplyr::mutate(
          absolute_risk_as_percent = bestcost::get_risk(exp = exp, erf_c = erf_c) ,
          impact = absolute_risk_as_percent/100 * pop_exp)

      impact_raw = list(main = impact_raw_main)

    }

    # Round impacts
    impact_raw[["main"]] <-
      impact_raw[["main"]] %>%
      dplyr::mutate(impact_rounded = round(impact, 0))



    return(impact_raw)

  }
