#' Attributable health cases based on relative risk

#' @description Calculates the health impacts for each uncertainty and geo area.
#' @inheritParams attribute
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
           time_horizon = NULL,
           min_age = NULL,
           max_age = NULL,
           corrected_discount_rate = NULL,
           approach_discount = NULL,
           pop_fraction_type,
           population = NULL){

    # Relative risk ############################################################
    if(unique(input$approach_risk) == "relative_risk"){
      # Get pop_fraction and add to the input data frame
      input_with_risk_and_pop_fraction <-
        healthiar:::get_risk_and_pop_fraction(input = input,
                                              pop_fraction_type = pop_fraction_type)

      # * Same input as output #################################################

      if(unique(input$health_metric) %in% "same_input_output") {

        # Get pop_fraction and add it to the input data frame
        impact_raw_main <-
          input_with_risk_and_pop_fraction |>
          # Build the result table adding the impact to the input table
          dplyr::mutate(impact = pop_fraction * bhd) |>
          # Order columns
          dplyr::select(exp_ci, bhd_ci, erf_ci,
                        pop_fraction, impact,
                        everything())

        impact_raw <- list(health_main = impact_raw_main)

        # * YLD ################################################################
      } else if (unique(input$health_metric) %in% "yld") {

        # Add impact
        impact_raw_main <-
          # impact_raw_main |> # Line for commented out code above
          input_with_risk_and_pop_fraction |>
          dplyr::mutate(impact = pop_fraction * bhd) |>
          dplyr::mutate(impact = impact * dw * duration)  |>
          # Order columns
          dplyr::select(exp_ci, bhd_ci, erf_ci,
                        pop_fraction, impact,
                        everything())

        impact_raw <- list(health_main = impact_raw_main)

        # * Lifetable ##########################################################
        } else if (unique(input$health_metric) %in% c("deaths_from_lifetable",
                                                      "yll_from_lifetable",
                                                      "yld_from_lifetable")) {
          outcome_metric <-
            gsub("_from_lifetable", "", unique(input$health_metric))

          pop_impact <-
            healthiar:::get_pop_impact(
              year_of_analysis = year_of_analysis,
              time_horizon = time_horizon,
              input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction,
              outcome_metric = outcome_metric,
              min_age = min_age)


          impact_raw <-
            healthiar:::get_deaths_yll_yld(
              outcome_metric = outcome_metric,
              pop_impact = pop_impact,
              year_of_analysis = year_of_analysis,
              time_horizon = time_horizon,
              min_age = min_age,
              max_age = max_age,
              corrected_discount_rate = corrected_discount_rate,
              approach_discount = approach_discount,
              input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

    } else if (unique(input$health_metric) %in% "daly_from_lifetable"){

      pop_impact <-
        healthiar:::get_pop_impact(
          year_of_analysis = year_of_analysis,
          time_horizon = time_horizon,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction,
          outcome_metric = "daly",
          min_age = min_age)


      impact_raw <-
        healthiar:::get_daly(
          outcome_metric = outcome_metric,
          pop_impact = pop_impact,
          year_of_analysis = year_of_analysis,
          time_horizon = time_horizon,
          min_age = min_age,
          max_age = max_age,
          corrected_discount_rate = corrected_discount_rate,
          input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)
    }


      # Absolute risk ##########################################################

    } else if (
      unique(input$approach_risk) == "absolute_risk" &
      unique(input$health_metric) == "same_input_output"
      ){

      # Calculate absolute risk for each exposure category
      impact_raw_main <-
        input |>
        dplyr::mutate(
          absolute_risk_as_percent = healthiar::get_risk(exp = exp, erf_eq = erf_eq),
          pop_exp = population * prop_pop_exp,
          impact = absolute_risk_as_percent/100 * pop_exp) |>
        dplyr::mutate(impact_rounded = round(impact, 0))

      impact_raw <- list(health_main = impact_raw_main)

    }


    # Store results ############################################################

    # Note: column is called prop_pop_exp (rr case) or pop_exp (ar case)
    # * Single geo unit ########################################################
    if ( ( unique(impact_raw[["health_main"]]$approach_risk) == "relative_risk" ) &
         ( unique(impact_raw[["health_main"]]$exposure_type) == "exposure_distribution" ) &
         ( !grepl("from_lifetable", impact_raw[["health_main"]]$health_metric[1]) ) &
         ( max(impact_raw[["health_main"]]$geo_id_raw) == 1 ) ) {

      impact_raw[["health_main"]] <- impact_raw[["health_main"]] |>
        dplyr::select(-c(exp, prop_pop_exp, exposure_dimension)) |>
        dplyr::left_join(
          x = _,
          y = input |>
            dplyr::group_by(exp_ci, erf_ci, bhd_ci, cutoff_ci) |>
            dplyr::summarize(exp = list(exp),
                             prop_pop_exp = list(prop_pop_exp),
                             exposure_dimension = list(exposure_dimension),
                             .groups = "drop") |>
            dplyr::select(c(exp_ci, erf_ci, exp, prop_pop_exp, exposure_dimension)),
          by = c("exp_ci", "erf_ci")
        )|>
        dplyr::mutate(exposure_type = input$exposure_type |> dplyr::first())

      # * Multiple geo units####################################################

    } else if ( ( unique(impact_raw[["health_main"]]$approach_risk) == "relative_risk" ) &
                ( unique(impact_raw[["health_main"]]$exposure_type) == "exposure_distribution" ) &
                ( !grepl("from_lifetable", impact_raw[["health_main"]]$health_metric[1]) ) &
                ( max(impact_raw[["health_main"]]$geo_id_raw) > 1 ) ) {

      impact_raw[["health_main"]] <- impact_raw[["health_main"]] |>
        dplyr::select(-c(exp, prop_pop_exp, exposure_dimension)) |>
        dplyr::left_join(
          x = _,
          y = input |>
            dplyr::group_by(exp_ci, erf_ci, bhd_ci, cutoff_ci, geo_id_raw) |>
            dplyr::summarize(exp = list(exp),
                             prop_pop_exp = list(prop_pop_exp), # Introduced error in ar pathway
                             exposure_dimension = list(exposure_dimension),
                             .groups = "drop") |>
            dplyr::select(c(exp_ci, erf_ci, exp, prop_pop_exp, exposure_dimension, geo_id_raw)),
          by = c("exp_ci", "erf_ci", "geo_id_raw")
        )|>
        dplyr::mutate(exposure_type = input$exposure_type |> dplyr::first())

    }

    if ( ( unique(impact_raw[["health_main"]]$approach_risk) == "relative_risk" ) ) {
      impact_raw[["health_main"]] <- impact_raw[["health_main"]] |>
        dplyr::mutate(impact_rounded = round(impact, 0))
    }

    # Calculate impact per 100K inhab.

    if("population" %in% colnames(impact_raw[["health_main"]])){
      impact_raw[["health_main"]] <-
        impact_raw[["health_main"]] |>
        dplyr::mutate(
          impact_per_100k_inhab = (impact / population) *1E5
        )
    }



    return(impact_raw)

  }
