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
           lifetable_withPop = NULL,
           year_of_analysis = NULL,
           min_age = NULL,
           max_age = NULL,
           disability_weight = NULL,
           duration = NULL){

    if(unique(input$risk_method) == "relative_risk" &
       unique(input$health_metric) %in% c("same_input_output",
                                          "yld_from_prevalence")){
      # Get PAF and added to the input data frame
      output_raw_main <-
        bestcost:::get_risk_and_paf(input = input) %>%
        # Build the result table adding the paf to the input_risk_paf table
        dplyr::mutate(impact = paf * bhd) %>%
        {if(unique(input$health_metric) == "yld_from_prevalence")
          dplyr::mutate(., impact = impact * disability_weight) else .} %>%
        dplyr::mutate(
          impact_rounded =
            round(impact, 0)) %>%
        # Order columns
        dplyr::select(exp_ci, bhd_ci, erf_ci,
                      paf, impact, impact_rounded,
                      everything())

      output_raw <- list(main = output_raw_main)
    }

    else if (unique(input$risk_method) == "relative_risk" &
             unique(input$health_metric) %in%
             c("deaths_from_lifetable", "yll_from_lifetable", "yld_from_lifetable")){

     outcome_metric <-
       gsub("_from_lifetable", "", unique(input$health_metric))

      # Get PAF and add to the input data frame
      input_risk_paf <-
        bestcost:::get_risk_and_paf(input = input)

      # Get population impact ####
      pop_impact <-
        bestcost:::get_pop_impact(
          lifetable_withPop = lifetable_withPop,
          year_of_analysis = year_of_analysis,
          pop_fraction = input_risk_paf[, c("erf_ci", "paf")],
          outcome_metric = outcome_metric)

      if(outcome_metric == "deaths"){
        # Calculate deaths ####
        output_raw <-
          bestcost:::get_deaths(
            pop_impact = pop_impact,
            year_of_analysis = year_of_analysis,
            min_age = min_age,
            max_age = max_age,
            meta = input_risk_paf)

      } else if(outcome_metric == "yll"){
        output_raw <-
          bestcost:::get_yll(
            pop_impact = pop_impact,
            year_of_analysis = year_of_analysis,
            min_age = min_age,
            max_age = max_age,
            meta = input_risk_paf)

      } else if(outcome_metric == "yld"){
        output_raw <-
          bestcost:::get_yld(
            pop_impact = pop_impact,
            year_of_analysis = year_of_analysis,
            min_age = min_age,
            max_age = max_age,
            disability_weight = disability_weight,
            duration = duration,
            meta = input_risk_paf)
      }





    } else if(unique(input$risk_method) == "absolute_risk" &
              unique(input$health_metric) == "same_input_output"){

      # Calculate absolute risk for each exposure category ####
      output_raw_main <-
        input %>%
        dplyr::mutate(
          absolute_risk_as_percent = bestcost::get_risk(exp = exp, erf_c = erf_c, erf_full = TRUE) ,
          impact = absolute_risk_as_percent/100 * pop_exp,
          impact_rounded = round(impact, 0))

      output_raw <- list(main = output_raw_main)



    }

    return(output_raw)

  }





