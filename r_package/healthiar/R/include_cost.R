#' include_cost

#' @description Monetize health impacts
#'
#' @param approach_discount \code{String} referring to the method to be used for the discounting choosing between the default "direct" (after obtaining the health impacts) and the alternative "indirect" (before the health impacts).
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results.
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).
#' @param valuation \code{Numberic value} referring to unit value of a health impact
#' @param time_period \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @param valuation \code{Numeric value} showing the value of statistical life which will be used in the health impact monetization
#' @inheritParams attribute
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_cost <- function(approach_discount = "direct",
                         output = NULL,
                         impact = NULL,
                         valuation,
                         corrected_discount_rate = NULL,
                         time_period = 1,
                         discount_shape = NULL) {



  # Indirect approach #######
  # This means applying the discount within the lifetable method
  if(approach_discount == "indirect"){

    outcome_metric <- unique(output[["health_detailed"]][["step_by_step_from_lifetable"]]$outcome_metric)
    discount_factor <- corrected_discount_rate + 1

    cost_output <-
      output[["health_detailed"]][["step_by_step_from_lifetable"]] |>

      ## Calculate total, discounted life years (single value) per sex & ci
      dplyr::mutate(
        impact_with_discount_nest = purrr::pmap(
          list(.x = lifeyears_nest, .y = last_year + 1, .z = impact_for_discounting_nest),
          function(.x, .y, .z){

            ## Calculate total, discounted life years (single value) per sex & ci ####
            lifeyear_nest_with_discount <-
              .x |>
              # Convert year to numeric
              dplyr::mutate(year = as.numeric(year),
                            time_period = year - {{year_of_analysis}},
                            corrected_discount_rate = {{corrected_discount_rate}},
                            discount_shape = {{discount_shape}}) |>

              # Calculate discount rate for each year
              dplyr::mutate(
                discount_factor =
                  healthiar::get_discount_factor(
                    corrected_discount_rate = corrected_discount_rate,
                    time_period = time_period,
                    discount_shape = discount_shape))|>
              # Calculate life years discounted

              dplyr::mutate(
                discounted_impact = impact * discount_factor)

            ## If yll or yld

            if({{outcome_metric}} %in% c("yll", "yld")){

              lifeyear_nest_with_discount <-
                ## Filter for the relevant years
                dplyr::filter(.data = lifeyear_nest_with_discount,
                              year < .y) |>
                ## Sum among years to obtain the total impact (single value)
                dplyr::summarise(impact = sum(discounted_impact), .groups = "drop")
            }


            ## Add a column to indicate that the impact is discounted
            lifeyear_nest_with_discount <-
              lifeyear_nest_with_discount |>
              dplyr::mutate(discounted = TRUE)

            ## Bind rows to have both discounted and not discounted
            lifeyear_nest_with_and_without_discount <-
              dplyr::bind_rows(.z,
                               lifeyear_nest_with_discount)

            return(lifeyear_nest_with_and_without_discount)

          }
        )
      )|>
      # Remove column impact to avoid duplication
      dplyr::select(-impact, -discounted) |>
      ## Unnest the obtained impacts to integrate them the main tibble
      ## Impact saved in column impact
      tidyr::unnest(impact_with_discount_nest)


    if ( ( unique(output[["health_main"]]$approach_risk) == "relative_risk" ) ) {
      output[["health_main"]] <- output[["health_main"]] |>
        dplyr::mutate(impact_rounded = round(impact, 0))
    }

    # Calculate impact per 100K inhab.

    if("population" %in% colnames(output[["health_main"]])){
      output[["health_main"]] <-
        output[["health_main"]] |>
        dplyr::mutate(
          impact_per_100k_inhab = (impact / population) *1E5
        )
    }

    # Get the main and detailed output by aggregating and/or filtering cases (rows)
    output <-
      healthiar:::get_output(output)


  }


  # Direct approach #######
  # This means applying the discount after obtaining the attributable health impact
  if(approach_discount == "direct"){
    # Identify the relevant columns for monetization that are in the output
    relevant_columns <-
      c("info", "geo_id_raw", "geo_id_aggregated",
        "impact", "valuation", "corrected_discount_rate",
        "cost_without_discount", "cost", "cost_rounded")

    if(!is.null(output) & is.null(impact)){
      # Duplicate output to work with costs
      cost_output <-
        output

      # Apply the function in main and detailed results
      cost_output[["cost_main"]] <-
        healthiar:::add_monetized_impact(df = output[["health_main"]],
                                         valuation = valuation,
                                         corrected_discount_rate = corrected_discount_rate,
                                         time_period = time_period,
                                         discount_shape = discount_shape) |>
        # Keep only relevant columns for monetization
        dplyr::select(
          # The columns containing "_ci" are the uncertainties that define the rows
          contains("_ci"),
          # Use any_of() instead of all_of() because depending on the calculation pathway
          # there might not be any of the relevant_columns
          any_of(relevant_columns))

      cost_output[["cost_detailed"]]<-
        healthiar:::add_monetized_impact(df = output[["health_detailed"]][["raw"]],
                                         valuation = valuation,
                                         corrected_discount_rate = corrected_discount_rate,
                                         time_period = time_period,
                                         discount_shape = discount_shape) |>
        # Keep only relevant columns for monetization
        dplyr::select(any_of(relevant_columns), contains("_ci"))
    }

    else if(!is.null(impact) & is.null(output)){
      # Apply the function in main and detailed results
      cost_output <-
        healthiar:::add_monetized_impact(df = data.frame(impact = impact),
                                         valuation = valuation,
                                         corrected_discount_rate = corrected_discount_rate,
                                         time_period = time_period,
                                         discount_shape = discount_shape)
    }
  }





  return(cost_output)

}
