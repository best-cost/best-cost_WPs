#' include_cost

#' @description Monetize health impacts
#'
#' @param approach_discount \code{String} referring to the method to be used for the discounting choosing between the default "direct" (after obtaining the health impacts) and the alternative "indirect" (before the health impacts).
#' @param output \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results.
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).
#' @param valuation \code{Numberic value} referring to unit value of a health impact
#' @param corrected_discount_rate \code{Numeric value} showing the discount rate for future years including correction from inflation rate
#' @param discount_shape \code{String} referring to the assumed equation for the discount factor. Per default: "exponential". Otherwise: "hyperbolic_harvey_1986" or "hyperbolic_mazur_1987".
#' @param time_period \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @param valuation \code{Numeric value} showing the value of statistical life which will be used in the health impact monetization
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




  # Using the output of attribute ####
  if(!is.null(output) & is.null(impact)){

    # Indirect approach #######
    # This means applying the discount within the lifetable method

    if(approach_discount == "indirect"){

      outcome_metric <-
        unique(output[["health_detailed"]][["raw"]]$outcome_metric)

      # Store the original data (they refer to health)
      output_health <- output

      # Output will be adapted according to costs
      #TODO The names health are kept just provisionally until we adapt get_output()
      impact_detailed <-
        output[["health_detailed"]][["raw"]] |>

        ## Calculate total, discounted life years (single value) per sex & ci
        dplyr::mutate(
          impact_with_discount_nest = purrr::pmap(
            list(.x = lifeyears_nest, .y = last_year + 1, .z = impact_for_discounting_nest),
            function(.x, .y, .z){


              ## Calculate total, discounted life years (single value) per sex & ci ####
              lifeyear_nest_with_and_without_discount <-
                .x |>
                # Convert year to numeric
                dplyr::mutate(year = as.numeric(year),
                              # Ignore user defined time_period
                              # Here the difference between year of analysis and
                              # last year of mortality data is to be used
                              time_period = year - {{year_of_analysis}},
                              corrected_discount_rate = {{corrected_discount_rate}},
                              discount_shape = {{discount_shape}})|>


                # Calculate discount rate for each year
                dplyr::mutate(
                  discount_factor =
                    healthiar::get_discount_factor(
                      corrected_discount_rate = corrected_discount_rate,
                      time_period = time_period,
                      discount_shape = discount_shape)) |>

                # Calculate life years discounted

                dplyr::mutate(
                  impact_before_discount = impact,
                  impact_after_discount = impact * discount_factor,
                  impact = impact_after_discount)


              ## If yll or yld

              if({{outcome_metric}} %in% c("yll", "yld")){


                lifeyear_nest_with_and_without_discount <-
                  ## Filter for the relevant years
                  dplyr::filter(.data = lifeyear_nest_with_and_without_discount,
                                year < .y) |>
                  ## Sum among years to obtain the total impact (single value)
                  dplyr::summarise(impact_before_discount = sum(impact_before_discount),
                                   impact_after_discount = sum(impact_after_discount),
                                   impact = sum(impact),
                                   .groups = "drop")
              }

              return(lifeyear_nest_with_and_without_discount)

            }
          )
        )|>

        # Remove column impact to avoid duplication
        dplyr::select(-impact) |>
        ## Unnest the obtained impacts to integrate them the main tibble
        ## Impact saved in column impact
        tidyr::unnest(impact_with_discount_nest) |>
        # Round results
        dplyr::mutate(
          impact_rounded = round(impact, 0),
          cost = impact * valuation,
          cost_before_discount = impact_before_discount * valuation,
          cost_after_discount = impact_after_discount * valuation,
          # Round costs
          cost_rounded = round(cost),
          cost_before_discount_rounded = round(cost_before_discount),
          cost_after_discount_rounded = round(cost_after_discount))





      # Calculate impact per 100K inhab.

      if("population" %in% colnames(impact_detailed)){
        impact_detailed <-
          impact_detailed |>
          dplyr::mutate(
            impact_per_100k_inhab = (impact / population) *1E5
          )
      }


      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output_cost <-
        healthiar:::get_output(impact_detailed) |>
        # Rename the list elements (not anymore health but health including cost)
        setNames(c("cost_main", "cost_detailed"))

      # Keep only the main detailed data frame (raw) for cost
      output_cost[["cost_detailed"]] <- output_cost[["cost_detailed"]][["raw"]]

      # Add the list elements health_main and health_detailed
      output_cost <-
        c(output_health,
          output_cost)


    # Direct approach #######
    # This means applying the discount after obtaining the attributable health impact
    } else if(approach_discount == "direct"){


      # Duplicate output to work with costs
      output_cost <-
        output

      # Apply the function in main and detailed results
      output_cost[["cost_main"]] <-
        healthiar:::add_monetized_impact(df = output[["health_main"]],
                                         valuation = valuation,
                                         corrected_discount_rate = corrected_discount_rate,
                                         time_period = {{time_period}},
                                         discount_shape = discount_shape)

      output_cost[["cost_detailed"]]<-
        healthiar:::add_monetized_impact(df = output[["health_detailed"]][["raw"]],
                                         valuation = valuation,
                                         corrected_discount_rate = corrected_discount_rate,
                                         time_period = {{time_period}},
                                         discount_shape = discount_shape)
    }


    # For both direct and indirect approach
    # Identify the relevant columns for monetization that are in the output
    relevant_columns <-
      c("info", "geo_id_raw", "geo_id_aggregated",
        paste0("impact", c("", "_before_discount", "_after_discount")),
        "corrected_discount_rate", "discount_shape", "approach_discount",
        "valuation",
        paste0("cost", c("", "_before_discount", "_after_discount")),
        paste0("cost", c("", "_before_discount", "_after_discount"), "_rounded"))


    # Keep only relevant columns for monetization
    output_cost[c("cost_main", "cost_detailed")]<-
      purrr::map(
        .x = output_cost[c("cost_main", "cost_detailed")],
        ~ dplyr::select(
          .x,
          # The columns containing "_ci" are the uncertainties that define the rows
          contains("_ci"),
          # Use any_of() instead of all_of() because depending on the calculation pathway
          # there might not be any of the relevant_columns
          any_of(relevant_columns))
      )


    # Using user input ####
    # If the user only provide a number of the impact (not based on output of attribute)
    }else if(!is.null(impact) & is.null(output)){


      # The approach cannot be indirect
      # Apply the function in main and detailed results
      output_cost <-
        healthiar:::add_monetized_impact(
          df = data.frame(impact = impact),
          valuation = valuation,
          corrected_discount_rate = corrected_discount_rate,
          time_period = time_period,
          discount_shape = discount_shape)
  }





  return(output_cost)

}
