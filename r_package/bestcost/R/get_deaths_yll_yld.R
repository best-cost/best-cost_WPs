# Title and description

#' Get deaths, YLL or YLD
#'
#' Get attributable deaths, years of life lost or years lived with disability from lifetable
#' @param outcome_metric \code{String} with the metric of the health outcome to be assessed. Options: "deaths", "yll" or "yld".
#' @param pop_impact \code{Data frame} with projected population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min \code{Numeric value}  with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max \code{Numeric value}  with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data
#' @param input_with_risk_and_pop_fraction \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @param corrected_discount_rate \code{Numeric value}  with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
#' @param disability_weight \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the duration (in years) of the morbidity health outcome
#' @return
#' This function returns a \code{List}
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @keywords internal
#'
#'
#'
get_deaths_yll_yld <-
  function(outcome_metric,
           pop_impact,
           year_of_analysis,
           min_age = NULL,
           max_age = NULL,
           input_with_risk_and_pop_fraction,
           corrected_discount_rate = NULL,
           disability_weight = NULL,
           duration = NULL){

    impact_detailed <-
      pop_impact %>%
      dplyr::mutate(
        lifeyears_nest =
          pop_impact_nest %>%
          purrr::map(.,
          function(.x){
            # Filter keeping only the relevant age
            # use {{}} to refer to the argument and avoid warnings
            if(!is.null({{max_age}})){
              .x <-
                .x %>%
                dplyr::filter(., age <= {{max_age}})
            }

            if(!is.null({{min_age}})){
              .x <-
                .x %>%
                dplyr::filter(., age >= {{min_age}})
            }

            # If YLL or YLD
            # Further data preparation is needed than for deaths
            if(outcome_metric %in% c("yll", "yld", "yll")){
              # Select relevant
              .x <-
                .x %>%
                dplyr::select(., contains("population_")) %>%
                # Remove the year of analysis (we are only interested in the following ones)
                {if(outcome_metric != "yll_airqplus") dplyr::select(., -contains(as.character(year_of_analysis))) else .} %>%
                # Sum over ages (i.e. vertically) that fulfill inputted "max_age" and "min_age" criteria
                as.matrix() %>%
                { `[<-`(., upper.tri(., diag = TRUE), NA) } %>%
                as_tibble() %>%
                dplyr::summarize_all(sum, na.rm = TRUE) %>%
                # Reshape to long format (output is data frame with 2 columns "year" & "impact")
                tidyr::pivot_longer(cols = starts_with("population_"),
                                    names_to = "year",
                                    values_to = "impact",
                                    names_prefix = "population_") %>%
                # Convert year to numeric
                dplyr::mutate(year = as.numeric(year))
            } else
              .x<-.x}))

    impact_detailed <- impact_detailed %>%
        # Calculate total, not discounted YLL (single number) ####
    dplyr::mutate(impact_nest = purrr::map(
          lifeyears_nest,
          function(.x){

            # If deaths
            if(outcome_metric == "deaths"){
              .x <-
                .x %>%
                dplyr::select(.,all_of(paste0("population_", year_of_analysis))) %>%
                sum(., na.rm = TRUE)
            }

            # If yll
            if(outcome_metric %in% c("yll")){
              .x <-
                .x %>%
                dplyr::summarise(., impact = sum(impact, na.rm = TRUE)) %>%
                dplyr::mutate(discounted = FALSE)
            }

            # If yld
            if(outcome_metric %in% "yld"){
              .x <-
                .x %>%
                # Filter for the relevant years
                dplyr::filter(., year < (year_of_analysis + duration + 1)) %>%
                # Sum among years to obtain the total impact (single value)
                dplyr::summarise(
                  impact = sum(impact, na.rm = TRUE))%>%
                dplyr::mutate(impact = impact * {{disability_weight}},
                              discounted = FALSE)
            }
            return(.x)
          })
        )


    # If a value for corrected_discount_rate was provided by the user,
    # apply discount
    if(!is.null(corrected_discount_rate)){
    # if(corrected_discount_rate != 0) {
      discount_factor <- corrected_discount_rate + 1

      impact_detailed <-
        impact_detailed %>%
        dplyr::mutate(
          impact_nest = purrr::map2(
            lifeyears_nest, impact_nest,
            function(.x, .y){
              ## Calculate total, discounted life years (single value) per sex & ci ####
              x_discounted <-
                .x %>%
                # Convert year to numeric
                dplyr::mutate(year = as.numeric(year)) %>%
                # Calculate discount rate for each year
                dplyr::mutate(
                  discount = 1/(discount_factor^(year-(year_of_analysis+1)))) %>%
                # Calculate life years discounted
                dplyr::mutate(
                  discounted_impact = impact * discount) %>%
                {if(outcome_metric %in% "yll")
                  # Sum among years to obtain the total impact (single value)
                  dplyr::summarise(., impact = sum(discounted_impact), .groups = "drop") else .} %>%
                {if(outcome_metric %in% "yld")
                  # Filter for the relevant years
                  dplyr::filter(., year < (year_of_analysis + duration + 1)) %>%
                  # Sum among years to obtain the total impact (single value)
                  dplyr::summarise(impact = sum(discounted_impact)* {{disability_weight}}, .groups = "drop") else .} %>%
                dplyr::mutate(discounted = TRUE)


              # Bind rows to have both discounted and not discounted
              x_with_and_without_discount <-
                dplyr::bind_rows(.y, x_discounted)

              return(x_with_and_without_discount)

            }
          ))}

    # Last preparation of the detailed results
    impact_detailed <-
      impact_detailed %>%
      # Unnest the obtained impacts to integrated them the main tibble
      tidyr::unnest(., impact_nest) %>%
      {if(outcome_metric == "deaths") dplyr::rename(., impact = impact_nest) else .}%>%
      # Add  metric
      dplyr::mutate(
        outcome_metric = outcome_metric)

    # Obtain total rows (sum across sex)
    impact_detailed_total <-
      impact_detailed %>%
      # Sum across sex adding total
      dplyr::group_by(.,
                      across(-c(sex, impact, contains("nest"))))%>%
                      # across(all_of(intersect(c("geo_id_raw", "geo_id_aggregated",
                      #                           "discounted", "erf_ci"),
                      #                         names(.))))) %>%
      dplyr::summarise(.,
                       across(.cols = c(impact), sum),
                       across(sex, ~"total"),
                       across(contains("nest"), ~list("total")),
                    .groups = "drop")

    # Bind the rows of impact_detailed and the totals
    impact_detailed <-
      dplyr::bind_rows(impact_detailed, impact_detailed_total)


    # Get the main results starting from a detailed table of results
    impact_main <-
      impact_detailed %>%
      dplyr::select(., -contains("nest"))%>%
      dplyr::filter(., sex %in% "total") %>%
      {if(!is.null(corrected_discount_rate))
      # {if(corrected_discount_rate != 0)
        dplyr::filter(., discounted %in% TRUE) else .}




    # Classify results in main and detailed
    output <- list(main = impact_main,
                   detailed = impact_detailed)

    return(output)


  }
