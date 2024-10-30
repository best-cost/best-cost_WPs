#' Get deaths, YLL or YLD
#'
#' @description Get attributable deaths, years of life lost or years lived with disability from lifetable
#' @param outcome_metric \code{String} with the metric of the health outcome to be assessed. Options: "deaths", "yll" or "yld".
#' @param pop_impact \code{Data frame} with projected population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min \code{Numeric value}  with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max \code{Numeric value}  with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data
#' @param input_with_risk_and_pop_fraction \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @param corrected_discount_rate \code{Numeric value}  with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
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
           corrected_discount_rate = NULL){

    impact_detailed <- pop_impact |>
      dplyr::mutate(
        lifeyears_nest =
          purrr::map(
            .x =  pop_impact_nest,
            function(.x){

            # Set values in upper triangle to NA (also removes newborns values)
            if(outcome_metric == "deaths"){ # If TRUE Select columns containing "deaths"
              .x <- .x |>
              dplyr::mutate(
                across(contains("deaths"), ~ {
                  mat <- as.matrix(.x)
                  mat[upper.tri(mat, diag = FALSE)] <- NA
                  return(mat)}))

            } else { # ELSE Select columns containing "population"
              .x <- .x |>
              # Set values in upper triangle to NA (also removes newborns values)
              dplyr::mutate(
                across(contains("population"),
                       ~ {mat <- as.matrix(.x)
                       mat[upper.tri(mat, diag = FALSE)] <- NA
                       return(mat)}))
                }

            # Filter keeping only the relevant age
            # use {{}} to refer to the argument and avoid warnings
            if(!is.null({{max_age}})){
              .x <-
                dplyr::filter(.x, age <= {{max_age}})
            }

            if(!is.null({{min_age}})){
              .x <-
                dplyr::filter(.x, age >= {{min_age}})
            }

            # If YLL or YLD
            # Further data preparation is needed than for deaths
            if(outcome_metric %in% c("yll", "yld")){
              # Select relevant
              .x <- .x |>
                dplyr::select(contains("population_")) |>
                # Remove the year of analysis (we are only interested in the following ones)
                # {if(outcome_metric != "yll_airqplus") dplyr::select(., -contains(as.character(year_of_analysis))) else .} |>
                # Sum over ages (i.e. vertically) that fulfill inputted "max_age" and "min_age" criteria
            dplyr::summarize_all(sum, na.rm = TRUE) |>
            # Reshape to long format (output is data frame with 2 columns "year" & "impact")
            tidyr::pivot_longer(cols = starts_with("population_"),
                                names_to = "year",
                                values_to = "impact",
                                names_prefix = "population_") |>
            # Convert year to numeric
            dplyr::mutate(year = as.numeric(year))
            } else
              .x<-.x}), .before = 1)


    # Add disability weights to "impact_detailed"
    if(outcome_metric %in% "yld"){

      # Determine year- and age-specific YLD
      impact_detailed <- impact_detailed |>
        dplyr::mutate(yll_nest =
                        purrr::map2(
          .x = yll_nest, .y = dw,
          function(yll_nest, dw){
            # YLL * DW = YLD
            yll_nest <- yll_nest * dw
            return(yll_nest)
          }
        )
        )

      # Determine sum of YLD per year
      impact_detailed <- impact_detailed |>
        dplyr::mutate(lifeyears_nest =
                        purrr::map2(
                          .x = lifeyears_nest, .y = dw,
                          function(lifeyears_nest, dw){
                            lifeyears_nest <- lifeyears_nest |>
                              mutate(impact = impact * dw)
                            return(lifeyears_nest)
                          }
                        ))

    }



    # If deaths
    if(outcome_metric == "deaths"){
    impact_detailed <- impact_detailed |>
    # Store in new column "impact_nest"
    dplyr::mutate(
      impact_nest = purrr::map(
        .x = lifeyears_nest,
        function(.x){

            .x <- .x |>
              dplyr::select(.data = _, all_of(paste0("deaths_", year_of_analysis))) |>
              sum(na.rm = TRUE)
            return(.x)
        }
      )
    )
    }

    # If yld or daly
    if (outcome_metric %in% c("yld", "daly", "yll")) {

      impact_detailed <- impact_detailed |>
        # Create new column for filtering for final year with yld health outcome
        dplyr::mutate(last_year = year_of_analysis + duration)

      impact_detailed <- impact_detailed |>
        # Calculate total, not discounted YLL (single number) ####
      # Store in new column "impact_nest"
      dplyr::mutate(
        impact_nest = purrr::pmap(
          list(.x = lifeyears_nest, .y = last_year + 1),
          function(.x, .y){
            # If deaths
            if(outcome_metric == "deaths"){
              .x <- .x |>
                dplyr::select(.data = _, all_of(paste0("deaths_", year_of_analysis))) |>
                sum(na.rm = TRUE)
              return(.x)
            }




          # If yll or yld
          if(outcome_metric %in% c("yll", "yld")){

            # Only if yld
            if(outcome_metric %in% "yld"){
              .x <-
                .x |>
                # Filter for the relevant years
                dplyr::filter(.data = _, year < .y)
            }

            # Both yll and yld cases
            .x <-
              .x |>
              dplyr::summarise(impact = sum(impact, na.rm = TRUE)) |>
              dplyr::mutate(discounted = FALSE)

            return(.x)
          }
        }
      )
    )
    }


    # If a value for corrected_discount_rate was provided by the user,
    # apply discount
    if( !is.null(corrected_discount_rate) ){
    # if(corrected_discount_rate != 0) {
      discount_factor <- corrected_discount_rate + 1

      impact_detailed <-
        impact_detailed |>
        dplyr::mutate(
          impact_nest = purrr::pmap(
            list(.x = lifeyears_nest, .y = last_year + 1, .z = impact_nest),
            function(.x, .y, .z){
              ## Calculate total, discounted life years (single value) per sex & ci ####
              .x <-
                .x |>
                # Convert year to numeric
                dplyr::mutate(year = as.numeric(year)) |>
                # Calculate discount rate for each year
                dplyr::mutate(
                  discount = 1/(discount_factor^(year-(year_of_analysis+1)))) |>
                # Calculate life years discounted
                dplyr::mutate(
                  discounted_impact = impact * discount)

              # If yll or yld
              if(outcome_metric %in% c("yll", "yld")){

                # If "yld"
                if (outcome_metric == "yld") {
                  .x <- .x |>
                    # Filter for the relevant years
                    dplyr::filter(.data = _, year < .y)
                }

                # Both yll and yld cases
                .x <- .x |>
                  # Sum among years to obtain the total impact (single value)
                  dplyr::summarise(impact = sum(discounted_impact), .groups = "drop")
              }

              # Add a column to indicate that the impact is discounted
              .x <- .x |>
                dplyr::mutate(discounted = TRUE)

              # Bind rows to have both discounted and not discounted
              x_with_and_without_discount <-
                dplyr::bind_rows(.z, .x)

              return(x_with_and_without_discount)

            }
          ))}

    # Last preparation of the detailed results
    impact_detailed <-
      impact_detailed |>
      # Unnest the obtained impacts to integrated them the main tibble
      tidyr::unnest(impact_nest) |>
      # Add  metric
      dplyr::mutate(
        outcome_metric = outcome_metric)


    # IF DEATHS
    if(outcome_metric == "deaths"){
      impact_detailed <- impact_detailed |>
      dplyr::rename(impact = impact_nest)
    }



    # Obtain total rows (sum across sex)
    ## ONLY if not a lifetable calculation, which already have a "total" row
    if (FALSE == grepl("from_lifetable", unique(impact_detailed$health_metric))){ # Is TRUE only for non-lifetable calculations
    impact_detailed_total <-
      impact_detailed |>
      # Sum across sex adding total
      dplyr::group_by(.,
                      across(-c(sex, impact, contains("nest"))))|>
                      # across(all_of(intersect(c("geo_id_raw", "geo_id_aggregated",
                      #                           "discounted", "erf_ci"),
                      #                         names(.))))) |>
      dplyr::summarise(.,
                       across(.cols = c(impact), sum),
                       across(sex, ~"total"),
                       across(contains("nest"), ~list("total")),
                    .groups = "drop")

    # Bind the rows of impact_detailed and the totals
    impact_detailed <-
      dplyr::bind_rows(impact_detailed, impact_detailed_total)
    }

    # Create ID for the rows (will be used below)
    id_columns <-
      c("sex",
        "exposure_name",
        "geo_id_raw",
        "erf_ci", "bhd_ci", "exp_ci", "dw_ci", "cutoff_ci", "duration_ci")

    id_columns_in_df <-
      id_columns[id_columns %in% names(impact_detailed)]

    id <-
      purrr::pmap_chr(impact_detailed[id_columns_in_df],
                      ~paste(..., sep = "_"))

    # Name rows with the ids for better overview in Environment
    impact_detailed <-
      impact_detailed  |>
      dplyr::mutate(across(contains("_nest"),
                           ~set_names(.x,
                                      id)))


    # Get the main results starting from a detailed table of results
    impact_main <-
      impact_detailed |>
      dplyr::select(-contains("nest"))|>
      dplyr::filter(sex %in% "total")

    if ("duration_ci" %in% names(impact_main)){impact_main <- impact_main |> dplyr::filter(duration_ci %in% "central")}
    if ("dw_ci" %in% names(impact_main)){impact_main <- impact_main |> dplyr::filter(dw_ci %in% "central")}

    if (!is.null(corrected_discount_rate)) {
      impact_main <- impact_main |>
        dplyr::filter(discounted %in% TRUE)
    }

    # Classify results in main and detailed
    output <- list(main = impact_main,
                   detailed = list(step_by_step_from_lifetable = impact_detailed))



    return(output)


  }
