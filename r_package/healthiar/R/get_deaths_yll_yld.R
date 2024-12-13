#' Get deaths, YLL or YLD
#'
#' @description Get attributable deaths, years of life lost or years lived with disability from lifetable
#' @inheritParams attribute
#' @param outcome_metric \code{String} with the metric of the health outcome to be assessed. Options: "deaths", "yll" or "yld".
#' @return
#' This function returns a \code{List}
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest
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
           time_horizon,
           min_age = NULL,
           max_age = NULL,
           input_with_risk_and_pop_fraction,
           corrected_discount_rate = NULL,
           discount_shape = NULL){

    # browser()

    # Determine default time horizon for YLL/YLD if not specified ##############
    if ( {{outcome_metric}} %in% c("yll", "yld")  &
         is.null(time_horizon) ) {

        time_horizon <- input_with_risk_and_pop_fraction %>%
          slice(1) %>%                      # Select the first row
          pull(lifetable_with_pop_nest) %>% # Extract the nested tibble column
          pluck(1) %>%                      # Get the tibble stored in the first element
          nrow()

      }


    # Filter for relevant ages
    impact_detailed <- pop_impact |>
      dplyr::mutate(
        outcome_metric = {{outcome_metric}},
        lifeyears_nest =
          purrr::map(
            .x =  pop_impact_nest,
            function(.x){

            # Set values in upper triangle to NA ###############################
            ## NOTE: also removes newborns values

            if({{outcome_metric}} == "deaths" ) {

              .x <- .x |>
              dplyr::mutate(
                across(contains("deaths"), ~ {
                  mat <- as.matrix(.x)
                  mat[upper.tri(mat, diag = FALSE)] <- NA
                  return(mat)}))

            } else {

              .x <- .x |>
              dplyr::mutate(
                across(contains("population"),
                       ~ {mat <- as.matrix(.x)
                       mat[upper.tri(mat, diag = FALSE)] <- NA
                       return(mat)}))
            }


            # Filter for relevant ages #########################################
            # use {{}} to refer to the argument and avoid warnings

            if ( !is.null( {{max_age}} ) ) {
              .x <-
                dplyr::filter(.x, age <= {{max_age}})
            }

            if ( !is.null( {{min_age}} ) ) {
              .x <-
                dplyr::filter(.x, age >= {{min_age}})
            }

            # Calculate YLL/YLD impact per year ################################

            if ({{outcome_metric}} %in% c("yll", "yld") ) {

              .x <- .x |>
                dplyr::select(contains("population_")) |>

            ## Sum over ages (i.e. vertically)
            ## only ages between "max_age" and "min_age" filtered for above
            dplyr::summarize_all(sum, na.rm = TRUE) |>

            ## Reshape to long format
            ## (output is data frame with 2 columns "year" & "impact")
            tidyr::pivot_longer(cols = starts_with("population_"),
                                names_to = "year",
                                values_to = "impact",
                                names_prefix = "population_") |>

            ## Convert year to numeric
            dplyr::mutate(year = as.numeric(year))
            } else
              .x<-.x}), .before = 1)



    # YLD ######################################################################

    ## Determine year- and age specific YLD
    if ({{outcome_metric}} %in% "yld" ) {

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

      ## Determine total YLD per year
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


    # Deaths ###################################################################

    ## Store total deaths in YOA in column impact_nest
    if ( outcome_metric == "deaths" ) {

      impact_detailed <- impact_detailed |>
        # Store in new column "impact_nest"
        dplyr::mutate(
          impact = purrr::map(
            .x = lifeyears_nest,
            function(.x){

              .x <- .x |>
                dplyr::select(.data = _, all_of(paste0("deaths_", year_of_analysis))) |>
                sum(na.rm = TRUE)
              return(.x)
            }
          )
        ) |>
        dplyr::mutate(impact = as.numeric(impact))


    }

    # Store total, not discounted YLL/YLD in YOA in column impact_nest #########
    ## Single number

    if ( {{outcome_metric}} %in% c("yll", "yld")){

      impact_detailed <- impact_detailed |>

        ## Add column for year of analysis
        dplyr::mutate(year_of_analysis = year_of_analysis) |>
        ## Add column for time horizon
        dplyr::mutate(time_horizon = time_horizon) |>
        ## Add column for last year of analysis
        dplyr::mutate(last_year = year_of_analysis + time_horizon - 1)

      ## Sum impacts
      impact_detailed <- impact_detailed |>

        dplyr::mutate(
          impact_nest = purrr::pmap(
            list(.x = lifeyears_nest, .y = last_year + 1),
            function(.x, .y){

            ## IF EVERYTHING RUNS SMOOTHLY, DELETE THIS CODE BLOCK ####
            ## If deaths
            # if(outcome_metric == "deaths"){
            #
            #   .x <- .x |>
            #     dplyr::select(.data = _, all_of(paste0("deaths_", year_of_analysis))) |>
            #     sum(na.rm = TRUE)
            #   return(.x)
            #
            #   }

            # If yll or yld
            if({{outcome_metric}} %in% c("yld", "yll")){

              .x <-
                .x |>

                ## Select all years within time horizon
                dplyr::filter(.data = _, year < .y) |>

                ## Sum impact
                dplyr::summarise(impact = sum(impact, na.rm = TRUE))|>
                dplyr::mutate(discounted = FALSE)

              return(.x)
            }
          }
        )
      ) |>
        dplyr::mutate(
          impact_for_discounting_nest = impact_nest
        ) |>
        ## Unnest the obtained impacts to integrate them the main tibble
        ## Impact saved in column impact
        tidyr::unnest(impact_nest)

    }


    # Assign ID ###################################

    ## Create ID for the rows (will be used below)
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

    ## Name rows with the ids for better overview in Environment
    impact_detailed <-
      impact_detailed  |>
      dplyr::mutate(across(contains("_nest"),
                           ~set_names(.x,
                                      id)))



    # Discount #################################################################

    if ( !is.null(corrected_discount_rate) ) {
    # if(corrected_discount_rate != 0) { # If everything runs without this line, delete ####

      discount_factor <- corrected_discount_rate + 1

      impact_detailed <-
        impact_detailed |>

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
                  impact_after_discount = impact * discount_factor)


              ## If yll or yld
              if({{outcome_metric}} %in% c("yll", "yld")){

                lifeyear_nest_with_discount <-
                  ## Filter for the relevant years
                  dplyr::filter(.data = lifeyear_nest_with_discount,
                                year < .y) |>
                  ## Sum among years to obtain the total impact (single value)
                  dplyr::summarise(impact = sum(impact_after_discount), .groups = "drop")
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
        dplyr::select(-any_of(c("impact", "discounted"))) |>
        ## Unnest the obtained impacts to integrate them the main tibble
        ## Impact saved in column impact
        tidyr::unnest(impact_with_discount_nest)



    }





    # Obtain total rows (sum across sex) #######################################

    ## ONLY if not a lifetable calculation, which already have a "total" row

    #if ( FALSE == grepl("from_lifetable", unique(impact_detailed$health_metric) ) ) { # Is TRUE only for non-lifetable calculations

    #impact_detailed_total <-
    #  impact_detailed |>
    #  ## Sum across sex adding total
    #  dplyr::group_by(.,
    #                  across(-c(sex, impact, contains("nest"))))|>
    #                  # If everything runs smoothly wihtout this lines, delete
    #                  # across(all_of(intersect(c("geo_id_raw", "geo_id_aggregated",
    #                  #                           "discounted", "erf_ci"),
    #                  #                         names(.))))) |>
    #  dplyr::summarise(.,
    #                   across(.cols = c(impact), sum),
    #                   across(sex, ~"total"),
    #                   across(contains("nest"), ~list("total")),
    #                .groups = "drop")
    #
    ### Bind the rows of impact_detailed and the totals
    #impact_detailed <-
    #  dplyr::bind_rows(impact_detailed, impact_detailed_total)
    #}



    # Get main results from detailed results ###################################


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

    ## Classify results in main and detailed
    output <- list(health_main = impact_main,
                   health_detailed = list(step_by_step_from_lifetable = impact_detailed))



    return(output)


  }
