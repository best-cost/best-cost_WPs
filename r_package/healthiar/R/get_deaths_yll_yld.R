#' Get deaths, YLL or YLD
#'
#' @description Get attributable deaths, years of life lost or years lived with disability from lifetable
#' @inheritParams attribute
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
  function(pop_impact,
           input_with_risk_and_pop_fraction) {

    ## Define outcome_metric variable
    outcome_metric <- sub("_.*", "", unique(input_with_risk_and_pop_fraction$health_metric))

    # Determine default time horizon for YLL/YLD if not specified ##############
    if ( outcome_metric %in% c("yll", "yld")  &
         !"time_horizon" %in% names(input_with_risk_and_pop_fraction ) ) {

        time_horizon <- input_with_risk_and_pop_fraction %>%
          slice(1) %>%                      # Select the first row
          pull(lifetable_with_pop_nest) %>% # Extract the nested tibble column
          pluck(1) %>%                      # Get the tibble stored in the first element
          nrow()

        ## Add time_horizon to tibble
        input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
          mutate(time_horizon = time_horizon)

    }

    # browser()

    ## ALTERNATIVE CODE
    ## Filter for relevant ages
    impact_detailed <- pop_impact |>
      dplyr::mutate(
        outcome_metric = outcome_metric,
        lifeyears_nest =
          purrr::pmap(
            list(.x =  pop_impact_nest,
                 max_age = unique(max_age),
                 min_age = unique(min_age),
                 outcome_metric = unique(outcome_metric)),
            function(.x, max_age, min_age, outcome_metric){

              # Set values in upper triangle to NA ###############################
              ## NOTE: also removes newborns values

              if ( outcome_metric == "deaths" ) {

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

              if ( !is.null( max_age) ) {

                .x <-
                  dplyr::filter(.x, age <= max_age)
              }

              if ( !is.null( min_age ) ) {
                .x <-
                  dplyr::filter(.x, age >= min_age)
              }

              # Calculate YLL/YLD impact per year ################################

              if ( outcome_metric %in% c("yll", "yld") ) {

                .x <- .x |>
                  dplyr::select(contains("population_")) |>

                  ## Sum over ages (i.e. vertically)
                  ## only ages between "max_age" and "input_with_risk_and_pop_fraction |>  pull(min_age) |> first()" filtered for above
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
    if ( outcome_metric %in% "yld" ) {

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

    # Store total, YLL/YLD in YOA in column impact_nest #########
    ## Single number

    if ( outcome_metric %in% c("yll", "yld")){

      impact_detailed <- impact_detailed |>

        ## Add column for year of analysis
        dplyr::mutate(year_of_analysis = year_of_analysis) |>
        ## Add column for time horizon
        dplyr::mutate(time_horizon = input_with_risk_and_pop_fraction |>  pull(time_horizon) |> first()) |>
        ## Add column for last year of analysis
        dplyr::mutate(last_year = year_of_analysis + time_horizon - 1)

      ## Sum impacts
      impact_detailed <- impact_detailed |>

        dplyr::mutate(
          impact_nest = purrr::pmap(
            list(.x = lifeyears_nest, .y = last_year + 1, outcome_metric = unique(outcome_metric)),
            function(.x, .y, outcome_metric){

            ## If yll or yld
            if( outcome_metric %in% c("yld", "yll")){

              .x <-
                .x |>

                ## Select all years within time horizon
                dplyr::filter(.data = _, year < .y) |>

                ## Sum impact
                dplyr::summarise(impact = sum(impact, na.rm = TRUE))

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

    return(impact_detailed)

  }
