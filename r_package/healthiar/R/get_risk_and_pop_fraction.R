#' Get input data and PAF

#' @description Calculates the population attributable fraction (PAF) based on the input data and puts the results in additional columns joined to the input data frame.
#' @param input \code{Data frame} with the input data
#' @param pop_fraction_type \code{String} indicating the type of the population fraction. Options: "paf" or "pif"
#' @return
#' This function returns a \code{data.frame} with the input data adding a column for the population attributable fraction
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
#' @note Experimental function
#' @keywords internal

get_risk_and_pop_fraction <-
  function(input,
           pop_fraction_type){


    # Define helper function ###################################################

    # This function enables the collapse of the data frame to have only one row
    # The columns with the same values inside will be condensed: e.g. c(1,1,1) = 1
    # The values in columns with different values are pasted: e.g. c(1,2,3) = "1, 2, 3"
    # The variable columns_for_group refers to the column that is used to group the collapse
    # The variable sep refers to the string to be used to collapse different values
    collapse_df_by_columns <-
      function(df, columns_for_group, sep){

        columns_for_group_present <-
          columns_for_group[columns_for_group %in% names(df)]

        df <-
          df |>
          dplyr::group_by(across(all_of(columns_for_group_present)))|>
          dplyr::summarize(
            across(everything(),
                   ~ if (length(unique(.)) == 1) {
                     first(.)
                   } else {
                     paste(collapse = sep)}),
            .groups = "drop")
      }


    # Determine risk at observed exposures #####################################

    # Check if erf_eq is NULL before going into get_risk
    # Otherwise the variable is created without value and cannot be evaluated
    # We need to know erf_eq is NULL if statements within get_risk
    if ( !"erf_eq" %in% names(input) ) { erf_eq <- NULL }

    input_with_risk_and_pop_fraction <-
      input |>
      ## Add pop fraction type
      dplyr::mutate(pop_fraction_type = pop_fraction_type)

      ## If PAF
    if ( {{pop_fraction_type}} == "paf" ) {

      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        ## Obtain the relative risk for the relevant concentration
        rowwise() |>
        dplyr::mutate(rr_conc =
                        healthiar::get_risk(rr = rr,
                                           exp = exp,
                                           cutoff = cutoff,
                                           erf_increment = erf_increment,
                                           erf_shape = erf_shape,
                                           erf_eq = erf_eq)) |>
        dplyr::ungroup()

      ## If PIF
    } else {
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::rowwise() |>
        dplyr::mutate(rr_conc_1 =
                        healthiar::get_risk(rr = rr,
                                           exp = exp_1,
                                           cutoff = cutoff,
                                           erf_increment = erf_increment,
                                           erf_shape = erf_shape,
                                           erf_eq = erf_eq),
                      rr_conc_2 =
                        healthiar::get_risk(rr = rr,
                                           exp = exp_2,
                                           cutoff = cutoff,
                                           erf_increment = erf_increment,
                                           erf_shape = erf_shape,
                                           erf_eq = erf_eq)) |>
        dplyr::ungroup()
      }

    # * Correction for multiexposure  ###############################################
    if ( "approach_multiexposure" %in% names(input) ) {

      # * * Multiplicative approach ############################################

      if ( unique(input$approach_multiexposure) %in% "multiplicative" ) {

        ## In the multiplicative approach, relative risks have to be merged
        ## by multiplying across different exposures
        if({{pop_fraction_type}} == "paf"){
          input_with_risk_and_pop_fraction <-
            input_with_risk_and_pop_fraction |>
            # prod() multiplies all elements in a vector
            dplyr::mutate(
              rr_conc_before_multiplying = rr_conc,
              rr_conc = prod(rr_conc))

          } else { ## if PIF
          input_with_risk_and_pop_fraction <-
            input_with_risk_and_pop_fraction |>
            ## prod() multiplies all elements in a vector
            dplyr::mutate(
              rr_conc_1_before_multiplying = rr_conc_1,
              rr_conc_2_before_multiplying = rr_conc_2,
              rr_conc_1 = prod(rr_conc_1),
              rr_conc_2 = prod(rr_conc_2))
          }

        ## Data wrangling for multiple exposures
        ## Collapse data frame pasting the columns with different values
        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          dplyr::mutate(exposure_name = paste(unique(exposure_name), collapse = ", ")) |>
          collapse_df_by_columns(
            columns_for_group = c(
              "geo_id_raw",
              "sex",
              "lifetable_with_pop_nest",
              "erf_ci",
              "exp_ci",
              "bhd_ci",
              "cutoff_ci",
              "dw_ci",
              "duration_ci",
              "erf_eq"
              ),
            sep = ", ")
      }
    }

    # Calculate PAF/PIF ########################################################

    ## Calculate population (attributable or impact) fraction (PAF or PIF)
    likely_columns_to_group_input <-
      c("geo_id_raw",
        "exposure_name",
        "exp_ci",
        "erf_ci",
        "cutoff_ci")

    available_columns_to_group_input <-
      likely_columns_to_group_input[likely_columns_to_group_input %in%
                                      names(input_with_risk_and_pop_fraction)]



    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>

      ## Group by exp_ci and cutoff_ci in case that there are different exposure or cutoff categories
      dplyr::group_by(across(all_of(available_columns_to_group_input)))
      ## Alternative coding if one of the grouping variables is NULL
      ## dplyr::group_by(across(all_of(intersect(c("geo_id_raw", "exp_ci", "erf_ci"), names(input))))) |>
      ## Alternative coding with if statement within group_by
      ## Using if statement as below because otherwise (e.g. with if_else or case_when)
      ## the FALSE condition is evaluate and results in an error because the names do not match
      ## dplyr::group_by(if("geo_id_raw" %in% names(input)){geo_id_raw}, exp_ci, erf_ci)|>

    # * PAF ####################################################################

    if ( {{pop_fraction_type}} == "paf" ) {

      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(
          pop_fraction =
            healthiar::get_pop_fraction(rr_conc_1 = rr_conc,
                                       rr_conc_2 = 1,
                                       prop_pop_exp_1 = prop_pop_exp,
                                       prop_pop_exp_2 = prop_pop_exp))

    # * PIF ####################################################################

      } else {
        input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(
          pop_fraction =
            healthiar::get_pop_fraction(rr_conc_1 = rr_conc_1,
                                       rr_conc_2 = rr_conc_2,
                                       prop_pop_exp_1 = prop_pop_exp_1,
                                       prop_pop_exp_2 = prop_pop_exp_2)) }

    ## Ungroup
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::ungroup()

    # * Correction for multiexposure ###########################################

    if("approach_multiexposure" %in% names(input)){
      if(unique(input$approach_multiexposure) %in% "combined"){

        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          dplyr::mutate(pop_fraction_before_combining = pop_fraction,
                        ## Multiply with prod() across all pollutants
                        pop_fraction = 1-(prod(1-pop_fraction)))


        ## Data wrangling for multiple exposures
        ## Collapse data frame pasting the columns with different values
        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          dplyr::mutate(exposure_name = paste(unique(exposure_name), collapse = ", ")) |>
          collapse_df_by_columns(columns_for_group = c(
            "geo_id_raw",
            "sex",
            "lifetable_with_pop_nest",
            "erf_ci",
            "exp_ci",
            "bhd_ci",
            "cutoff_ci",
            "dw_ci",
            "duration_ci",
            "erf_eq"),
            sep = ", ")
        }
      }

    # Prepare output ###########################################################

    ## Only if exposure distribution (multiple exposure categories)
    ## then reduce the number of rows to keep the same number as in rr
    if(unique(input$exposure_type) == "exposure_distribution"){
    input_with_risk_and_pop_fraction <-
      collapse_df_by_columns(df = input_with_risk_and_pop_fraction,
                             columns_for_group = c(
                               "geo_id_raw",
                               "exposure_name",
                               "sex",
                               "lifetable_with_pop_nest",
                               "erf_ci",
                               "exp_ci",
                               "bhd_ci",
                               "cutoff_ci",
                               "dw_ci",
                               "duration_ci",
                               "erf_eq"),
                             sep = ", ")
    }

    return(input_with_risk_and_pop_fraction)

  }
