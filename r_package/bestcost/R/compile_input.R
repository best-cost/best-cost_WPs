# Title and description

#' Compile input

#' Compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param pop_exp \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories. The length of this input variable must be the same as "exp".
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the central estimate as well as the lower and upper bound of the confidence interval.
#' @param erf_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param bhd \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population).
#' @param info \code{String} showing additional information or id for the pollutant. The suffix "info" will be added to the column name. Default value = NULL.
#' @param min_age \code{Numberic value} of the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age \code{Numberic value} of the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old).
#' @param method \code{String} showing the calculation methods.

#' @return
#' This function returns a \code{data.frame} with all input data together
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
#' @export

compile_input <-
  function(
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = NULL,
           pop_exp = NULL,
           cutoff = NULL,
           rr_central, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           min_age = NULL,
           max_age = NULL,
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           info = NULL,
           method = NULL,
           disability_weight = NULL,
           duration = NULL){

    # Check input data ####
    stopifnot(exprs = {
      #length(exp) == length(prop_pop_exp)
      #is.null(min_age) == FALSE
      #is.null(max_age) == FALSE
    })


    # Input data in data frame ####

    # If the erf is defined by rr, increment, shape and cutoff

    if(is.null(erf_c_central)){
      # Input data in data frame ####
      # Compile rr data to assign categories
      erf_data <-
        # tibble instead of data.frame because tibble converts NULL into NA
        dplyr::tibble(
          erf_increment = erf_increment,
          erf_shape = erf_shape,
          cutoff = cutoff,
          rr_central = rr_central,
          rr_lower =  rr_lower,
          rr_upper = rr_upper)

    }else{ # If it is defined by the erf function
      erf_data <-
        # tibble instead of data.frame because tibble converts NULL into NA
        dplyr::tibble(
          erf_c_central = erf_c_central,
          erf_c_lower = erf_c_lower,
          erf_c_upper = erf_c_upper)

    }

    # Store the lentgh of the exposure distribution (to be used below)
    # Let's take the first element
    length_expDist <-
      ifelse(is.list(exp_central),
             length(exp_central[[1]]),
             length(exp_central))


    input <-
      # Build a tibble instead  a data.frame because tibble converts NULL into NA
      dplyr::tibble(
        # First compile input data that are only geo-dependent,
        # ie. those which require adjustment to have the same dimension
        # as those with multiple dimension because of exposure distribution
        # Let's use rep() to ensure that there is dimension match
        geo_id_raw = rep(geo_id_raw, each = length_expDist) ,
        geo_id_aggregated = rep(geo_id_aggregated, each = length_expDist),
        bhd_central = rep(unlist(bhd_central), each = length_expDist),
        bhd_lower = rep(unlist(bhd_lower), each = length_expDist),
        bhd_upper = rep(unlist(bhd_lower), each = length_expDist),
        # Second those variables that will have lenght = 1 (no problematic)
        disability_weight = disability_weight,
        duration = duration,
        # Finally, those variables that are multi-dimentional (exposure distribution)
        exp_central = unlist(exp_central),
        exp_lower = unlist(exp_lower),
        exp_upper = unlist(exp_upper),
        prop_pop_exp = unlist(prop_pop_exp),
        pop_exp = unlist(pop_exp)) %>%

      # Add rr with a cross join to produce all likely combinations
      dplyr::bind_cols(., erf_data) %>%
      # Add additional (meta-)information
      bestcost::add_info(df=., info=info) %>%
      # Information derived from input data
      dplyr::mutate(
        # Add age_max and age_min (not needed without life table)
        age_range = ifelse(!is.null(max_age) & is.null(min_age), paste0("below", max_age + 1),
                           ifelse(!is.null(min_age) & is.null(max_age), paste0("from", min_age),
                                  NA)),
        # Add the method that refer to the function
        method = method,
        exposure_type =
          ifelse((is.list(exp_central) &
                   unique(purrr::map_int(exp_central, function(x) length(x))) == 1) |
                   (is.vector(exp_central) &
                      length(exp_central) == 1),
                 "population_weighted_mean",
                 "exposure_distribution"))%>%
      # Remove all columns with all values being NA
      dplyr::select(where(~ !all(is.null(.))))%>%
      # Pivot longer to show all combinations of central, lower and upper estimate
      # (relevant for iteration)
      ## For exposure,
      tidyr::pivot_longer(.,
                          cols = starts_with("exp_"),
                          names_to = "exp_ci",
                          names_prefix = "exp_",
                          values_to = "exp") %>%
      ## Exposure response function &
      {if(is.null(erf_c_central))
        tidyr::pivot_longer(.,
                            cols = starts_with("rr_"),
                            names_to = "erf_ci",
                            names_prefix = "rr_",
                            values_to = "rr")
        else
          tidyr::pivot_longer(.,
                              cols = starts_with("erf_c_"),
                              names_to = "erf_ci",
                              names_prefix = "erf_c_",
                              values_to = "erf_c")}%>%
      ## Baseline health data
      {if(!is.null(bhd_central))
        tidyr::pivot_longer(.,
                            cols = starts_with("bhd_"),
                            names_to = "bhd_ci",
                            names_prefix = "bhd_",
                            values_to = "bhd") else .}



  }
