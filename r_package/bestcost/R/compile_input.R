# Title and description

#' Compile input

#' Compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param pop_exp \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories. The length of this input variable must be the same as "exp".
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the central estimate as well as the lower and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
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
  function(exp,
           prop_pop_exp = NULL,
           pop_exp = NULL,
           cutoff = NULL,
           rr = NULL,
           rr_increment = NULL,
           erf_shape = NULL,
           erf_c = NULL,
           bhd = NULL,
           min_age = NULL,
           max_age = NULL,
           info = NULL,
           method = NULL,
           disability_weight = NULL,
           duration = NULL){
    # Check input data ####
    stopifnot(exprs = {
      #length(exp) == length(prop_pop_exp)
      is.na(min_age) == FALSE
      is.na(max_age) == FALSE
    })


    # Input data in data frame ####

    # If the erf is defined by rr, increment, shape and cutoff

    if(is.null(erf_c)){
      # Input data in data frame ####
      # Compile rr data to assign categories
      erf_data <-
        data.frame(
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          cutoff = cutoff,
          rr = rr)
    }

    # If it is defined by the erf function
    if(!is.null(erf_c)){
      erf_data <-
        data.frame(
          erf_c = erf_c,
          # Check which risk of the vector is higher for an exp=10
          # (random value, it could be another one)
          erf_10 = c(bestcost::get_risk(exp = 10, erf_c = erf_c[1], erf_full = TRUE),
                      bestcost::get_risk(exp = 10, erf_c = erf_c[2], erf_full = TRUE),
                      bestcost::get_risk(exp = 10, erf_c = erf_c[3], erf_full = TRUE)),
          rr = erf_10)

    }


    erf_data <-
      erf_data %>%
      mutate(
        # Assign central estimate as well as lower and upper bound of rr values
        erf_ci = ifelse(rr %in% min(rr), "lower",
                       ifelse(rr %in% max(rr), "upper",
                              "central"))) %>%

      # In case of same value in mean and low and/or high, assign value randomly
      {if(sum(duplicated(.$rr))==1) dplyr::mutate(.,
                                                erf_ci = ifelse(duplicated(rr),
                                                               "central",
                                                               erf_ci))
        else .} %>%

      {if(sum(duplicated(.$rr))==2) dplyr::mutate(.,
                                                erf_ci = c("central", "lower", "upper"))
        else .}



    # Compile input data except meta-info
    input <-
      data.frame(
        exp = exp)%>%
      # If variable is not NULL,
      # add it to the data frame. Otherwise, leave it out.
      {if(!is.null(bhd)) mutate(., bhd = bhd) else .} %>%
      {if(!is.null(prop_pop_exp)) mutate(., prop_pop_exp = prop_pop_exp) else .} %>%
      {if(!is.null(pop_exp)) mutate(., pop_exp = pop_exp) else .} %>%
      {if(!is.null(disability_weight)) mutate(., disability_weight = disability_weight) else .} %>%
      {if(!is.null(duration)) mutate(., duration = duration) else .} %>%

      # Add rr with a cross join to produce all likely combinations
      dplyr::cross_join(., erf_data) %>%
      # Add additional (meta-)information
      bestcost::add_info(df=., info=info) %>%
      # Information derived from input data
      dplyr::mutate(
        # Add age_max and age_min (not needed without life table)
        age_range = ifelse(!is.null(max_age) & is.null(min_age), paste0("below", max_age + 1),
                           ifelse(!is.null(min_age) & is.null(max_age), paste0("from", min_age),
                                  NA)),
        # Add the method that refer to the function
        method = method)

  }
