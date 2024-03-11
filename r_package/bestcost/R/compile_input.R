# Title and description

#' Compile input

#' Compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)
#' @param prop_pop_exp \code{Numeric value} or {vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
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
           prop_pop_exp,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           erf_c,
           bhd,
           min_age = NULL,
           max_age = NULL,
           info,
           method){
    # Check input data ####
    # TBA: checks

    # Input data in data frame ####
    # Compile rr data to assign categories
    erf_data <-
      data.frame(
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        cutoff = cutoff,
        rr = rr,
        # Assign mean, low and high rr values
        rr_ci = ifelse(rr %in% min(rr), "low",
                       ifelse(rr %in% max(rr), "high",
                              "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) %>%

      # If erf_c is not NULL add it to the data frame. Otherwise, leave it out
      {if(!is.null(erf_c)) mutate(., erf_c = erf_c) else .}

    # Compile input data except meta-info
    input <-
      data.frame(
        exp = exp,
        prop_pop_exp = prop_pop_exp)%>%
      # If bhd is not NULL, add it to the data frame. Otherwise (e.g. in life table approach), leave it out.
      {if(!is.null(bhd)) mutate(., bhd = bhd) else .} %>%

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
