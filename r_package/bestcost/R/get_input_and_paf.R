# Title and description

#' Get input data and PAF

#' Compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)
#' @param prop_pop_exp \code{Numeric value} or {vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} to choose among "linear" and "loglinear".
#' @param bhd \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population).
#' @param info \code{String} showing additional information or id for the pollutant. The suffix "info" will be added to the column name. Default value = NULL.

#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. mean, lower and upper bound confidence interval.
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
attribute_health_singlebhd_rr <-
  function(exp,
           prop_pop_exp = 1,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           bhd,
           info = NULL){
