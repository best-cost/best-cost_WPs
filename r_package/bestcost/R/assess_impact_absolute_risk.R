# Title and description

#' Health impacts based on single baseline health value
#'
#' Calculates the health impacts, of an environmental stressor (e.g. noise) using the absolute risk instead of the relative risk
#' @param exp \code{Vector} showing the exposure categories (average of the exposure ranges) in a exposure distribution refering only to exposed population. The length of exp and pop_exp should be the same.
#' @param pop_exp \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories. The length of this input variable should be the same as "exp".
#' @param erf_shape \code{String} to choose among "linear", "loglinear" and "quadratic".
#' @param erf_parameters \code{Vector} of numeric values as in order of apereance in the exposure response function.
#' @param info_pollutant \code{String} showing additional information or id for the pollutant. Default value = NULL.
#' @param info_outcome \code{String} showing additional information or id for the health outcome. Default value = NULL.
#' @param info_exp \code{String} showing additional information or id for the exposure. This information will be added to all rows of the results. Default value = NULL.
#' @param info_ar \code{String} showing additional information or id for the concentration-response function. This information will be added to all rows of the results. Default value = NULL.
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. mean, lower and upper bound confidence interval.
#' Moreover, the data frame include columns such as:
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
assess_impact_absolute_risk <-
  function(exp,
           pop_exp,
           erf_shape,
           erf_parameters,
           info_pollutant = NULL,
           info_outcome = NULL,
           info_exp = NULL,
           info_cf = NULL,
           info_erf = NULL){

    # First compile crf data to assign categories
    if(erf_shape == "quadratic"){
      erf <- function(exp){
        erf_parameters[1] + erf_parameters[2]*exp + erf_parameters[3]*exp^2
      }
    }



    # Input data in data frame
    input <-
      data.frame(
        exp = exp,
        pop_exp = pop_exp,
        erf_shape = erf_shape,
        erf_parameters = paste(erf_parameters, collapse = ", "),
        approach_id = paste0("absolute risk")) %>%
      # Add additional information (info_x variables)
      dplyr::mutate(
        info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
        info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
        info_exp = ifelse(is.null(info_exp), NA, info_exp),
        info_crf = ifelse(is.null(info_crf), NA, info_crf),
        info_bhd = ifelse(is.null(info_bhd), NA, info_bhd))

    output <-
      input %>%
      # Calculate absolute risk for each exposure category
      dplyr::mutate(
        absolute_risk_as_percent = erf(exp),
        population_affected = absolute_risk_as_percent/100 * pop_exp,
        population_affected_rounded = round(population_affected, 0)) %>%
      dplyr::bind_rows(
        summarise(.,
                  across(absolute_risk_as_percent, population_affected, pop_exp,
                         sum),
                  across(exp, paste(., collapse = ", "))))




    return(output)
  }
