# Title and description

#' Determine years lived with disability (YLD) attributable to the incidence of a specific morbidity health outcome

#' Calculates the YLDs using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bounds of the morbidity impact based on the confidence interval of the exposure-response function. Assumption: cases happen at the start of the year.
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or \code{vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or \code{Numeric vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input must match that of "exp". By default, 1 if a single exposure value is inputted
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Numeric vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} to choose among "linear" and "loglinear".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param bhd \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population).
#' @param disability_weight \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param info \code{String} showing additional information or id for the pollutant. The suffix "info" will be added to the column name. Default value = NULL.
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. mean, lower and upper bound confidence interval.
#' The YLDs are listed in the columns:
#' \itemize{
#'  \item yld
#'  \item yld_rounded
#'  }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Axel Luyten
#' @note Experimental function
#' @export
attribute_yld_singlebhd_rr <-
  function(exp,
           prop_pop_exp = 1,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           erf_c,
           bhd,
           disability_weight,
           info = NULL){

    # Check input data ####
    stopifnot(exprs = {
      length(exp) == length(prop_pop_exp)
    })

    # Call attribute_health_singlebhd_rr
    dat <- attribute_health_singlebhd_rr(
      exp = exp,
      prop_pop_exp = prop_pop_exp,
      cutoff = cutoff,
      rr = rr,
      rr_increment = rr_increment,
      erf_shape = erf_shape,
      bhd = bhd,
      info = info)[["total"]]

    # Calculate YLD and add as column
    dat <-
      dat %>%
      dplyr::mutate(impact = paf * bhd * disability_weight,
                    impact_rounded = round(impact, 0))
    #%>%
      #dplyr::select(yld, yld_rounded, impact_rounded, exp, exp_mean, cutoff, bhd,
                    #rr, rr_forPaf, rr_increment, ci, erf_shape,
                    #paf, prop_pop_exp,
                    #starts_with("info_")) %>%
      #dplyr::relocate(yld_rounded, yld, prop_pop_exp, .before = impact_rounded)

    output <- list(total = dat)

    return(output)

  }
