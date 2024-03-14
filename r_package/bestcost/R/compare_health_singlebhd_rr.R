# Title and description

#' Compare attributable health cases based

#' Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @param comparison_method \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @param exp_1 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 1.
#' @param exp_2 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 2.
#' @param prop_pop_exp \code{Numeric value} or {vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param bhd_1 \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population) in the scenario 1.
#' @param bhd_2 \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population) in the scenario 1.
#' @param info_1 \code{String} or {data frame} showing additional information or id of the scenario 1. The suffix "info" will be added to the column name. Default value = NULL.
#' @param info_2 \code{String} or {data frame} showing additional information or id of the scenario 1. The suffix "info" will be added to the column name. Default value = NULL.

#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
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
compare_health_singlebhd_rr <-
  function(comparison_method = "delta",
           exp_1, exp_2,
           prop_pop_exp_1 = 1, prop_pop_exp_2 = 1,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           erf_c = NULL,
           bhd_1, bhd_2,
           info_1 = NULL, info_2 = NULL){


    # Calculate attributable health impacts in the scenario 1
    att_health_1 <-
      bestcost::attribute_health_singlebhd_rr(
        exp = exp_1,
        prop_pop_exp = prop_pop_exp_1,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        bhd = bhd_1,
        info = info_1)

    # Calculate attributable health impacts in the scenario 2
    att_health_2 <-
      bestcost::attribute_health_singlebhd_rr(
        exp = exp_2,
        prop_pop_exp = prop_pop_exp_2,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_c = erf_c,
        bhd = bhd_2,
        info = info_2)


    # If "delta" is chosen as comparison method
    if(comparison_method == "delta"){

      joining_variables <-
        names(att_health_1[["total"]])[! names(att_health_1[["total"]]) %in%
                                  c("exp", "bhd", "rr_forPaf", "paf", "impact", "impact_rounded", "info")]
      binding_variables <-  c("exp", "bhd", "rr_forPaf", "paf", "impact", "impact_rounded", "info")

      # Merge the result tables by common columns
      att_health <-
        dplyr::left_join(
          att_health_1[["total"]],
          att_health_2[["total"]],
          by = joining_variables,
          suffix = c("_1", "_2"))%>%
        # Calculate the delta (difference) between scenario 1 and 2
        dplyr::mutate(impact = impact_1 - impact_2,
                      impact_rounded = round(impact, 0))

    }



   output <- list(total = att_health,
                  detailed = list(scenario_1 = att_health_1,
                                  scenario_2 = att_health_2))


    return(output)
  }

