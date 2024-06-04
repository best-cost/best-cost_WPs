# Title and description

#' Compare attributable health cases based on absolute risk

#' Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments). It provides as a result the central estimate as well as the lower and the higher bound of the confidence interval based on the uncertainty of the exposure-response function.
#' @param exp_1 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 1.
#' @param exp_2 \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution in the scenario 2.
#' @param pop_exp_1 \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories in the scenario 1. The length of this input variable must be the same as "exp".
#' @param pop_exp_1 \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories in the scenario 1. The length of this input variable must be the same as "exp".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param info_1 \code{String} or {data frame} showing additional information or id of the scenario 1. The suffix "info" will be added to the column name. Default value = NULL.
#' @param info_2 \code{String} or {data frame} showing additional information or id of the scenario 1. The suffix "info" will be added to the column name. Default value = NULL.

#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
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
compare_health_ar <-
  function(exp_central_1, exp_lower_1 = NULL, exp_upper_1 = NULL,
           exp_central_2, exp_lower_2 = NULL, exp_upper_2 = NULL,
           pop_exp_1, pop_exp_2,
           erf_c_central, erf_c_lower = NULL, erf_c_upper = NULL,
           info_1 = NULL, info_2 = NULL){



    # Calculate attributable health impacts in the scenario 1
    att_health_1 <-
      bestcost::attribute_health_singlebhd_ar(
        exp_central = exp_central_1,
        pop_exp = pop_exp_1,
        erf_c_central = erf_c_central,
        info = info_1)

    # Calculate attributable health impacts in the scenario 2
    att_health_2 <-
      bestcost::attribute_health_singlebhd_ar(
        exp_central = exp_central_2,
        pop_exp = pop_exp_2,
        erf_c_central = erf_c_central,
        info = info_2)

    # Identify the columns that are common for scenario 1 and 2
    # grepl instead of %in% because there might be differnt info columns starting with info_
    joining_columns <-
      names(att_health_1[["main"]])[! grepl(c("exp|rr_conc|absolute_risk_as_percent|population_affected|impact|impact_rounded|info"),
                                             names(att_health_1[["main"]]))]

    # Merge the result tables by common columns
    att_health <-
      dplyr::left_join(
        att_health_1[["main"]],
        att_health_2[["main"]],
        by = joining_columns,
        suffix = c("_1", "_2"))%>%
      # Calculate the delta (difference) between scenario 1 and 2
      dplyr::mutate(impact = impact_1 - impact_2)

    # No PIF option because the absolute risk does not have population fraction
    # So only delta option

      # Round results
      att_health <-
        att_health %>%
        mutate(impact_rounded = round(impact, 0))

   output <- list(main = att_health,
                  detailed = list(scenario_1 = att_health_1,
                                  scenario_2 = att_health_2))


    return(output)
  }

