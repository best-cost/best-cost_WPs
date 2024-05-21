# Title and description

#' Attributable health cases based on absolute risk
#'
#' Calculates the health impacts, of an environmental stressor (e.g. noise) using the absolute risk instead of the relative risk
#' @param exp \code{Vector} showing the mid-point exposure in the exposure categories (average of the exposure ranges) in a exposure distribution referring only to the exposed population. The length of exp and pop_exp must be the same.
#' @param pop_exp \code{Numeric value} or {vector} showing the population exposed for each of the exposure categories. The length of this input variable must be the same as "exp".
#' @param erf_c \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @param info \code{String} or {data frame} showing additional information or id. The suffix "info" will be added to the column name. Default value = NULL.

#' @return
#' TBD. E.g. This function returns a \code{list} with two \code{data.frames}, one with the total health impact and the second one with a row for each category of the exposure distribution.
#' The data frame include columns such as:
#' \itemize{
#'  \item TBD
#' }
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export
#'
#'
attribute_health_singlebhd_ar <-
  function(exp,
           pop_exp,
           erf_c,
           info = NULL){

    # Check input data ####
    # TBA: length(exp) == length(pop_exp)

    # Compile input data except erf data
    input <-
      bestcost::compile_input(
        exp = exp,
        pop_exp = pop_exp,
        prop_pop_exp = NULL,
        cutoff = NULL,
        rr = NULL,
        erf_increment = NULL,
        erf_shape = NULL,
        erf_c = erf_c,
        bhd = NULL,
        min_age = NULL,
        max_age = NULL,
        info = info,
        method = "absolute_risk",
        disability_weight = NULL,
        duration = NULL)%>%
      # Remove erf_10 and rr
      # They were added to identify what is central, lower and upper
      # but not needed anymore
      dplyr::select(-rr, -erf_10)



    # Calculate absolute risk for each exposure category ####
    output_byExposureCategory <-
      input %>%
      dplyr::mutate(
        absolute_risk_as_percent = get_risk(exp = exp, erf_c = erf_c, erf_full = TRUE) ,
        population_affected = absolute_risk_as_percent/100 * pop_exp,
        population_affected_rounded = round(population_affected, 0))

    output_total <-
      output_byExposureCategory %>%
      dplyr::mutate(exp = paste(exp, collapse = ", ")) %>%
      dplyr::group_by(exp,
                      erf_ci,
                      erf_c,
                      method,
                      across(starts_with("info"))) %>%
      dplyr::summarize(
        across(c(pop_exp, absolute_risk_as_percent, population_affected),
               sum),
               .groups = "drop") %>%
      dplyr::mutate(impact = population_affected,
                    impact_rounded = round(impact, 0))

    output <-
      list(total = output_total,
           detailed = list(by_exp_category = output_byExposureCategory))

    return(output)
  }
