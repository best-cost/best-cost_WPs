# Title and description

#' Calculation of Health Impacts
#'
#' Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3.
#' @param cf \code{Numeric value} showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param bhd \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population).
#' @param crf_per \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param crf_rescale_method \code{String} to choose among "linear" and "loglinear".
#' @param info_pollutant \code{String} showing additional information or id for the pollutant. Default value = NULL.
#' @param info_outcome \code{String} showing additional information or id for the health outcome. Default value = NULL.
#' @param info_exp \code{String} showing additional information or id for the exposure. This information will be added to all rows of the results. Default value = NULL.
#' @param info_cf \code{String} showing additional information or id for counter-factual scenario (cut-off). This information will be added to all rows of the results. Default value = NULL.
#' @param info_crf \code{String} showing additional information or id for the concentration-response function. This information will be added to all rows of the results. Default value = NULL.
#' @param info_bhd \code{String} showing additional information or id for the baseline health data. This information will be added to all rows of the results. Default value = NULL.
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
assess_impact_single <-
  function(exp, cf, crf, bhd,
           crf_per, crf_rescale_method,
           info_pollutant = NULL,
           info_outcome = NULL,
           info_exp = NULL,
           info_cf = NULL,
           info_crf = NULL,
           info_bhd = NULL){

    # Input data in data frame
    input <-
      data.frame(
        crf = crf,
        exp = exp,
        cf = cf,
        bhd = bhd,
        crf_per = crf_per,
        crf_rescale_method = crf_rescale_method,
        approach_id = paste0("lifetable_", crf_rescale_method)) %>%
      # Add additional information (info_x variables)
      dplyr::mutate(
        info_pollutant = ifelse(is.null(info_pollutant), NA, info_pollutant),
        info_outcome = ifelse(is.null(info_outcome), NA, info_outcome),
        info_exp = ifelse(is.null(info_exp), NA, info_exp),
        info_cf = ifelse(is.null(info_cf), NA, info_cf),
        info_crf = ifelse(is.null(info_crf), NA, info_crf),
        info_bhd = ifelse(is.null(info_bhd), NA, info_bhd))


    # Calculate crf estimate which corresponds to the exposure
    # depending on the method
    calculation <-
      input %>%
      dplyr::mutate(
        crf_forPaf = rescale_crf(crf = crf,
                              exp = exp,
                              cf = cf,
                              crf_per = crf_per,
                              method = {{crf_rescale_method}}
                              #{{}} ensures that the
                              # value from the function argument is used
                              # instead of from an existing column
                              ),
        crf_ci = ifelse(crf %in% min(crf), "low",
                        ifelse(crf %in% max(crf), "high",
                               "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(crf), "mean", ci)) %>%


      # Calculate population attributable fraction (PAF) as well as impact
      dplyr::mutate(paf =  bestcost::get_paf(crf_conc = crf_forPaf),
                    impact = round(paf * bhd, 0)) %>%
      # Order columns
      dplyr::select(exp, cf, bhd,
                    crf, crf_forPaf, crf_per, ci, crf_rescale_method,
                    paf, impact,
                    starts_with("info_"))


    return(calculation)
  }

