# Title and description

#' Calculation of Health Impacts
#'
#' Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3.
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param bhd Numeric value showing the baseline health data (incidence of the health outcome in the population),
#' @param crf_per Numeric value showing the increment of the concentration-response function in ug/m3 (usually 10 or 5)
#' @param crf_rescale_method String to choose among "linear" and "loglinear",
#' @param pollutant String with the name of the pollutant,
#' @param outcome_metric String with the name of the health outcome,
#' @param exp_info \code{Data frame} of one row showing extra information in columns. Default value = NULL,
#' @param cf_info \code{Data frame} of one row showing extra information in columns. Default value = NULL,
#' @param crf_info \code{Data frame} of three rows (mean, lower bound and upper bound) showing extra information in columns. Default value = NULL,
#' @param bhd_info \code{Data frame} of one row showing extra information in columns. Default value = NULL,
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
           pollutant, outcome_metric,
           exp_info = NULL,
           cf_info = NULL,
           crf_info = NULL,
           bhd_info = NULL){

    # Bind input data by category
    input_fun <- list()

    if(is.null(exp_info)){
      input_fun[["exp"]] <- data.frame(exp = exp)
    } else {
      # When there is an extra_info data frame
      # the values from argument should replace the values
      # of likely columns with same name
      input_fun[["exp"]] <- exp_info
      input_fun[["exp"]]$exp <- exp}

    if(is.null(cf_info)){
      input_fun[["cf"]] <- data.frame(cf = cf)
    } else {
      input_fun[["cf"]] <- cf_info
      input_fun[["cf"]]$cf <- cf}

    if(is.null(crf_info)){
      input_fun[["crf"]] <- data.frame(crf = crf)
    } else {
      input_fun[["crf"]] <- crf_info
      input_fun[["crf"]]$crf <- crf
      input_fun[["crf"]]$crf_per <- crf_per}

    if(is.null(bhd_info)){
      input_fun[["bhd"]] <- data.frame(bhd = bhd)
    } else {
      input_fun[["bhd"]] <- bhd_info
      input_fun[["bhd"]]$bhd <- bhd}

    # Add pollutant to all input data tables to provide a common key for joining
    # {{}} ensure that the value from the function argument is used
    # instead of from an existing column is used
    input_fun <-
      input_fun %>%
      purrr::map(~mutate(., pollutant = {{pollutant}}))

    # Join all input data together
    calculation <-
      dplyr::left_join(input_fun[["exp"]],
                       input_fun[["cf"]],
                       by = c("pollutant"),
                       suffix = c("_exp", "_cf"))%>%
      dplyr::left_join(.,
                       input_fun[["crf"]],
                       by = c("pollutant"),
                       suffix = c("", "_crf"))%>%
      dplyr::left_join(.,
                       input_fun[["bhd"]],
                       by = c("pollutant"),
                       suffix = c("", "_bhd"))%>%

      # Calculate crf estimate which corresponds to the exposure
      # depending on the method
      dplyr::mutate(crfConc = rescale_crf(crf = crf,
                                          exp = exp,
                                          cf = cf,
                                          crf_per = crf_per,
                                          method = {{crf_rescale_method}}),
                    crf_ci = ifelse(crf %in% min(crf), "low",
                                    ifelse(crf %in% max(crf), "high",
                                           "mean")),
                    crf_rescale_method = crf_rescale_method) %>%
      dplyr::mutate(ci = ifelse(duplicated(crf), "mean", ci))%>%

      # Calculate attributable fraction (AF) and impact
      dplyr::mutate(approach_id = paste0("singleValue_", crf_rescale_method),
                    af =  bestcost::get_paf(crfConc = crfConc),
                    impact = round(af * bhd, 0),
                    impact_metric = {{outcome_metric}},
                    pollutant = {{pollutant}})%>%
      # Order columns
      dplyr::select(everything(),
                    exp, cf, bhd, crf, crfConc, crf_per, crf_ci, crf_rescale_method,
                    af, impact, impact_metric, pollutant)

    return(calculation)
  }

