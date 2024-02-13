# Title and description

#' Calculation of Health Impacts
#'
#' Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {vector] showing the proportion of population exposed (as fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cf \code{Numeric value} showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param crf_per \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param crf_rescale_method \code{String} to choose among "linear" and "loglinear".
#' @param bhd \code{Numeric value} showing the baseline health data (incidence of the health outcome in the population).
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
  function(exp, prop_pop_exp = 1,
           cf,
           crf,
           crf_per, crf_rescale_method,
           bhd,
           info_pollutant = NULL,
           info_outcome = NULL,
           info_exp = NULL,
           info_cf = NULL,
           info_crf = NULL,
           info_bhd = NULL){

    # First compile crf data to clean up them
    crf_data <-
      data.frame(
        crf = crf,
        crf_per = crf_per,
        crf_rescale_method = crf_rescale_method,
        crf_ci = ifelse(crf %in% min(crf), "low",
                        ifelse(crf %in% max(crf), "high",
                               "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(crf), "mean", ci))
      # Assign mean, low and high crf values


    # Input data in data frame
    input <-
      data.frame(
        exp = exp,
        prop_pop_exp = prop_pop_exp,
        cf = cf,
        bhd = bhd,
        approach_id = paste0("lifetable_", crf_rescale_method)) %>%
      # Add crf with a cross join to produce all likely combinations
      dplyr::cross_join(., crf_data) %>%
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
        crf_forPaf =
          rescale_crf(crf = crf,
                      exp = exp,
                      cf = cf,
                      crf_per = crf_per,
                      method = {{crf_rescale_method}}
                      #{{}} ensures that the
                      # value from the function argument is used
                      # instead of from an existing column
                      ))

    # Calculate population attributable fraction (PAF)
    paf <-
      calculation %>%
      # Group by exp in case that there are different exposure categories
      #TODO: DELETE THE LINE BELOW (JUST FOR TESTING PURPOSES)
      #filter(crf==min(calculation$crf)) %>%
      dplyr::group_by(crf)%>%
      dplyr::summarize(paf = bestcost::get_paf(crf_conc = crf_forPaf,
                                               prop_pop_exp = prop_pop_exp))

    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in crf
    if(length(exp)>1){
      calculation <-
        calculation %>%
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_mean = mean(exp),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp = paste(exp, collapse = ", "),
          prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
          crf_forPaf = paste(crf_forPaf, collapse = ", "))%>%
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }


    # Build the result table adding the paf to the calculation table
    results <-
      left_join(paf,
                calculation,
                by = "crf")%>%
      dplyr::mutate(impact = round(paf * bhd, 0)) %>%
      # Order columns
      dplyr::select(exp, cf, bhd,
                    crf, crf_forPaf, crf_per, ci, crf_rescale_method,
                    paf, impact,
                    starts_with("info_"))


    return(results)
  }

