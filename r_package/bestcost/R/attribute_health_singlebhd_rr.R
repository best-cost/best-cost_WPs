# Title and description

#' Health impacts based on absolute risk

#' Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp \code{Numeric value} showing the population-weighted mean exposure in ug/m3 or {vector} showing the exposure category in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param prop_pop_exp \code{Numeric value} or {vector} showing the proportion of population exposed (as a fraction, i.e. values between 0 and 1) for a single exposure value or for multiple categories, i.e., a exposure distribution, respectively. If a exposure distribution is used, the dimension of this input variable should be the same as "exp". By default, 1 for single exposure value will be assigned to this input variable assuming a single exposure value, but users can change this value.
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} to choose among "linear" and "log_linear".
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

    # Check input data ####
    stopifnot(exprs = {
      length(exp) == length(prop_pop_exp)
    })

    # Input data in data frame ####
    # Compile rr data to assign categories
    input <-
      data.frame(
        # Compile input data
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        cutoff = cutoff,
        bhd = bhd,
        approach_id = paste0("lifetable_", erf_shape),
        # Assign mean, low and high rr values
        rr_ci = ifelse(rr %in% min(rr), "low",
                       ifelse(rr %in% max(rr), "high",
                              "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci)) %>%
      # Add exposure categories to rr's with a cross join to produce all likely combinations between exp categories and rr estimates (central, upper & lower CI)
      dplyr::cross_join(.,
                        data.frame(exp = exp,
                                   prop_pop_exp = prop_pop_exp)) %>%
      # rescale rr's for PAF
      dplyr::mutate(
        rr_forPaf =
          bestcost::get_risk(rr = rr,
                               exp = exp,
                               cutoff = cutoff,
                               rr_increment = rr_increment,
                               erf_shape = {{erf_shape}}),
        #{{}} ensures that the
        # value from the function argument is used
        # instead of from an existing column
        # Add a column for the average exp (way to summarize exposure)
        exp_mean = mean(exp))


    # Calculate population attributable fraction (PAF) ####
    paf <-
      input %>%
      # Group by increasing exp in case that there are different exposure categories
      dplyr::group_by(rr) %>%
      # Calculate PAFs per row & then reduce nrow by summing PAFs belonging to same rr
      dplyr::summarize(paf = bestcost::get_paf(rr_conc = rr_forPaf,
                                               prop_pop_exp = prop_pop_exp))

    # Data wrangling ####
    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    if(length(exp)>1){
      input <-
        input %>%
        dplyr::mutate(
          # Replace the actual values with "multiple" (i.e. vector) to enable reduction of rows
          exp = paste(exp, collapse = ", "),
          prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
          rr_forPaf = paste(rr_forPaf, collapse = ", ")) %>%
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    # Join the input table with paf values
    input <-
      input %>%
      dplyr::left_join(paf,
                       input,
                       by = "rr")

  # Build the result table adding the paf to the input table
   output <-
      input %>%
      dplyr::mutate(impact = paf * bhd,
                    impact_rounded = round(impact, 0)) %>%
     # Add additional information (info_x variables)
     dplyr::mutate(
       info = ifelse(is.null(info), NA, info)) %>%
      # Order columns
      dplyr::select(exp, prop_pop_exp, exp_mean, impact, impact_rounded,
                    cutoff, bhd, rr, rr_forPaf, rr_increment, ci, erf_shape,
                    paf,
                    starts_with("info"))

    return(output)
  }

