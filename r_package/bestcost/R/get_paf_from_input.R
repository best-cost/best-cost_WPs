# Title and description

#' Get input data and PAF

#' Calculates the population attributable fraction (PAF) based on the input data and puts the results in additional columns joined to the input data frame.
#' @param input \code{Data frame} with the input data including proportion of population exposed (prop_pop_exp), the cut-off (cutoff), the exposure-response data (rr_increment)
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param rr \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param rr_increment \code{Numeric value} showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param erf_shape \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
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
get_paf_from_input <-
  function(exp,
           prop_pop_exp,
           cutoff,
           rr,
           rr_increment,
           erf_shape,
           bhd,
           info){
    # Check input data ####
    # TBA: checks

    # Input data in data frame ####
    # Compile rr data to assign categories
    erf_data <-
      data.frame(
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        cutoff = cutoff,
        rr = rr,
        # Assign mean, low and high rr values
        rr_ci = ifelse(rr %in% min(rr), "low",
                       ifelse(rr %in% max(rr), "high",
                              "mean"))) %>%
      # In case of same value in mean and low or high, assign value randomly
      dplyr::mutate(ci = ifelse(duplicated(rr), "mean", ci))

    # Compile input data except meta-info
    input <-
      data.frame(
        exp = exp,
        prop_pop_exp = prop_pop_exp,
        bhd = bhd)%>%
      # Add rr with a cross join to produce all likely combinations
      dplyr::cross_join(., erf_data)
    # Add additional (meta-)information
    input <-
      bestcost::add_info(df=input, info=info)


    # Calculate health impact attributable to exposure ####
    input_and_paf <-
      input %>%
      dplyr::mutate(
        rr_forPaf =
          get_risk(rr = rr,
                   exp = exp,
                   cutoff = cutoff,
                   rr_increment = rr_increment,
                   erf_shape = unique(erf_shape)
          ))

    # Calculate population attributable fraction (PAF) ####
    paf <-
      input_and_paf %>%
      # Group by exp in case that there are different exposure categories
      dplyr::group_by(rr) %>%
      dplyr::summarize(paf = bestcost::get_paf(rr_conc = rr_forPaf,
                                               prop_pop_exp = prop_pop_exp))

    # Data wrangling ####
    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    if(length(exp)>1){
      input_and_paf <-
        input_and_paf %>%
        dplyr::mutate(
          # Add a column for the average exp (way to summarize exposure)
          exp_mean = mean(exp),
          # Replace the actual values with "multiple" to enable reduction of rows
          exp = paste(exp, collapse = ", "),
          prop_pop_exp = paste(prop_pop_exp, collapse = ", "),
          rr_forPaf = paste(rr_forPaf, collapse = ", ")) %>%
        # Keep only rows that are distinct
        dplyr::distinct(.)
    }

    # Join the input table with paf values
    input_and_paf <-
      input_and_paf %>%
      dplyr::left_join(paf,
                       input_and_paf,
                       by = "rr")
  }
