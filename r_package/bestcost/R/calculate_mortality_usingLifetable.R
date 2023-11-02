# Title and description

#' Calculation of Health Impacts
#'
#' Calculates the mortality, i.e. premature deaths or years of life lost, attributed to the exposure to an environmental stressor using a life table approach. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3,
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3,
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval,
#' @param crf_per Numeric value showing the increment of the concentration-response function in ug/m3 (usually 10 or 5),
#' @param crf_rescale_method String to choose among "linear" and "loglinear",
#' @param lifetable_withPop_male \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (only male),
#' @param lifetable_withPop_female \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (only female),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table,
#' @param nonNatural_death_male \code{Data frame} with two columns: the first one for age, the second one for the percentage of non-natural deaths (only male),
#' @param nonNatural_death_female \code{Data frame} with two columns: the first one for age, the second one for the percentage of non-natural deaths (Only female),
#' @param pollutant String with the name of the pollutant,
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),
#' @param min_age Number with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param max_age Number with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param corrected_discount_rate Numeric value with the corrected discount rate as proportion (i.e. 0.1 instead of 10%)
#' @param exp_info \code{Data frame} of one row showing extra information in columns. Default value = NULL,
#' @param cf_info \code{Data frame} of one row showing extra information in columns. Default value = NULL,
#' @param crf_info \code{Data frame} of three rows (mean, lower bound and upper bound) showing extra information in columns. Default value = NULL,
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
calculate_mortality_usingLifetable <-
  function(exp, cf, crf, crf_per, crf_rescale_method,
           lifetable_withPop_male, lifetable_withPop_female, firstYear_lifetable,
           nonNatural_death_male, nonNatural_death_female,
           pollutant,
           age_group,
           corrected_discount_rate,
           min_age = 30, max_age = 0,
           exp_info = NULL, cf_info = NULL, crf_info =NULL){

        # Digest input data


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

        # Add ci
        input_fun[["crf"]] <-
          mutate(input_fun[["crf"]],
                 ci = ifelse(crf %in% min(crf), "lowci",
                        ifelse(crf %in% max(crf), "highci",
                               "mean")))

        # Add pollutant to all input data tables to provide a common key for joining
        # {{}} ensure that the value from the function argument is used
        # instead of from an existing column is used
        input_fun <-
          input_fun %>%
          purrr::map(~mutate(.,
                             pollutant = {{pollutant}},
                             outcome_group = {{age_group}}))

        # The life table has to be provided as a data.frame (by sex)
        # The first column has to be the age. Second, probability of death. Third, population.
        # Rename column names to standard names
        names(lifetable_withPop_male) <-
          c("age", "death_probability", paste0("population_", firstYear_lifetable))
        names(lifetable_withPop_female) <-
          c("age", "death_probability", paste0("population_", firstYear_lifetable))
        # Store data in a list
        lifetable_withPop <- list(male = lifetable_withPop_male,
                                  female = lifetable_withPop_female)


        # The mortality has to be provided as a data.frame (by sex)
        # The first column has to be the age. Second, percent_nonNatural.
        # Rename column names to standard names
        names(nonNatural_death_male) <-
          c("age", "percent_nonNatural")
        names(nonNatural_death_female) <-
          c("age", "percent_nonNatural")
        # Store data in a list
        nonNatural_death <- list(male = nonNatural_death_male,
                                 female = nonNatural_death_female)


        # Get shifted_popOverTime
        shifted_popOverTime <-
          bestcost:::get_shifted_popOverTime(
            lifetab_withPop = lifetable_withPop,
            nonNatural_death = nonNatural_death,
            firstYear_lifetable = firstYear_lifetable,
            crf = input_fun[["crf"]],
            crf_per = crf_per,
            age_group = age_group,
            exp = input_fun[["exp"]],
            cf = input_fun[["cf"]],
            crf_rescale_method = crf_rescale_method)


        # Calculate deaths
        deaths <-
          bestcost:::get_deaths_fromLifetable(
            shifted_popOverTime = shifted_popOverTime,
            firstYear_lifetable = firstYear_lifetable,
            age_group = age_group,
            min_age = min_age,
            max_age = max_age,
            exp = input_fun[["exp"]] ,
            cf = input_fun[["cf"]],
            crf_rescale_method = crf_rescale_method)

        # Calculate years of life lost (yll)
        yll <-
          bestcost:::get_yll(
            shifted_popOverTime = shifted_popOverTime,
            firstYear_lifetable = firstYear_lifetable,
            age_group = age_group,
            min_age = min_age,
            max_age = max_age,
            exp = input_fun[["exp"]] ,
            cf = input_fun[["cf"]],
            crf_rescale_method = crf_rescale_method,
            corrected_discount_rate = corrected_discount_rate)


        output <-
          list(
            shifted_popOverTime = shifted_popOverTime,
            deaths_long = deaths[["deaths_long"]],
            deaths = deaths[["deaths"]],
            yll_long = yll[["yll_long"]],
            yll = yll[["yll"]])

        return(output)

  }





