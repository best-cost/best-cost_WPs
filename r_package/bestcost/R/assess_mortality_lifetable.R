# Title and description

#' Calculation of Health Impacts
#'
#' Calculates the mortality, i.e. premature deaths or years of life lost, attributed to the exposure to an environmental stressor using a life table approach. It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3,
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3.
#' @param crf \code{Vector} of three numeric values referring to the mean as well as the lower bound and upper bound of the confidence interval.
#' @param crf_per Numeric value showing the increment of the concentration-response function in ug/m3 (usually 10 or 5).
#' @param crf_rescale_method String to choose among "linear" and "loglinear".
#' @param first_age_pop Numeric value of the first item of the age sequence from population and life table data
#' @param last_age_pop Numeric value of the last item of the age sequence from population and life table data
#' @param interval_age_pop Numeric value of the interval of the age sequence from population and life table data
#' @param prob_natural_death_male \code{Vector} containing the probability of dying because of natural cause (excluding non-natural deaths such as violence or accidents) by age or age group for male.
#' @param prob_natural_death_female \code{Vector} containing the probability of dying because of natural cause (excluding non-natural deaths such as violence or accidents) by age or age group for female.
#' @param prob_total_death_male \code{Vector} containing the probability of dying because of all causes (including non-natural deaths such as violence or accidents) by age or age group for male.
#' @param prob_total_death_female \code{Vector} containing the probability of dying because of all causes (including non-natural deaths such as violence or accidents) by age or age group for female.
#' @param population_male \code{Vector} containing the mid-year male population for the year of analysis.
#' @param population_female \code{Vector} containing the mid-year female population for the year of analysis.
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table.
#' @param pollutant String with the name of the pollutant.
#' @param min_age Number with the minimal age to be considered for adults (by default 30, i.e. 30+).
#' @param max_age Number with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old).
#' @param corrected_discount_rate Numeric value with the corrected discount rate as proportion (i.e. 0.1 instead of 10\%).
#' @param info_exp \code{Data frame} of one row and one or multiple columns showing information about the exposure. This information will be added to all rows of the result tables. Default value = NULL.
#' @param info_cf \code{Data frame} of one row and one or multiple columns showing information about the counter-factual scenario (cut-off). This information will be added to all rows of the result tables. Default value = NULL.
#' @param info_crf \code{Data frame} of one row and one or multiple columns showing information about the concentration-response function. This information will be added to all rows of the result tables. Default value = NULL.
#' @param info_bhd \code{Data frame} of one row and one or multiple columns showing information about the baseline health data. This information will be added to all rows of the result tables. Default value = NULL.
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
assess_mortality_lifetable <-
  function(exp, cf,
           crf, crf_per, crf_rescale_method,
           first_age_pop, last_age_pop, interval_age_pop,
           prob_natural_death_male, prob_natural_death_female,
           prob_total_death_male, prob_total_death_female,
           population_male, population_female,
           year_of_analysis,
           pollutant,
           corrected_discount_rate,
           min_age = NULL, max_age = NULL,
           info_exp = NULL, info_cf = NULL, info_crf = NULL){

        # Digest input data


        # Bind input data by category
        input_fun <- list()

        if(is.null(info_exp)){
          input_fun[["exp"]] <- data.frame(exp = exp)
        } else {
          # When there is an extra_info data frame
          # the values from argument should replace the values
          # of likely columns with same name
          input_fun[["exp"]] <- info_exp
          input_fun[["exp"]]$exp <- exp}

        if(is.null(info_cf)){
          input_fun[["cf"]] <- data.frame(cf = cf)
        } else {
          input_fun[["cf"]] <- info_cf
          input_fun[["cf"]]$cf <- cf}

        if(is.null(info_crf)){
          input_fun[["crf"]] <- data.frame(crf = crf)
        } else {
          input_fun[["crf"]] <- info_crf
          input_fun[["crf"]]$crf <- crf
          input_fun[["crf"]]$crf_per <- crf_per}

        # Add ci
        input_fun[["crf"]] <-
          mutate(input_fun[["crf"]],
                 ci = ifelse(crf %in% min(crf), "lowci",
                        ifelse(crf %in% max(crf), "highci",
                               "mean")))

        # Add bhd
        if(!is.null(info_bhd)){
          input_fun[["bhd"]] <- data.frame(bhd = bhd)}

        # Add pollutant to all input data tables to provide a common key for joining
        # {{}} ensure that the value from the function argument is used
        # instead of from an existing column is used
        input_fun <-
          input_fun %>%
          purrr::map(~mutate(.,
                             pollutant = {{pollutant}}))

        # The life table has to be provided as a data.frame (by sex)
        # The first column has to be the age. Second, probability of death. Third, population.
        # Rename column names to standard names

        lifetable_withPop <- list(

          male =
            data.frame(
              age = seq(from = first_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop),
              age_end = seq(from = first_age_pop + interval_age_pop,
                            to = last_age_pop,
                            by = interval_age_pop + interval_age_pop),
              death_probability_natural = prob_natural_death_male,
              death_probability_total = prob_total_death_male,
              population = population_male),

          female =
            data.frame(
              age = seq(from = first_age_pop,
                        to = last_age_pop,
                        by = interval_age_pop),
              age_end = seq(from = first_age_pop + interval_age_pop,
                            to = last_age_pop,
                            by = interval_age_pop + interval_age_pop),
              death_probability_natural = prob_natural_death_female,
              death_probability_total = prob_total_death_female,
              population = population_female))


        # Get population impact
        shifted_popOverTime <-
          bestcost::get_pop_impact(
            lifetab_withPop = lifetable_withPop,
            year_of_analysis = year_of_analysis,
            crf = input_fun[["crf"]],
            crf_per = crf_per,
            exp = input_fun[["exp"]],
            cf = input_fun[["cf"]],
            crf_rescale_method = crf_rescale_method)


        # Calculate deaths
        deaths <-
          bestcost::get_deaths(
            shifted_popOverTime = shifted_popOverTime,
            year_of_analysis = year_of_analysis,
            min_age = min_age,
            max_age = max_age,
            exp = input_fun[["exp"]],
            cf = input_fun[["cf"]],
            crf_rescale_method = crf_rescale_method)

        # Calculate years of life lost (yll)
        yll <-
          bestcost::get_yll(
            shifted_popOverTime = shifted_popOverTime,
            year_of_analysis = year_of_analysis,
            min_age = min_age,
            max_age = max_age,
            exp = input_fun[["exp"]],
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





