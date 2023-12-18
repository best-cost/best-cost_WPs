# Title and description

#' Get deaths
#'
#' Get deaths
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3,
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3,
#' @param crf_rescale_method String to choose among "linear" and "loglinear",
#' @param shifted_popOvertime \code{Data frame} with shifted population over time,
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table,
#' @param min_age Number with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param max_age Number with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)#'
#' @return
#' This function returns a \code{data.frame} with the number of deaths based on the life table
#' @import dplyr
#' @import tidyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function


get_deaths <-
  function(exp, cf, crf_rescale_method,
           shifted_popOverTime, year_of_analysis,
           min_age=min_age, max_age=max_age){

    deaths_by_list <- list()

    for (s in sex){
      for (v in ci){
        population_secondYear_lifetable <-
          paste0("population_", year_of_analysis+1)

        deaths_by_list[[s]][[v]]<-
          shifted_popOverTime[["shifted_popOverTime"]][[s]][[v]] %>%
          # Select only relevant columns
          dplyr::select(., age, all_of(population_secondYear_lifetable)) %>%
          # Filter keeping only the relevant age
          {if(!is.null(max_age)&!is.na(max_age))
            dplyr::filter(., age <= max_age)
            else .} %>%
          {if(!is.null(min_age)&!is.na(min_age))
            dplyr::filter(., age >= min_age)
            else .} %>%
          dplyr::select(all_of(population_secondYear_lifetable)) %>%
          sum(., na.rm = TRUE)
      }
    }

    # Convert list into data frame
    deaths_by <-
      deaths_by_list %>%
      dplyr::bind_rows(., .id ="sex")%>%
      # Reshape to long format
      tidyr::pivot_longer(cols = where(is.numeric),
                          names_to = "ci",
                          values_to = "impact_per_unit")

    deaths_long <-
      deaths_by%>%
      # Add concentration data
      dplyr::bind_cols(.,
                       exp[, c("pollutant", "exp")])%>%
      #dplyr::left_join(.,
      #exp[, c("pollutant", "exp", "outcome_group")],
      #by ="outcome_group") %>%
      dplyr::mutate(cf = cf$cf)%>%
      # Add crf
      dplyr::left_join(.,
                       shifted_popOverTime[["crf"]][, c("pollutant", "ci", "crf")],
                       by = c("pollutant", "ci"))%>%

      # Create column impact
      {if(crf_rescale_method == "ap10")
        # Calculate the health impact for the actual exposure and not only for 10ug/m3
        dplyr::mutate(., impact = impact_per_unit * (exp - cf)/10)
        else dplyr::mutate(., impact = impact_per_unit)}%>%

      # Sum among age groups
      # Sum among sex
      # Add row for total by age group (infants+adults)
      dplyr::bind_rows(
        group_by(., pollutant, ci, outcome_group, exp, cf, crf) %>%
          summarise(.,
                    across(.cols=c(impact_per_unit, impact), sum),
                    # Mean to keep the value (since it is the mean of male and female
                    # and both have the same value)
                    across(where(is.character), ~"total"),
                    .groups = "keep"))%>%
      # Add approach and metric and round
      dplyr::mutate(approach_id = paste0("lifetable_", crf_rescale_method),
                    impact_metric = "Premature deaths",
                    age_range = ifelse(outcome_group %in% c("infants", "infant", "children"),
                                       paste0("below", max_age+1),
                                       ifelse(outcome_group %in% c("adults", "adult"),
                                              paste0("from", min_age),
                                              NA)),
                    # Round column impact
                    impact = round(impact, 0))%>%


      # Order columns
      dplyr::select(pollutant, sex, ci, everything())%>%
      # Order rows
      dplyr::arrange(pollutant, sex, ci)


    deaths <-
      deaths_long%>%
      dplyr::filter(sex %in% "total")


    output <- list(deaths_long = deaths_long, deaths = deaths)

    return(output)


  }
