# Title and description

#' Get deaths
#'
#' Get deaths
#' @param shifted_popOvertime \code{Data frame} with shifted population over time.
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param min_age \code{Numeric value} with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param max_age \code{Numeric value} with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)#'
#' @param meta \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @return
#' This function returns a \code{data.frame} with the number of deaths based on the life table
#' @import dplyr
#' @import tidyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function


get_deaths <-
  function(shifted_popOverTime, year_of_analysis,
           meta,
           min_age = min_age, max_age = max_age){

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
          {if(!is.na(max_age))
            dplyr::filter(., age <= max_age)
            else .} %>%
          {if(!is.na(min_age))
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
                          values_to = "impact")

    deaths_long <-
      deaths_by %>%
      # Sum among age groups
      # Sum among sex
      # Add row for total by age group (infants+adults)
      dplyr::bind_rows(
        group_by(., ci) %>%
          summarise(.,
                    across(.cols=c(impact), sum),
                    # Mean to keep the value (since it is the mean of male and female
                    # and both have the same value)
                    across(where(is.character), ~"total"),
                    .groups = "keep"))%>%
      # Round column impact
      dplyr::mutate(impact_beforeRounding = impact,
                    impact = round(impact, 0))%>%

      # Add approach and metric and round
      dplyr::mutate(impact_metric = "Premature deaths")%>%


      # Add input data and info_ data
      dplyr::left_join(.,
                       meta,
                       by = "ci") %>%
      # Order columns
      dplyr::select(sex, ci, everything())%>%
      # Order rows
      dplyr::arrange(sex, ci)


    deaths <-
      deaths_long%>%
      dplyr::filter(sex %in% "total")


    output <- list(deaths_long = deaths_long, deaths = deaths)

    return(output)


  }
