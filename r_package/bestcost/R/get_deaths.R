# Title and description

#' Get deaths
#'
#' Get deaths
#' @param pop_impact \code{Data frame} with shifted population over time.
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param min_age \code{Numeric value} with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param max_age \code{Numeric value} with the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old)
#' @param meta \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @return
#' This function returns a \code{data.frame} with the number of deaths based on the life table
#' @import dplyr
#' @import tidyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

get_deaths <-
  function(pop_impact, year_of_analysis,
           meta,
           min_age = NULL, max_age = NULL){

    # Input data check ####
    # To be added

    # Add description of what happens in next code chunk ####
    deaths_by_list <- list()

    for(s in names(pop_impact[["pop_impact"]])){ # c(male, female)
      for (v in unique(unlist(purrr::map(pop_impact[["pop_impact"]], names)))){ # c(central, lower, upper) or only central
        population_secondYear_lifetable <-
          paste0("population_", year_of_analysis+1)

        deaths_by_list[[s]][[v]]<-
          pop_impact[["pop_impact"]][[s]][[v]] %>%
          # Filter keeping only the relevant age
          # Filter keeping only the relevant age
          {if(!is.null(max_age))
            dplyr::filter(., age <= max_age)
            else .} %>%
          {if(!is.null(min_age))
            dplyr::filter(., age >= min_age)
            else .} %>%
          dplyr::select(all_of(population_secondYear_lifetable)) %>%
          sum(., na.rm = TRUE)
      }
    }

    # Convert list into data frame
    deaths_by <-
      deaths_by_list %>%
      dplyr::bind_rows(., .id ="sex") %>%
      # Reshape to long format
      tidyr::pivot_longer(cols = where(is.numeric),
                          names_prefix = "erf_ci_",
                          names_to = "erf_ci",
                          values_to = "impact")
    # Add up deaths ####
    deaths_detailed <-
      deaths_by %>%
      # Sum among age groups
      # Sum among sex
      dplyr::bind_rows(
        group_by(., erf_ci) %>%
          summarise(.,
                    across(.cols=c(impact), sum),
                    # Mean to keep the value (since it is the mean of male and female
                    # and both have the same value)
                    across(where(is.character), ~"total"),
                    .groups = "keep")) %>%
      # Round column impact
      dplyr::mutate(impact_rounded = round(impact, 0)) %>%

      # Add approach and metric and round
      # dplyr::mutate(impact_metric = "Premature deaths") %>%

      # Data wrangling ####
      # Add input data and info_ data
      dplyr::left_join(.,
                       meta,
                       by = "erf_ci") %>%
      # Order rows
      dplyr::arrange(sex, erf_ci)


    deaths <-
      deaths_detailed %>%
      dplyr::filter(sex %in% "total")


    output <- list(main = deaths,
                   detailed = deaths_detailed)

    return(output)


  }
