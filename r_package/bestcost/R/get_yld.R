# Title and description

#' Get years lived with disability
#'
#' Get years lived with disability
#' @param pop_impact \code{Data frame} with projected population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min \code{Numeric value}  with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max \code{Numeric value}  with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param meta \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @param corrected_discount_rate \code{Numeric value}  with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
#' @param disbility_weight \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the duration (in years) of the morbidity health outcome
#' @return
#' This function returns a \code{List}
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' TBD
#' @author Axel Luyten
#' @note Experimental function
#' @export

get_yld <-
  function(pop_impact,
           year_of_analysis,
           min_age,
           max_age,
           meta,
           corrected_discount_rate = 0,
           disability_weight,
           duration = NULL){

    if (is.null(duration)) duration <- 99

    lifeyears_byYear <- list()
    yld_by_list<-list()

    discount_factor <- corrected_discount_rate + 1

    # Calculate YLD ####
    for(s in sex){
      for (v in ci){

        ## Sum life years by year (result is data frame with 2 columns "year" & "impact" [which contains YLD]) ####
        lifeyears_byYear[[s]][[v]] <-
          pop_impact[["pop_impact"]][[s]][[v]] %>%

          # Filter keeping only the relevant age
          {if(!is.null(max_age))
            dplyr::filter(., age <= max_age)
            else .} %>%
          {if(!is.null(min_age))
            dplyr::filter(., age >= min_age)
            else .} %>%

          # Select relevant columns
          dplyr::select(., contains("population_")) %>%
          # Remove the year of analysis (we are only interested in the following ones)
          dplyr::select(., -contains(as.character(year_of_analysis))) %>%
          # Sum over all ages (i.e. vertically) that fulfill inputted "max_age" and "min_age" criteria
          dplyr::summarize_all(sum, na.rm = TRUE) %>% # The rows in each column are summed and the sum replaces the existing columns values (output is data frame with 1 row)
          # Reshape data frame to long format (output is data frame with 2 columns "year" & "impact")
          tidyr::pivot_longer(cols = starts_with("population_"),
                              names_to = "year",
                              values_to = "impact", # Summed YLDs are saved in the column "impact"
                              names_prefix = "population_") %>%
          # Convert year to numeric
          dplyr::mutate(year = as.numeric(year))

        ## Calculate total, not discounted YLD (single number) per sex & ci ####
        yld_by_list[[s]][[v]][["noDiscount"]] <-
          lifeyears_byYear[[s]][[v]] %>%
          # Filter for the relevant years
          filter(year < (year_of_analysis + duration + 1)) %>%
          # Sum among years to obtain the total impact (single value)
          dplyr::summarise(impact = sum(impact) * disability_weight, .groups = 'drop')

        # ## Calculate total, discounted life years (single value) per sex & ci ####
        yld_by_list[[s]][[v]][["discounted"]] <-
          lifeyears_byYear[[s]][[v]]%>%
          # Calculate discount rate for each year
          dplyr::mutate(discount = 1/(discount_factor^(year-(year_of_analysis+1)))) %>%
          # Calculate life years discounted
          dplyr::mutate(discounted_impact = impact*discount) %>%
          # Filter for the relevant years
          filter(year < (year_of_analysis + duration + 1)) %>%
          # Sum among years to obtain the total impact (single value)
          dplyr::summarise(impact = sum(discounted_impact) * disability_weight, .groups = 'drop')
      }
    }

    # Data wrangling ####
    # Convert list into data frame
    yld_by <-
      yld_by_list%>%
      purrr::map(map, dplyr::bind_rows, .id = "discount")%>%
      purrr::map(dplyr::bind_rows, .id = "ci" )%>%
      dplyr::bind_rows(., .id = "sex")

    ## Compile information needed for detailed yld results ####
    yld_detailed <-
      yld_by %>%
      # Sum among sex adding total
      dplyr::bind_rows(
        group_by(.,
                 discount, ci) %>%
          summarise(.,
                    across(.cols=c(impact), sum),
                    across(where(is.character), ~"total"),
                    .groups = "keep"))%>%

      # Add  metric
      dplyr::mutate(
        impact_metric = "Year lived with disability") %>%
      # Add meta information (with left join)
      dplyr::left_join(.,
                       meta,
                       by = "ci")%>%

      # Round the results
      dplyr::mutate(impact_rounded = round(impact, 0))%>%

      # Order columns
      dplyr::select(discount, sex, ci, everything())%>%
      # Order rows
      dplyr::arrange(discount, sex, ci)

    yld <-
      yld_detailed %>%
      dplyr::filter(sex %in% "total",
                    discount %in% "discounted")


    output <- list(total = yld, detailed = yld_detailed)

    return(output)
  }
