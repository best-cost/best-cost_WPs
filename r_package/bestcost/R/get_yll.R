# Title and description

#' Get years of life lost
#'
#' Get years of life lost
#' @param pop_impact \code{Data frame} with projected population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min \code{Numeric value} with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max \code{Numeric value} with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param meta \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @param corrected_discount_rate \code{Numeric value}  with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal

get_yll <-
  function(pop_impact, year_of_analysis,
           min_age = min_age, max_age = max_age,
           meta,
           corrected_discount_rate=0){

    lifeyears_byYear <- list()
    yll_by_list<-list()

    discount_factor <- corrected_discount_rate + 1

    # Calculate YLL ####
    for(s in names(pop_impact[["pop_impact"]])){ # c(male, female)
      for (v in unique(unlist(purrr::map(pop_impact[["pop_impact"]], names)))){ # c(central, lower, upper) or only central

        ## Sum life years by year (result is data frame with 2 columns "year" & "impact" [which contains YLL]) ####
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
          # Sum over ages (i.e. vertically) that fulfill inputted "max_age" and "min_age" criteria
          dplyr::summarize_all(sum, na.rm = TRUE) %>%
          # Reshape to long format (output is data frame with 2 columns "year" & "impact")
          tidyr::pivot_longer(cols = starts_with("population_"),
                              names_to = "year",
                              values_to = "impact",
                              names_prefix = "population_")

        ## Calculate total, not discounted YLL (single number) per sex & ci ####
        yll_by_list[[s]][[v]][["noDiscount"]] <-
          lifeyears_byYear[[s]][[v]]%>%
          # Sum among years to obtain the total impact (single value)
          dplyr::summarise(impact = sum(impact), .groups = 'drop')



        ## Calculate total, discounted life years (single value) per sex & ci ####
        yll_by_list[[s]][[v]][["discounted"]] <-
          lifeyears_byYear[[s]][[v]]%>%
          # Convert year to numeric
          dplyr::mutate(year = as.numeric(year))%>%
          # Calculate discount rate for each year
          dplyr::mutate(discount = 1/(discount_factor^(year-(year_of_analysis+1))))%>%
          # Calculate life years discounted
          dplyr::mutate(discounted_impact = impact*discount)%>%
          # Sum among years to obtain the total impact (single value)
          dplyr::summarise(impact = sum(discounted_impact), .groups = 'drop')
      }
    }

    # Data wrangling ####
    # Convert list into data frame
    yll_by <-
      yll_by_list %>%
      purrr::map(map, dplyr::bind_rows, .id = "discounted") %>%
      purrr::map(dplyr::bind_rows, .id = "erf_ci" ) %>%
      dplyr::bind_rows(., .id = "sex")%>%
      # Replace "discount" and "noDiscount" with TRUE and FALSE
      # Add discount rate
      # Rename erf_ci values
      dplyr::mutate(
        discounted = ifelse(discounted %in% "discounted", TRUE,
                            ifelse(discounted %in% "noDiscount", FALSE,
                                   NA)),
        corrected_discount_rate = corrected_discount_rate,
        erf_ci = gsub("erf_ci_", "", erf_ci))


    ## Compile information needed for detailed YLL results ####
    yll_detailed <-
      yll_by %>%

      # Sum among sex adding total
      dplyr::bind_rows(
        group_by(.,
                 discounted, erf_ci, corrected_discount_rate) %>%
          summarise(.,
                    across(.cols=c(impact), sum),
                    across(where(is.character), ~"total"),
                    .groups = "keep")) %>%

      # Add  metric
      dplyr::mutate(
        impact_metric = "Year of life lost") %>%
      # Add meta information (with left join)
      dplyr::left_join(.,
                       meta,
                       by = "erf_ci")%>%

      # Round the results
      dplyr::mutate(impact_rounded = round(impact, 0))%>%

      # Order columns
      dplyr::select(discounted, sex, erf_ci, everything())%>%
      # Order rows
      dplyr::arrange(discounted, sex, erf_ci)

    yll <-
      dplyr::filter(yll_detailed, sex %in% "total")

    # If the user does not want any discount
    # keep only the no-discount rows removing the ones with discount
    # in both tables
    if(corrected_discount_rate == 0){
      yll <-
        dplyr::filter(yll, discounted %in% FALSE)

      yll_detailed <-
        dplyr::filter(yll_detailed, discounted %in% FALSE)


    } else {
      # If a discount is desired then show the discounting in the main results
      yll <-
        yll %>%
        dplyr::filter(discounted %in% TRUE)
    }


    output <- list(main = yll, detailed = yll_detailed)

    return(output)
  }
