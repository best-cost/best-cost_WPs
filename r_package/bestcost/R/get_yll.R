# Title and description

#' Get years of life lost
#'
#' Get years of life lost
#' @param shifted_popOvertime \code{Data frame} with shifted population over time
#' @param year_of_analysis Numeric value of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min Number with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max Number with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param corrected_discount_rate Numeric value with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
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
#' @import tidyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function



get_yll <-
  function(shifted_popOverTime, year_of_analysis,
           min_age = min_age, max_age = max_age,
           corrected_discount_rate){

    lifeyears_byYear <- list()
    yll_by_list<-list()

    discount_factor <- corrected_discount_rate + 1

    for(s in sex){
      for (v in ci){

        # Life years by year (NO FUNCTION CALLED)
        lifeyears_byYear[[s]][[v]][["noDiscount"]] <-
          shifted_popOverTime[["shifted_popOverTime"]][[s]][[v]] %>%

          # Filter keeping only the relevant age
          dplyr::filter(., age >= min_age & age <= max_age) %>%

          # Sum over ages
          dplyr::select(., contains("population_")) %>%
          dplyr::summarize_all(sum, na.rm = TRUE) %>%

          # Add age_range
          dplyr::mutate(
            age_range = ifelse(!is.null(max_age),
                               paste0("below", max_age+1),
                               ifelse(!is.null(min_age),
                                      paste0("from", min_age),
                                      NA)))

        # Years of life lost
        yll_by_list[[s]][[v]][["noDiscount"]] <-
          # Convert to long
          lifeyears_byYear[[s]][[v]][["noDiscount"]]%>%
          tidyr::pivot_longer(cols = starts_with("population_"),
                              names_to = "year",
                              values_to = "value",
                              names_prefix = "population_")%>%
          # Sum among years
          dplyr::group_by(age_range)%>%
          dplyr::summarise(value = sum(value), .groups = 'drop')



        # Discounted life years
        yll_by_list[[s]][[v]][["discounted"]] <-
          lifeyears_byYear[[s]][[v]][["noDiscount"]]%>%
          # Reshape to long format
          tidyr::pivot_longer(cols = starts_with("population_"),
                              names_to = "year",
                              values_to = "value",
                              names_prefix = "population_")%>%
          # Convert year to numeric
          dplyr::mutate(year = as.numeric(year))%>%
          # Calculate discount
          dplyr::mutate(discount = 1/(discount_factor^(year-(year_of_analysis+1))))%>%
          # Calculate life years discounted
          dplyr::mutate(discounted_value = value*discount)%>%
          # Sum among years
          dplyr::group_by(age_range)%>%
          dplyr::summarise(value = sum(discounted_value), .groups = 'drop')
      }
    }

    # Convert list into data frame
    yll_by <-
      yll_by_list%>%
      purrr::map(map, dplyr::bind_rows, .id = "discount")%>%
      purrr::map(dplyr::bind_rows, .id = "ci" )%>%
      dplyr::bind_rows(., .id = "sex")


    # Calculate Years of Life Lost (YLLs)
    yll_long <-
      yll_by %>%
      # Rename column
      dplyr::rename("impact_beforeRounding" = "value") %>%

      # Create new column impact (later rounded)
      dplyr::mutate(., impact = impact_beforeRounding) %>%


      # Sum among sex
      dplyr::bind_rows(
        group_by(.,
                 discount, ci) %>%
          summarise(.,
                    across(.cols=c(impact_beforeRounding, impact), sum),
                    across(where(is.character), ~"total"),
                    .groups = "keep"))%>%
      # Add information such as exp, cf, approach, metric
      dplyr::mutate(
        pollutant = exp$pollutant,
        exp = exp$exp,
        cf = cf$cf,
        approach_id = paste0("lifetable_", crf_rescale_method),
        impact_metric = "Year of life lost") %>%
      # Add crf (with left join)
      dplyr::left_join(.,
                       shifted_popOverTime[["crf"]][, c("pollutant", "ci",
                                                        "crf")],
                       by = c("pollutant", "ci"))%>%
      # Round the results
      dplyr::mutate(
        # Round column impact
        impact = round(impact, 0))%>%



      # Order columns
      dplyr::select(pollutant, discount, sex, age_range, ci, everything())%>%
      # Order rows
      dplyr::arrange(pollutant, discount, sex, age_range, ci)

    yll <-
      yll_long%>%
      dplyr::filter(sex %in% "total",
                    discount %in% "discounted")


    output <- list(yll_long = yll_long, yll = yll)

    return(output)
  }
