# Title and description

#' Get years of life lost
#'
#' Get years of life lost
#' @param exp Numeric value showing the population-weighted mean exposure in ug/m3,
#' @param cf Numeric value showing the counter-factual scenario (i.e. minimum cut-off concentration) in ug/m3,
#' @param crf_per Numeric value showing the increment of the concentration-response function in ug/m3 (usually 10 or 5),
#' @param crf_rescale_method String to choose among "linear" and "loglinear",
#' @param shifted_popOvertime \code{Data frame} with shifted population over time
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table,
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),
#' @param age_min Number with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max Number with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param corrected_discount_rate Numeric value with the annual discount rate as proportion (i.e. 0.1 instead of 10%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
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
  function(exp, cf, crf_rescale_method,
           shifted_popOverTime, firstYear_lifetable,
           age_group, min_age = min_age, max_age = max_age,
           corrected_discount_rate){

    lifeyears_byYear <- list()
    yll_by_list<-list()

    discount_factor <- corrected_discount_rate + 1

    for(s in sex){
      for (v in ci){

        # Life years by year
        lifeyears_byYear[[s]][[v]][["noDiscount"]] <-
          bestcost::get_lifeyears(
            spot = shifted_popOverTime[["shifted_popOverTime"]][[s]][[v]],
            age_group = age_group,
            min_age = min_age,
            max_age = max_age)

        # Years of life lost
        yll_by_list[[s]][[v]][["noDiscount"]] <-
          # Convert to long
          lifeyears_byYear[[s]][[v]][["noDiscount"]]%>%
          tidyr::pivot_longer(cols = starts_with("population_"),
                              names_to = "year",
                              values_to = "value",
                              names_prefix = "population_")%>%
          # Sum among years
          dplyr::group_by(age_range, outcome_group)%>%
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
          dplyr::mutate(discount = 1/(discount_factor^(year-(firstYear_lifetable+1))))%>%
          # Calculate life years discounted
          dplyr::mutate(discounted_value = value*discount)%>%
          # Sum among years
          dplyr::group_by(age_range, outcome_group)%>%
          dplyr::summarise(value = sum(discounted_value), .groups = 'drop')
      }
    }

    # Convert list into data frame
    yll_by <-
      yll_by_list%>%
      #purrr::map(map, map, dplyr::bind_rows, .id = "outcome_group")%>%
      purrr::map(map, dplyr::bind_rows, .id = "discount")%>%
      purrr::map(dplyr::bind_rows, .id = "ci" )%>%
      dplyr::bind_rows(., .id = "sex")


    # Calculate Years of Life Lost (YLLs)
    yll_long <-
      yll_by%>%
      # Add exposure and counterfactual scenario
      dplyr::left_join(.,
                       exp[, c("pollutant", "exp", "outcome_group")],
                       by ="outcome_group") %>%
      dplyr::mutate(cf = cf$cf)%>%
      # Add crf
      dplyr::left_join(.,
                       shifted_popOverTime[["crf"]][, c("pollutant",
                                                        "outcome_group", "ci",
                                                        "crf")],
                       by = c("pollutant", "outcome_group", "ci"))%>%
      # Round crf
      # dplyr::mutate(crf = round(crf, 3))%>%
      # Rename column
      dplyr::rename("impact_per_unit" = "value")%>%

      {if(crf_rescale_method == "ap10")
        # Calculate the health impact for the actual exposure and not only for 10ug/m3
        dplyr::mutate(., impact = round(impact_per_unit * (exp - cf)/10, 0))
        else dplyr::mutate(., impact = round(impact_per_unit))}%>%

      # Sum among sex
      # Add row for total by age group (infants+adults)
      dplyr::bind_rows(
        group_by(.,
                 pollutant, discount, ci, age_range, outcome_group, exp, cf,
                 crf) %>%
          summarise(.,
                    across(.cols=c(impact_per_unit, impact), sum),
                    across(where(is.character), ~"total"),
                    .groups = "keep"))%>%
      # Add approach
      dplyr::mutate(approach_id = paste0("lifetable_", crf_rescale_method),
                    # Add metric
                    impact_metric = "Year of life lost")%>%
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
