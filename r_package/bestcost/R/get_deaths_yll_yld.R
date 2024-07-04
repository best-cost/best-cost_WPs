# Title and description

#' Get deaths, YLL or YLD
#'
#' Get attributable deaths, years of life lost or years lived with disability from lifetable
#' @param outcome_metric \code{String} with the metric of the health outcome to be assessed. Options: "deaths", "yll" or "yld".
#' @param pop_impact \code{Data frame} with projected population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param age_min \code{Numeric value}  with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param age_max \code{Numeric value}  with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)
#' @param first_age_pop \code{Numeric value} starting age of the youngest age group from population and life table data
#' @param last_age_pop \code{Numeric value} ending age of the oldest age group from population and life table data
#' @param input_with_risk_and_pop_fraction \code{Data frame} with meta-information such as input data, additional information and intermediate results.
#' @param corrected_discount_rate \code{Numeric value}  with the annual discount rate as proportion (i.e. 0.1 instead of 10\%). It can be calculated as (1+discount_rate_beforeCorrection/1+rate_of_increase)-1
#' @param disability_weight \code{Numeric value} showing the disability weight associated with the morbidity health outcome
#' @param duration \code{Numeric value} showing the duration (in years) of the morbidity health outcome
#' @return
#' This function returns a \code{List}
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @keywords internal
#'
#'
#'
get_deaths_yll_yld <-
  function(outcome_metric,
           pop_impact,
           year_of_analysis,
           min_age = NULL,
           max_age = NULL,
           input_with_risk_and_pop_fraction,
           corrected_discount_rate = NULL,
           disability_weight = NULL,
           duration = NULL){

    lifeyears_by_year <- list()
    impact_by_list<-list()

    for(s in names(pop_impact[["pop_impact"]])){ # c(male, female)
      for (v in unique(unlist(purrr::map(pop_impact[["pop_impact"]], names)))){ # c(central, lower, upper) or only central

        ## Sum life years by year (result is data frame with 2 columns "year" & "impact" [which contains YLD]) ####
        lifeyears_by_year[[s]][[v]] <-
          pop_impact[["pop_impact"]][[s]][[v]] %>%

          # Filter keeping only the relevant age
          {if(!is.null(max_age))
            dplyr::filter(., age <= max_age)
            else .} %>%

          {if(!is.null(min_age))
            dplyr::filter(., age >= min_age)
            else .}


        # Get deaths (if applicable)
        if(outcome_metric == "deaths"){
          impact_by_list[[s]][[v]]<-
            lifeyears_by_year[[s]][[v]] %>%
            dplyr::select(all_of(paste0("population_", year_of_analysis+1))) %>%
            sum(., na.rm = TRUE)
        }



        # Get YLL or YLD (if applicable)
        if(outcome_metric %in% c("yll", "yld")){
          # Select relevant
          lifeyears_by_year[[s]][[v]] <-
            lifeyears_by_year[[s]][[v]] %>%
            dplyr::select(., contains("population_")) %>%
            # Remove the year of analysis (we are only interested in the following ones)
            dplyr::select(., -contains(as.character(year_of_analysis))) %>%
            # Sum over ages (i.e. vertically) that fulfill inputted "max_age" and "min_age" criteria
            dplyr::summarize_all(sum, na.rm = TRUE) %>%
            # Reshape to long format (output is data frame with 2 columns "year" & "impact")
            tidyr::pivot_longer(cols = starts_with("population_"),
                                names_to = "year",
                                values_to = "impact",
                                names_prefix = "population_") %>%
            # Convert year to numeric
            dplyr::mutate(year = as.numeric(year))


          ## Calculate total, not discounted YLL (single number) per sex & ci ####
          # If yll
          if(outcome_metric %in% "yll"){
            impact_by_list[[s]][[v]][["noDiscount"]] <-
              lifeyears_by_year[[s]][[v]]%>%
              dplyr::summarise(., impact = sum(impact), .groups = "drop")

            # If yld
          } else{
            impact_by_list[[s]][[v]][["noDiscount"]] <-
              lifeyears_by_year[[s]][[v]]%>%
              # Filter for the relevant years
              dplyr::filter(., year < (year_of_analysis + duration + 1)) %>%
              # Sum among years to obtain the total impact (single value)
              dplyr::summarise(
                impact = sum(impact) * disability_weight, .groups = 'drop')
          }


          # If a value for corrected_discount_rate was provided by the user,
          # apply discount
          if(!is.null(corrected_discount_rate)){

            discount_factor <- corrected_discount_rate + 1

            ## Calculate total, discounted life years (single value) per sex & ci ####
            impact_by_list[[s]][[v]][["discounted"]] <-
              lifeyears_by_year[[s]][[v]]%>%
              # Convert year to numeric
              dplyr::mutate(year = as.numeric(year))%>%
              # Calculate discount rate for each year
              dplyr::mutate(discount = 1/(discount_factor^(year-(year_of_analysis+1))))%>%
              # Calculate life years discounted
              dplyr::mutate(discounted_impact = impact*discount)%>%

              {if(outcome_metric %in% "yll")
                # Sum among years to obtain the total impact (single value)
                dplyr::summarise(., impact = sum(discounted_impact), .groups = 'drop') else .} %>%
              {if(outcome_metric %in% "yld")
                # Filter for the relevant years
                dplyr::filter(., year < (year_of_analysis + duration + 1)) %>%
                # Sum among years to obtain the total impact (single value)
                dplyr::summarise(impact = sum(discounted_impact) * disability_weight, .groups = 'drop') else .}
            }
          }

      }
    }


    # Convert list into data frame
    if(outcome_metric == "deaths"){
      impact_by <-
        impact_by_list %>%
        # Flatten by sex
        dplyr::bind_rows(., .id = "sex") %>%
        # Reshape to long format
        tidyr::pivot_longer(.,
                            cols = where(is.numeric),
                            names_prefix = "erf_ci_",
                            names_to = "erf_ci",
                            values_to = "impact") %>%
        # Create discounted and corrected_discount_rate columns with NA
        dplyr::mutate(discounted = NA,
                      corrected_discount_rate = NA)
    }

    if(outcome_metric %in% c("yll", "yld")){
      impact_by <-
        impact_by_list %>%
        # Flatten by discounted erf_ci and sex
        purrr::map(map, dplyr::bind_rows, .id = "discounted") %>%
        purrr::map(dplyr::bind_rows, .id = "erf_ci" ) %>%
        dplyr::bind_rows(., .id = "sex") %>%
        # Change and add columns
        dplyr::mutate(
          # Replace "discount" and "noDiscount" with TRUE and FALSE
          discounted = ifelse(discounted %in% "discounted", TRUE,
                              ifelse(discounted %in% "noDiscount", FALSE,
                                     NA)),
          # Add discount rate
          corrected_discount_rate = corrected_discount_rate,
          # Rename erf_ci values
          erf_ci = gsub("erf_ci_", "", erf_ci))}

    ## Compile information needed for detailed yld results ####
    impact_detailed <-
      impact_by %>%
      # Sum among sex adding total
      dplyr::group_by(.,
                      discounted, erf_ci) %>%
      dplyr::summarise(.,
                across(.cols=c(impact), sum),
                across(where(is.character), ~"total"),
                .groups = "keep")%>%
      dplyr::bind_rows(impact_by, .) %>%

      # Add  metric
      dplyr::mutate(
        outcome_metric = outcome_metric) %>%
      # Add meta information (with left join)
      dplyr::left_join(.,
                       input_with_risk_and_pop_fraction,
                       by = "erf_ci")%>%
      # Order columns
      dplyr::select(discounted, sex, erf_ci, everything())%>%
      # Order rows
      dplyr::arrange(discounted, sex, erf_ci)

    impact <-
      dplyr::filter(impact_detailed, sex %in% "total") %>%
      {if(!is.null(corrected_discount_rate))
        dplyr::filter(., discounted %in% TRUE) else .}


    output <- list(main = impact,
                   detailed = impact_detailed)

    return(output)
  }
