# Title and description

#' Get life year
#'
#' Get life years from lifetable
#' @param spot \code{Data frame} with shifted population over time#'
#' @param age_group String with the denomination of the age group (e.g. "adults" or "infants"),
#' @param min_age Number with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param max_age Number with the maximal age to be considered for infants/children (by default 0, i.e. below 1 years old)#' @return
#' This function returns a \code{data.frame} with the number of life years
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
get_lifeyears <-
  function(spot, age_group, min_age = min_age, max_age = max_age){
    output <-
      spot %>%
      # Filter ages depending on the age_group
      {if(age_group %in% "adults")
        dplyr::filter(., age >= min_age)
        else .}%>%

      {if(age_group %in% "infants")
        dplyr::filter(., age <= max_age)
        else .}%>%
      # Sum over ages
      dplyr::select(., contains("population_"))%>%
      dplyr::summarize_all(sum, na.rm = TRUE)%>%
      # Add outcome_group and age_range
      dplyr::mutate(
        outcome_group = age_group,
        age_range = ifelse(outcome_group %in% c("infants", "infant", "children"),
                           paste0("below", max_age+1),
                           ifelse(outcome_group %in% c("adults", "adult"),
                                  paste0("from", min_age),
                                  NA)))%>%


    return(output)
  }

