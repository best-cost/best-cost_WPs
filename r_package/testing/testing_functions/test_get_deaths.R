#' Get deaths
#'
#' @param pop_impact \code{Data frame} with each row representing the starting age group population + the projected population over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table,
#' @param min_age \code{Numeric value} with the minimal age to be considered for adults (by default 30, i.e. 30+),
#' @param max_age \code{Numeric value} with the maximal age to be considered for infants/children (by default 0, i.e. below 1 year old)
#' @param meta \code{Data frame} with meta-information
#' @return
#' This function adds up deaths per sex and ci and across sex from the inputted \code{Data frame} and returns a \code{Data frame} with the number of deaths based on the life table approach
#' @import dplyr
#' @import tidyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

# Input data
load("../testing/testing_functions/test_get_deaths.rda")

# Function call ####
output_function_call <- bestcost::get_deaths(
  pop_impact = pop_impact,
  year_of_analysis = year_of_analysis,
  min_age = min_age,
  max_age = max_age,
  meta = meta)

# Function code ####
# Add description of what happens in next code chunk ####
deaths_by_list <- list()

# Sum all deaths from the column "population_yoa+1" (containing the deaths due to air pollution from exposure in the YOA), taking the specified age ranges into account (min_age & max_age)
for (s in sex){
  for (v in ci){
    population_secondYear_lifetable <-
      paste0("population_", year_of_analysis+1)
    
    # Apply the min_age & max_age criteria (if inputted)
    deaths_by_list[[s]][[v]]<-
      pop_impact[["pop_impact"]][[s]][[v]] %>%
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
  dplyr::bind_rows(., .id ="sex") %>% # id = "sex" adds an extra column "sex" with the row values "female" & "male"
  # Reshape to long format with the columns "sex", "ci", "impact"
  tidyr::pivot_longer(cols = where(is.numeric),
                      names_to = "ci",
                      values_to = "impact")
# Add up deaths ####
deaths_detailed <-
  deaths_by %>%
  # Add 3 rows for total deaths (male+female) (mean, lowci, highci)
  dplyr::bind_rows(
     group_by(., ci) %>%
      summarise(.,
                across(.cols=c(impact), sum), # Sum the impacts
                # Mean to keep the value (since it is the mean of male and female
                # and both have the same value) AL: not sure what these comments mean? #####
                across(where(is.character), ~"total"), # Adds "total" (in the column "sex" to the new rows)  
                .groups = "keep")) %>%
  # Round column impact
  dplyr::mutate(impact_rounded = round(impact, 0)) %>%
  # Add approach and metric and round
  dplyr::mutate(impact_metric = "Premature deaths") %>%
  # Add input data and info_ data
  dplyr::left_join(.,
                 meta,
                 by = "ci") %>%
  # Order columns
  dplyr::select(sex, ci, everything())


deaths <-
  deaths_detailed %>%
  dplyr::filter(sex %in% "total")


output <- list(deaths_detailed = deaths_detailed, deaths = deaths)
