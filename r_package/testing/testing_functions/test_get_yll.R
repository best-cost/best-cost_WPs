# Input data ####
load("../testing/testing_functions/test_get_yll.rda")
min_age <- NA # before: 20
max_age <- NA # before: also NA

lifeyears_byYear <- list()
yll_by_list<-list()

discount_factor <- corrected_discount_rate + 1

for(s in sex){
  for (v in ci){
    
    # Life years by year (NO FUNCTION CALLED)
    lifeyears_byYear[[s]][[v]] <-
      pop_impact[["pop_impact"]][[s]][[v]] %>%
      
      # Filter keeping only the relevant age
      {if(!is.na(max_age))
        dplyr::filter(., age <= max_age)
        else .} %>%
      {if(!is.na(min_age))
        dplyr::filter(., age >= min_age)
        else .} %>%
      
      # Select relevant columns
      dplyr::select(., contains("population_")) %>%
      # Remove the year of analysis (we are only interested in the following ones)
      dplyr::select(., -contains(as.character(year_of_analysis))) %>%
      # Sum over ages
      dplyr::summarize_all(sum, na.rm = TRUE) %>%
      # Reshape to long format
      tidyr::pivot_longer(cols = starts_with("population_"),
                          names_to = "year",
                          values_to = "impact",
                          names_prefix = "population_")
    
    # Years of life lost
    yll_by_list[[s]][[v]][["noDiscount"]] <-
      lifeyears_byYear[[s]][[v]]%>%
      # Sum among years
      dplyr::summarise(impact = sum(impact), .groups = 'drop')
    
    
    
    # Discounted life years
    yll_by_list[[s]][[v]][["discounted"]] <-
      lifeyears_byYear[[s]][[v]]%>%
      # Convert year to numeric
      dplyr::mutate(year = as.numeric(year))%>%
      # Calculate discount
      dplyr::mutate(discount = 1/(discount_factor^(year-(year_of_analysis+1))))%>%
      # Calculate life years discounted
      dplyr::mutate(discounted_impact = impact*discount)%>%
      # Sum among years
      dplyr::summarise(impact = sum(discounted_impact), .groups = 'drop')
  }
}

# Convert list into data frame
yll_by <-
  yll_by_list%>%
  purrr::map(map, dplyr::bind_rows, .id = "discount")%>%
  purrr::map(dplyr::bind_rows, .id = "ci" )%>%
  dplyr::bind_rows(., .id = "sex")


# Calculate Years of Life Lost (YLLs)
yll_detailed <-
  yll_by %>%
  
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
    impact_metric = "Year of life lost") %>%
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

yll <-
  yll_detailed %>%
  dplyr::filter(sex %in% "total",
                discount %in% "discounted")


output <- list(yll_detailed = yll_detailed, yll = yll)