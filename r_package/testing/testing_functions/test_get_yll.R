# Input data ####
load("../testing/testing_functions/test_get_yll.rda")
input_withPaf <- meta
min_age <- NA # before: 20
max_age <- 80 # before: also NA

# Function call ####
# output_function_call <- bestcost::get_yll(
#   pop_impact = pop_impact,
#   year_of_analysis = year_of_analysis,
#   min_age = min_age,
#   max_age = max_age,
#   meta = input_withPaf,
#   corrected_discount_rate = corrected_discount_rate)
# 
# print(output_function_call[["yll"]][["impact"]])

# Function code ####
lifeyears_byYear <- list()
yll_by_list<-list()

discount_factor <- corrected_discount_rate + 1

# Calculate YLL ####
for(s in sex){
  for (v in ci){
    
    ## Sum life years by year (result is data frame with 2 columns "year" & "impact" [which contains YLL]) ####
    lifeyears_byYear[[s]][[v]] <-
       pop_impact[["pop_impact"]][[s]][[v]] %>% 
       #pop_impact[["popOverTime"]][[s]][[v]] %>%
    
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
      # Sum over all ages (i.e. vertically) that fulfill inputted "max_age" and "min_age" criteria
      dplyr::summarize_all(sum, na.rm = TRUE) #%>% # The rows in each column are summed and the sum replaces the existing columns values (output is data frame with 1 row)
      # Reshape data frame to long format (output is data frame with 2 columns "year" & "impact")
      # tidyr::pivot_longer(cols = starts_with("population_"),
      #                     names_to = "year",
      #                     values_to = "impact", # Summed YLLs are saved in the column "impact"
      #                     names_prefix = "population_")

    ## Calculate total, not discounted YLL (single number) per sex & ci ####
    # yll_by_list[[s]][[v]][["noDiscount"]] <-
    #   lifeyears_byYear[[s]][[v]] %>%
    #   # Sum among years to obtain the total impact (single value)
    #   dplyr::summarise(impact = sum(impact), .groups = 'drop')
    # 
    # 
    # 
    # ## Calculate total, discounted life years (single value) per sex & ci ####
    # yll_by_list[[s]][[v]][["discounted"]] <-
    #   lifeyears_byYear[[s]][[v]]%>%
    #   # Convert year to numeric
    #   dplyr::mutate(year = as.numeric(year)) %>%
    #   # Calculate discount rate for each year
    #   dplyr::mutate(discount = 1/(discount_factor^(year-(year_of_analysis+1)))) %>%
    #   # Calculate life years discounted
    #   dplyr::mutate(discounted_impact = impact*discount) %>%
    #   # Sum among years to obtain the total impact (single value)
    #   dplyr::summarise(impact = sum(discounted_impact), .groups = 'drop')
  }
}
print(lifeyears_byYear[["female"]][["mean"]][1])

# Filtering gives the same results
test_popOverTime <- pop_impact[["popOverTime"]][["female"]][["mean"]] %>% filter(age >= min_age) %>% dplyr::select(., contains("population_")) %>%
  # Remove the year of analysis (we are only interested in the following ones)
  dplyr::select(., -contains(as.character(year_of_analysis)))

test_pop_impact <- pop_impact[["pop_impact"]][["female"]][["mean"]] %>%  filter(age >= min_age) %>% dplyr::select(., contains("population_")) %>%
  # Remove the year of analysis (we are only interested in the following ones)
  dplyr::select(., -contains(as.character(year_of_analysis)))

# Data wrangling ####
# Convert list into data frame
yll_by <-
  yll_by_list%>%
  purrr::map(map, dplyr::bind_rows, .id = "discount")%>%
  purrr::map(dplyr::bind_rows, .id = "ci" )%>%
  dplyr::bind_rows(., .id = "sex")


## Compile information needed for detailed YLL results ####
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
print(output[["yll"]][["impact"]])

