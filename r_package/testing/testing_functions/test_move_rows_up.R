# Load input data
load("../testing/testing_functions/test_move_rows_up.rda")

# Start of the loop
year_loopStart <- year_of_analysis+1
# End of the loop
year_loopEnd <- year_of_analysis + nrow(popOTime) - 1

years <- year_loopStart:year_loopEnd

output <- popOTime

for(i in (1:length(years))){
  # Calculate population for the next years without considering the effect of air pollution
  # Calculate population in the next years based on the row above
  
  y <- years[i]
  
  output[, paste0("population_", y)] <-
    dplyr::lead(output[, paste0("population_", y)], n = i)
  
}

# Age as numeric
output$age <- as.numeric(output$age)
