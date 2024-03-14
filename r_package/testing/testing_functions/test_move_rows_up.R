# Load input data
load("../testing/testing_functions/test_move_rows_up.rda")
popOTime <- popOverTime
rm(popOverTime)

# Function call ####
output_function_call <- move_rows_up(popOTime = popOTime,
                                     year_of_analysis = year_of_analysis)

# Function test ####
## OLD CODE ####
# # Start of the loop
# year_loopStart <- year_of_analysis+1
# # End of the loop
# year_loopEnd <- year_of_analysis + nrow(popOTime) - 1
# 
# years <- year_loopStart:year_loopEnd
# 
# output <- popOTime
# 
# for(i in (1:length(years))){
#   
#   y <- years[i]
#   
#   output[, paste0("population_", y)] <-
#     dplyr::lead(output[, paste0("population_", y)], n = i)
#   
# }
# 
# # Age as numeric
# output$age <- as.numeric(output$age) # NOTE: IT WAS ALREADY NUMERIC #####

# NEW CODE ####
years <- c( (year_of_analysis+1) : (year_of_analysis + nrow(popOTime) - 1) ) # vector indicating which year columns have to be shifted upwards

for(i in (1:length(years))){ # i.e. length(year_loopStart:year_loopEnd)

  y <- years[i]
  
  popOTime[, paste0("population_", y)] <-
    dplyr::lead(popOTime[, paste0("population_", y)], n = i)
  
}

report(popOTime)