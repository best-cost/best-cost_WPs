library(readxl) ; library(stringr)

disability_weights <- read_excel("J:/EEH/QHIAS/2023_BEST-COST/WP4/Literature/YLD/Disability weights/IHME_GBD_2019_DISABILITY_WEIGHTS_Y2020M010D15.XLSX", skip = 1)

# Remove rows without disability weights
disability_weights <- disability_weights[!(disability_weights$`Disability Weight`=="--"),] # 290 rows removed

# Extract disability weight central estimate
disability_weights$dw_central <- 0
for (i in 1:nrow(disability_weights)){
  disability_weights$dw_central[i] <- str_split_1(string = disability_weights$`Disability Weight`[i], pattern = "\r")[1]  %>% as.numeric()
}

# Extract disability weight lower estimate
disability_weights$dw_lowci <- 0
for (i in 1:nrow(disability_weights)){
  disability_weights$dw_lowci[i] <- as.numeric(str_split_1(string = str_split_1(string = str_split_1(string = disability_weights$`Disability Weight`[i], pattern = "\n")[2], pattern = "-")[1], pattern = fixed("("))[2])
}

# Extract disability weight upper estimate
disability_weights$dw_highci <- NA
for (i in 1:nrow(disability_weights)){
  disability_weights$dw_highci[i] <- as.numeric(str_sub(str_split_1(str_split_1(string = disability_weights$`Disability Weight`[i], pattern = fixed("("))[2], pattern = fixed("-"))[2], end = -2))

  # This (fractured) approach below didn't work)
  #temp <- str_split_1(string = disability_weights$`Disability Weight`[i], pattern = fixed("("))[2]
  #temp <  str_split_1(string = temp, pattern = fixed("-"))[2]
  #disability_weights$dw_highci[i] <- as.numeric(str_sub(temp[1], end = -2))
}

save(disability_weights, file = "../bestcost/data/disability_weights.rda")
