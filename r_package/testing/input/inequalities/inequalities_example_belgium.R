### PM2.5 BURDEN INEQUALITIES

## required packages
library(dplyr)

## import burden
mort <-
  read.csv("../testing/input/inequalities/mort_pm25_sector_2019.csv")

## subset Flanders (NUTS1 = BE2)
mort <- subset(
  mort,
  NUTS1 == 'BE2' &
    POPULATION > 0
)

## import derpivation index
bimd <- read.csv("../testing/input/inequalities/BIMD_2011_WITHOUT_HEALTH_ELLIS_WIDE.csv")

## merge
mort_bimd <-
  merge(mort, bimd, by.x = "CS01012020", by.y = "CD_RES_SECTOR")

## exposure and attributable burden per deprivation decile
exposure_mean_bimd <-
  with(mort_bimd, tapply(PM25_MEAN, deciles, mean))
exposure_mean_bimd

mort_attr_rt_bimd <-
1e5 * with(mort_bimd, tapply(MORTALITY_ATTR, deciles, sum))/
  with(mort_bimd, tapply(POPULATION, deciles, sum))
mort_attr_rt_bimd 

mort_total_rt_bimd <-
1e5 * with(mort_bimd, tapply(MORTALITY_TOTAL, deciles, sum))/
  with(mort_bimd, tapply(POPULATION, deciles, sum))
mort_total_rt_bimd 

## inequalities
exposure_mean_bimd[1] - exposure_mean_bimd[10] ## absolute diff
100 * ( (exposure_mean_bimd[1] / exposure_mean_bimd[10]) - 1 ) ## relative diff
100 * (mean(mort_bimd$PM25_MEAN) - exposure_mean_bimd[10]) / mean(mort_bimd$PM25_MEAN) # PAF

mort_attr_rt_bimd[1] - mort_attr_rt_bimd[10] ## absolute diff
100 * ( (mort_attr_rt_bimd[1] / mort_attr_rt_bimd[10]) - 1 ) ## relative diff

mort_attr_rt_mean <-
  (1e5 * sum(mort_bimd$MORTALITY_ATTR) / sum(mort_bimd$POPULATION))

100 * (mort_attr_rt_mean - mort_attr_rt_bimd[10]) / mort_attr_rt_mean # PAF

