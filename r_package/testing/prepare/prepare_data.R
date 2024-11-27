## script to generate data to test prepare_... function

library(terra)
library(sf)

bxl_pm25 <- terra::rast("./data/prepare/pm25_brussels_region_2020.tif")
sect <- sf::st_read("./data/prepare/statistical_sectors_brussels_region.gpkg")
pop <- read_excel("./data/prepare/Populatie SS 1-1-2020.xlsx")

sect <- sect[, c('CS01012020', 'CNIS5_2020')]
pop <- pop[, c("CD_SECTOR", 'CD_REFNIS', 'TOTAL')]
bxl_sect_pop <- merge(
  sect,
  pop,
  by.x = 'CS01012020',
  by.y = 'CD_SECTOR'
)

names(bxl_sect_pop)[names(bxl_sect_pop) == 'CS01012020'] <- 'sect_id'
names(bxl_sect_pop)[names(bxl_sect_pop) == 'CNIS5_2020'] <- 'mun_id'
bxl_sect_pop$CD_REFNIS <- NULL
names(bxl_sect_pop)[names(bxl_sect_pop) == 'TOTAL'] <- 'population'

rm(pop)
rm(sect)

save.image(file='../testing/input/data/input_data_for_testing_Rpackage.RData')
