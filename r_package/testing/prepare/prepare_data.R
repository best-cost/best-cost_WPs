## script to generate data to test prepare_... function

library(terra)
library(sf)
library(data.table)

## Brussels data
grid_bxl_pm25 <- terra::rast("./data/prepare/pm25_brussels_region_2020.tif")
sect <- sf::st_read("./data/prepare/statistical_sectors_brussels_region.gpkg")
pop <- read_excel("./data/prepare/Populatie SS 1-1-2020.xlsx")

sect <- sect[, c('CS01012020', 'CNIS5_2020')]
pop <- pop[, c("CD_SECTOR", 'CD_REFNIS', 'TOTAL')]
pop <- pop %>% group_by(CD_SECTOR) %>% summarise(POPULATION = sum(TOTAL))
pop <- pop %>% dplyr::filter(substr(CD_SECTOR, 6, 9) != "ZZZZ") # Filter out population that cannot be located (sector ID = "ZZZZ")
sf_bxl_pop <- left_join(sect, pop, by = c('CS01012020' = 'CD_SECTOR'))
sf_bxl_pop$POPULATION <- nafill(sf_bxl_pop$POPULATION, fill = 0L)

names(sf_bxl_pop)[names(sf_bxl_pop) == 'CS01012020'] <- 'sect_id'
names(sf_bxl_pop)[names(sf_bxl_pop) == 'CNIS5_2020'] <- 'mun_id'
names(sf_bxl_pop)[names(sf_bxl_pop) == 'POPULATION'] <- 'population'

rm(pop)
rm(sect)

## Europe data (netCDF)
grid_eur_airpoll <- terra::rast("./data/prepare/sce0.nc")

## save .RData file
save.image(file='../testing/input/data/input_data_for_testing_Rpackage.RData')
