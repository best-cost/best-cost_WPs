##
from osgeo import gdal
import geopandas as gpd
import pandas as pd
import numpy as np
from pathlib import Path

## INPUTS

data_folder = Path(r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Data')
depr_path = data_folder / r'Deprivation\ilc_mdsd18_linear.csv'
nuts21_path = data_folder / r'Geographical_units\NUTS\NUTS_RG_01M_2021_3035.shp'
nuts16_path = data_folder / r'Geographical_units\NUTS\NUTS_RG_01M_2016_3035.shp'
nuts13_path = data_folder / r'Geographical_units\NUTS\NUTS_RG_01M_2013_3035.shp'
pop_path = data_folder / r'Population\NUTS\demo_r_pjangrp3_linear.csv'
mort_path = data_folder / r'Mortality/hlth_cd_asdr2_linear.csv'
morb_inp_path = data_folder / r'Morbidity/hlth_co_disch2t_linear.csv'
morb_dayp_path = data_folder / r'Morbidity/hlth_co_disch4t_linear.csv'

## OUTPUT

out_path = Path(r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\BEST_COST_WP3_Mapping_data.gpkg')

## PARAMS

crs_eu = 'EPSG:3035'

## NUTS

print('NUTS')

nuts21 = gpd.read_file(nuts21_path)
nuts16 = gpd.read_file(nuts16_path)
nuts13 = gpd.read_file(nuts13_path)

# print(nuts21.columns)
# print(nuts16.columns)
# print(nuts13.columns)

nuts16.drop('FID',
    axis=1,
    inplace=True)
nuts13.drop('FID',
    axis=1,
    inplace=True)

nuts21.to_file(out_path,
    driver='GPKG',
    layer='NUTS_2021')
nuts16.to_file(out_path,
    driver='GPKG',
    layer='NUTS_2016')
nuts13.to_file(out_path,
    driver='GPKG',
    layer='NUTS_2013')

## POPULATION

print('POPULATION')

pop = pd.read_csv(pop_path)
columns = pop.columns.values.astype(str)
columns = np.char.upper(columns)
columns[-4] = 'NUTS_ID'
pop.columns = columns
pop['geom'] = None
pop = gpd.GeoDataFrame(pop,
    geometry=[None] * len(pop),
    crs=crs_eu)
pop.to_file(out_path,
    driver='GPKG',
    layer='POPULATION')

## MORTALITY

print('MORTALITY')

mort = pd.read_csv(mort_path)
columns = mort.columns.values.astype(str)
columns = np.char.upper(columns)
columns[-4] = 'NUTS_ID'
mort.columns = columns
mort['geom'] = None
mort = gpd.GeoDataFrame(mort,
    geometry=[None] * len(mort),
    crs=crs_eu)
mort.to_file(out_path,
    driver='GPKG',
    layer='MORTALITY')

## MORBIDITY

print('MORBIDITY')

morb_inp = pd.read_csv(morb_inp_path)
columns = morb_inp.columns.values.astype(str)
columns = np.char.upper(columns)
columns[-4] = 'NUTS_ID'
morb_inp.columns = columns
morb_inp['geom'] = None

morb_dayp = pd.read_csv(morb_dayp_path)
columns = morb_dayp.columns.values.astype(str)
columns = np.char.upper(columns)
columns[-4] = 'NUTS_ID'
morb_dayp.columns = columns
morb_dayp['geom'] = None

# concatenate the inpatient and day case dataframes
morb_inp['TYPE'] = 'INPATIENTS'
morb_dayp['TYPE'] = 'DAY CASES'
morb = gpd.GeoDataFrame(
    pd.concat([morb_inp, morb_dayp],
        axis=0,
        ignore_index=True),
    geometry=[None] * (len(morb_inp) + len(morb_dayp)),
    crs=crs_eu
    )

morb.to_file(out_path,
    driver='GPKG',
    layer='MORBIDITY')

## DEPRIVATION

print('DEPRIVATION')

depr = pd.read_csv(depr_path)
columns = depr.columns.values.astype(str)
columns = np.char.upper(columns)
columns[-4] = 'NUTS_ID'
depr.columns = columns
depr['geom'] = None
depr = gpd.GeoDataFrame(depr,
    geometry=[None] * len(depr),
    crs=crs_eu)
depr.to_file(out_path,
    driver='GPKG',
    layer='DEPRIVATION')