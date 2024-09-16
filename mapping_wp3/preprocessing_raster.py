##
from osgeo import gdal
from osgeo import gdalconst
import geopandas as gpd
import pandas as pd
import numpy as np
from pathlib import Path
from tqdm import tqdm
import rasterio
import copy
from rasterio.mask import mask
from rasterstats import zonal_stats
import matplotlib.pyplot as plt

## INPUTS

data_folder = Path(r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Data')
pop21_path = data_folder / r'Population\GRID\ESTAT_OBS-VALUE-T_2021_V1-0.tiff'
pop18_path = data_folder / r'Population\GRID\JRC_1K_POP_2018.tif'
noise_path = data_folder / r'Noise_pollution\EEA_QSI_100Mgrid_2016.tif'
air_pollutants = [
    'NO2',
    'O3',
    'PM25'
]
air_pollutant_indicators = [
    'avg',
    'somo35',
    'avg'
]
air_pollutant_who_thresholds = [
    10,
    6000,  # this threshold is actually based on EEA work, not WHO
    5
]
air_pollutant_years = np.arange(2015, 2022).astype(str)
air_pollutant_paths = []

for year in air_pollutant_years:
    for pollutant, indicator in zip(air_pollutants, air_pollutant_indicators):
        air_pollutant_paths.append(data_folder / r'Air_pollution\{}\EEA_1kmgrid_{}_{}_{}.tif'.format(pollutant, year, pollutant.lower(), indicator))

air_pollutant_res_paths = [p.with_name(p.stem + '_resampled.tif') for p in air_pollutant_paths]

## OUTPUT

out_path_gpkg = Path(r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\BEST_COST_WP3_Mapping_data.gpkg')

## PARAMS

crs_eu = 'EPSG:3035'

## FUNCTIONS

def resample_raster(input_raster_path, target_raster_path, output_raster_path,
        resample_alg=gdalconst.GRA_Bilinear,
        out_gdal_dtype=gdal.GDT_Float32):
    
    # Open input raster
    input_raster = gdal.Open(input_raster_path, gdalconst.GA_ReadOnly)
    if input_raster is None:
        print("Error: Could not open input raster.")
        return
    
    # Open target raster
    target_raster = gdal.Open(target_raster_path, gdalconst.GA_ReadOnly)
    if target_raster is None:
        print("Error: Could not open target raster.")
        return
    
    # Get target raster information
    target_geotransform = target_raster.GetGeoTransform()
    target_projection = target_raster.GetProjection()
    target_cols = target_raster.RasterXSize
    target_rows = target_raster.RasterYSize
    
    # Create output raster
    driver = gdal.GetDriverByName('GTiff')
    output_raster = driver.Create(output_raster_path, target_cols, target_rows, 1, out_gdal_dtype)
    output_raster.SetGeoTransform(target_geotransform)
    output_raster.SetProjection(target_projection)
    
    # Perform resampling
    gdal.ReprojectImage(input_raster, output_raster, None, None, resample_alg)
    
    # Close datasets
    input_raster = None
    target_raster = None
    output_raster = None
    
## GET CORRECTED 2021 POPULATION RASTER

# resample the 2018 image to the 2021 grid
pop18_res_path = pop18_path.with_name(pop18_path.stem + '_resampled.tif')
pop21_corr_path = pop21_path.with_name(pop21_path.stem + '_corrected.tif')
print(pop18_res_path)

resample_raster(pop18_path, pop21_path, pop18_res_path,
    out_gdal_dtype=gdal.GDT_Int32)

# replace missing values in the 2021 image with values from the resampled 2018 image
pop21 = gdal.Open(pop21_path)
pop21_array = pop21.ReadAsArray().astype(int)

pop18_res = gdal.Open(pop18_res_path)
pop18_res_array = pop18_res.ReadAsArray().astype(int)

con = pop21_array < 0
pop21_array[con] = pop18_res_array[con]

# Get target raster information
target_geotransform = pop21.GetGeoTransform()
target_projection = pop21.GetProjection()
target_cols = pop21.RasterXSize
target_rows = pop21.RasterYSize

# Create output raster
driver = gdal.GetDriverByName('GTiff')
output_raster = driver.Create(pop21_corr_path, target_cols, target_rows, 1, gdal.GDT_Int32)
output_raster.SetGeoTransform(target_geotransform)
output_raster.SetProjection(target_projection)
band = output_raster.GetRasterBand(1)
band.WriteArray(pop21_array.squeeze())
output_raster = None

## RESAMPLE THE OTHER RASTERS

noise_res_path = noise_path.with_name(noise_path.stem + '_resampled.tif')
resample_raster(noise_path, pop21_path, noise_res_path)

for air_pollutant_path in tqdm(air_pollutant_paths):
    
    air_pollutant_path_out = air_pollutant_path.with_name(air_pollutant_path.stem + '_resampled.tif')
    resample_raster(air_pollutant_path, pop21_path, air_pollutant_path_out)

## CALCULATE THE 2021 ZONAL STATISTICS ON POPULATION WEIGHTED POLLUTION AND POPULATION EXPOSED TO POLLUTION ABOVE WHO THRESHOLDS

nuts = gpd.read_file(out_path_gpkg, layer='NUTS_2021')

stats=['min', 'max', 'mean', 'median', 'sum', 'std']

year = '2021'
air_pollutant_res_paths = []
for pollutant, indicator in zip(air_pollutants, air_pollutant_indicators):
    air_pollutant_res_paths.append(data_folder / r'Air_pollution\{}\EEA_1kmgrid_{}_{}_{}_resampled.tif'.format(pollutant, year, pollutant.lower(), indicator))

# get the population raster
pop21_corr_path = pop21_path.with_name(pop21_path.stem + '_corrected.tif')
src_pop = rasterio.open(pop21_corr_path)
transform = src_pop.transform
raster_pop = src_pop.read(1)
raster_pop[raster_pop < 0] = -1

# get the population zonal statistics
zs_pop = zonal_stats(nuts, raster_pop,
    affine=transform,
    stats=['sum'],
    nodata=-1)

zs_pop = np.array([z['sum'] for z in zs_pop]).astype(float)
zs_pop[np.isnan(zs_pop)] = 1.0
zs_pop[zs_pop == 0.0] = 1.0

# noise pollution and exposure
print('NOISE')
noise_res_path = noise_path.with_name(noise_path.stem + '_resampled.tif')
src_noise = rasterio.open(noise_res_path)
raster_noise = src_noise.read(1)

noise_exposure = raster_pop * (1 - raster_noise)  # complement of QSI
noise_exposure[noise_exposure < 0] = -1
raster_noise[raster_noise < 0] = 1

zs = zonal_stats(nuts, raster_noise,
    affine=transform,
    stats=stats,
    nodata=-1)

for stat in stats:
    nuts['QSIC_{}_{}'.format(year, stat.upper())] = [z[stat] for z in zs]

zs_exp = zonal_stats(nuts, noise_exposure,
    affine=transform,
    stats=['sum'],
    nodata=-1)

zs_exp = np.array([z['sum'] for z in zs_exp]).astype(float)
zs_exp_nan_mask = np.isnan(zs_exp)
zs_exp[zs_exp_nan_mask] = 0.0
zs_exp = zs_exp / zs_pop
zs_exp[zs_exp_nan_mask] = np.nan
nuts['QSIC_{}_PWP'.format(year)] = zs_exp

# air pollution and exposure

for pollutant, threshold, path in zip(air_pollutants, air_pollutant_who_thresholds, air_pollutant_res_paths):
    
    print(pollutant)
    
    src_pollutant = rasterio.open(path)
    raster_pollutant = src_pollutant.read(1)
    raster_pollutant[raster_pollutant < 0] = -1
    
    pollutant_exposure = raster_pop * raster_pollutant
    pollutant_exposure[pollutant_exposure < 0] = -1
    
    pollutant_threshold_mask = np.where(raster_pollutant >= threshold, 1.0, 0.)
    pollutant_threshold_exposure = raster_pop * pollutant_threshold_mask
    pollutant_threshold_exposure[pollutant_threshold_exposure < 0] = -1
    
    zs = zonal_stats(nuts, raster_pollutant,
        affine=transform,
        stats=stats,
        nodata=-1)

    for stat in stats:
        nuts['{}_{}_{}'.format(pollutant, year, stat.upper())] = [z[stat] for z in zs]
    
    zs_exp = zonal_stats(nuts, pollutant_exposure,
        affine=transform,
        stats=['sum'],
        nodata=-1)
    zs_exp = np.array([z['sum'] for z in zs_exp]).astype(float)
    zs_exp_nan_mask = np.isnan(zs_exp)
    zs_exp[zs_exp_nan_mask] = 0
    zs_exp = zs_exp / zs_pop
    zs_exp[zs_exp_nan_mask] = np.nan

    nuts['{}_{}_PWP'.format(pollutant, year)] = zs_exp
        
    zs_thres = zonal_stats(nuts, pollutant_threshold_exposure,
        affine=transform,
        stats=['sum'],
        nodata=-1)
    zs_thres = np.array([z['sum'] for z in zs_thres]).astype(float)
    zs_thres_nan_mask = np.isnan(zs_thres)
    zs_thres[zs_thres_nan_mask] = 0
    zs_thres = zs_thres / zs_pop
    zs_thres[zs_thres_nan_mask] = np.nan

    nuts['{}_{}_PAT'.format(pollutant, year)] = zs_thres

nuts.to_file(out_path_gpkg,
    driver='GPKG',
    layer='POLLUTION')
