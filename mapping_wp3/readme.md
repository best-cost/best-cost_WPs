Scripts used for the mapping proposed in BEST-COST WP3
======================================================

Overview
--------

This folder contains the Python scripts, SQL scripts and QGIS project file used to perform the cartographic mapping of health, environmental stressors and deprivation data, as outlined in WP3 of the BEST-COST project. Practically, the bulk of the mapping is done here using QGIS, whereas the included Python scripts mainly serve to preprocess and consolidate the used spatial data and to generate support graphs.

As can be seen in the preprocessing scripts, all used spatial data are organized as layers in a single geopackage, which is essentially an SQLite database file. The QGIS project file contains several SQL scripts that query, combine and enhance these data to make them suitable for specific map products. The SQL scripts can be accessed via the SQL Window in the DB manager add-on when opening the QGIS project. Copies of these SQL scripts have also been included as sql-files in the subfolder 'Queries', because the Python script generating the support graphs draws on these files to reproduce the data mapped in QGIS.

For more information on the challenges and the proposed methodological framework that underpin this work, we refer you to the report of Task 3.2 that can also be found on the BEST-COST WP3 SharePoint.

Included scripts
----------------
This folder contains the following 5 Python scripts. It is recommended to run these scripts in an environment supporting code cells, to complete the workflow piecewise.

1. *preprocessing_vector.py*: script for minor preprocessing and importing of vector GIS/tabular statistical data into the geopackage.
2. *preprocessing_raster.py*: script for preprocessing of raster GIS-data, by transforming them into zonal statistics and derived indices on NUTS0-3 level, and importing into the geopackage.
3. *plot_functions.py*: definition of the custom plot functions used in *bivariate_legend.py* and *support_graphs.py*.
4. *bivariate_legend.py*: script used to produce correct legends for bivariate choropleth maps, a feature that is currently still poorly supported in QGIS.
5. *support_graphs.py*: script used to generate various support graphs, providing additional statistical information/alternate perspectives on the mapped data.

The QGIS project file *BEST_COST_WP3_Mapping.qgz* contains the various map layouts and SQL scripts that are used to query the geopackage. The content of this project can be explored interactively when opened in QGIS. Please note that the project's content is extensive, so fully loading the project in QGIS may take some time.

The 'Queries' folder contains copies of the 7 SQL scripts, that are also present in the QGIS project file. Each of these scripts caters to a specific mapping example that was elaborated to illustrate the data availability scenarios of our proposed mapping framework (see report for more info):

1. *SC1B_NO2_PAT_NUTS3.sql*
2. *SC2A_MORT_NUTS1.sql*
3. *SC2B_MORT_TYPE_NUTS1.sql*
4. *SC3A_MORT_NO2EXP_NUTS2.sql*
5. *SC4A_MORT_NUTS1.sql*
6. *SC4A_NOISE_NUTS3.sql*
7. *SC5A_MORT_NO2EXP_DEPR_NUTS2.sql*

These SQL scripts draw on the SQLite dialect.

Software dependencies
---------------------

- QGIS
- Python (10+)
    - matplotlib
    - numpy
    - gdal
    - pandas
    - geopandas
    - rasterio
    - sqlalchemy
    - 
    
How to access the mapped data
-----------------------------

The geopackage containing the mapped spatial data can be downloaded from the BEST-COST WP3 SharePoint via [this link](https://ugentbe.sharepoint.com/:u:/r/teams/Group.PR202302461/Gedeelde%20documenten/WP3%20INEQUALITIES/Task%203.2/BEST_COST_WP3_Mapping_data.gpkg?csf=1&web=1&e=gEVg2E), assuming you have the necessary permissions. <ins>Please copy this geopackage to the same folder that contains the QGIS project file</ins>, to avoid breaking the relative file paths.

Besides the mapped statistical data, we've also used several background layer of country and sea borders to improve the layout of the maps and provide geographic context. A compressed folder containing the shapefiles of these background layers can also be downloaded from the BEST-COST WP3 SharePoint via [this link](https://ugentbe.sharepoint.com/:u:/r/teams/Group.PR202302461/Gedeelde%20documenten/WP3%20INEQUALITIES/Task%203.2/Background.zip?csf=1&web=1&e=8PTY43). Again, <ins>please copy and extract this zip-file in the same folder that contains the QGIS project file</ins>.

Additional remarks
------------------

The file paths specified in the Python scripts may have to be manually updated when executing this code on your machine.

The original sources of the used spatial and statistical datasets are listed in the Task 3.2 report.

The database connection to the geopackage should already be configured in the DB manager of QGIS when opening the QGIS project. However, if needed the connection can be restored manually and the associated SQL scripts can also be imported.
