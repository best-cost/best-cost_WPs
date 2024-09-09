## MODULES
from osgeo import gdal
import pandas as pd
import numpy as np
from pathlib import Path
from tqdm import tqdm
import rasterio
from rasterio.mask import mask
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from sqlalchemy import create_engine
from plot_functions import create_interpolated_cmap, histogram, barchart, scatterplot


## PATHS

gpkg_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\BEST_COST_WP3_Mapping_data.gpkg'
connection_str = r'sqlite:///{}'.format(gpkg_path)
engine = create_engine(connection_str)


## SCENARIO 1A (HISTOGRAM)

image_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Data\Air_pollution\NO2\EEA_1kmgrid_2021_no2_avg_resampled.tif'
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario1_hist.png'

ds = gdal.Open(image_path)
image = ds.ReadAsArray()
x = image[image > 0]
cmap = plt.get_cmap('RdBu_r')
title = 'NO$_2$ air pollution in the EU27+CC+UK (2021)'
xlabel = 'Interpolated average concentration ($\mu$g / m$^3$)'
ylabel = r'Fraction of land surface (%)'
classborders = [0, 2.5, 5, 7.5, 10, 20]
avg = np.round(float(x.mean()), 1)
ax = plt.gca()
histogram(x,
    ax=ax,
    bins=20,
    density=True,
    scale_n=100.0,
    classborders=classborders,
    classborders_plotlines=False,
    classborder_linestyle='-',
    line=avg,
    line_label=' EU average = {} $\mu$g / m$^3$'.format(avg),
    line_labelsize=9,
    line_pos=0.13,
    title=title,
    xlabel=xlabel,
    ylabel=ylabel,
    axis_fontsize=11,
    cmap=cmap,
    background_color='0.95',
    show=False)
plt.tight_layout()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')


## SCENARIO 1A (BAR CHART NATIONAL AVERAGE +- STD)

cmap = plt.get_cmap('RdBu_r')
title = 'NO$_2$ air pollution by country in the EU27+CC+UK (2021):\nAverage $\pm$ Standard deviation'
xlabel = 'Interpolated concentration ($\mu$g / m$^3$)'
classborders = [0, 2.5, 5, 7.5, 10, 20]
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario1a_bar.png'

# perform a small query on the POLLUTION table in the geopackage that already contains the zonal statistics on NUTS0 level
sql_query = """
    SELECT
        NUTS_ID,
        LEVL_CODE,
        CNTR_CODE,
        NO2_2021_MEAN,
        NO2_2021_STD
    FROM
        POLLUTION
    WHERE
        LEVL_CODE = 0
    ORDER BY
        NO2_2021_MEAN DESC
    """

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)

barpos = range(df.shape[0]) 
barsize = df['NO2_2021_MEAN'].values
avg = barsize.mean()
std = barsize.std()
err = df['NO2_2021_STD'].values
n_countries = df.shape[0]
xtick_labels = [c + ' (' + str(i + 1) + ')' for i, c in zip(range(n_countries), df['CNTR_CODE'].values)]

# add EU average on top with empty space below
barpos = np.arange(df.shape[0] + 1)
barpos += 1
barpos[0] = 0
barsize = [avg] + list(barsize)
err = [std] + list(err)
xtick_labels = ['EU'] + xtick_labels

# plt.figure(figsize=(10, 5))
plt.figure(figsize=(5, 8))
ax = plt.gca()
barchart(barpos, barsize,
    orientation='horizontal',
    ax=ax,
    xerr=err,
    ecolor='0.2',
    capsize=2,
    error_kw={'linewidth': 0.75},
    ytick_labels=xtick_labels,
    classborders=classborders,
    cmap=cmap,
    title=title,
    title_fontsize=11,
    xlabel=xlabel,
    axis_fontsize=11,
    background_color='0.95',
    line=10.01,  # small difference with 10 to avoid overlapping grid lines
    line_label=' WHO threshold =\n {} $\mu$g / m$^3$'.format(10),
    line_labelsize=9,
    line_color='0.5',
    line_linestyle='--',
    line_linewidth=1,
    invert_yaxis=True,
    show=False)
# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')

## SCENARIO 1B (HISTOGRAM)
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario1b_hist.png'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC1B_NO2_PAT_NUTS3.sql'

with open(query_path) as file:
    sql_query = file.readlines()

sql_query = ''.join(sql_query)

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)

x = df['no2_2021_pat'].values * 100
x = x[np.logical_not(np.isnan(x))]
avg = int(x.mean())
ax = plt.gca()

cmap = plt.get_cmap('PuOr_r')
title = 'Population in the EU27+CC+UK (2021) exposed to an\naverage yearly NO$_2$ air pollution $\geq$ 10 $\mu$g / m$^3$ (WHO threshold)'
xlabel = 'Fraction of the local NUTS3 population (%)'
ylabel = r'Number of NUTS3 regions'
classborders = [0, 35, 65, 85, 95, 100]

ax = plt.gca()
histogram(x,
    ax=ax,
    bins=20,
    classborders=classborders,
    classborders_plotlines=False,
    line=avg,
    line_label='EU average = {} %  '.format(avg),
    line_labelsize=9,
    line_ha='right',
    line_pos=270,
    # line_pos=0.13,
    title=title,
    xlabel=xlabel,
    ylabel=ylabel,
    axis_fontsize=11,
    cmap=cmap,
    background_color='0.95',
    show=False)
plt.tight_layout()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')


## SCENARIO 1B (BAR CHART NATIONAL AVERAGE)

title = 'Population by country in the EU27+CC+UK (2021) exposed to an\naverage yearly NO$_2$ air pollution $\geq$ 10 $\mu$g / m$^3$ (WHO threshold)'
xlabel = 'Fraction of population (%)'
cmap = plt.get_cmap('PuOr_r')
classborders = [0, 35, 65, 85, 95, 100.000001]
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario1b_bar.png'

# query the national-level pollution exposure data
sql_query = """
    select
        p.nuts_id,
        p.no2_2021_mean,
        case
            when n.cntr_code = 'TR' then NULL  -- no pop grid coverage of Turkey
            else p.no2_2021_pat
        end as no2_2021_pat,
        n.geom,
        n.levl_code,
        n.cntr_code,
        n.nuts_name
    from
        pollution as p,
        nuts_2021 as n
    where
        p.nuts_id = n.nuts_id and
        n.levl_code = 0
        and n.cntr_code != 'TR' -- no complete data coverage for Turkey
    order by
        no2_2021_pat desc
    """
    
with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
barpos = range(df.shape[0])
barsize = df['no2_2021_pat'].values * 100
barsize = barsize[np.logical_not(np.isnan(barsize))]
n_countries = df.shape[0]
ytick_labels = [c + ' (' + str(i + 1) + ')' for i, c in zip(range(n_countries), df['CNTR_CODE'].values)]

# add EU average on top with empty space below
avg = barsize.mean()
barpos = np.arange(df.shape[0] + 1)
barpos += 1
barpos[0] = 0
barsize = [avg] + list(barsize)
ytick_labels = ['EU average'] + ytick_labels

plt.figure(figsize=(5, 8))
ax = plt.gca()
barchart(barpos, barsize,
    ax=ax,
    orientation='horizontal',
    ytick_labels=ytick_labels,
    cmap=cmap,
    classborders=classborders,
    title=title,
    title_fontsize=11,
    xlabel=xlabel,
    axis_fontsize=11,
    background_color='0.95',
    invert_yaxis=True,
    show=False)
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')


## SCENARIO 2A (HISTOGRAM)

cmap = plt.get_cmap('Reds')
title = 'Standardized mortality due to malignant\nneoplasms in the EU27+CC in 2021'
xlabel = 'Age-weighted mortality rate (deaths / 100k inhabitants)'
ylabel = 'Number of NUTS1 regions'
classborders = [120, 210, 230, 250, 330]
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario2a_hist.png'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC2A_MORT_NUTS1.sql'

with open(query_path) as file:
    sql_query = file.readlines()

sql_query = ''.join(sql_query)

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
x = df['mortality_rate_cancer'].values
avg = int(x.mean())
ax = plt.gca()
histogram(x,
    ax=ax,
    classborders=classborders,
    bins=25,
    line=avg,
    line_label='  EU average = {}'.format(avg),
    line_labelsize=9,
    line_pos=16.5,
    title=title,
    xlabel=xlabel,
    ylabel=ylabel,
    axis_fontsize=11,
    cmap=cmap,
    cmap_cutoff=0,
    background_color='0.95',
    show=False)
ax.set_ylim(0, 18)
# plt.show()
plt.tight_layout()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')


## SCENARIO 2A (BAR CHART NATIONAL AVERAGE)

cmap = plt.get_cmap('Reds')
classborders = [120, 210, 230, 250, 330]
title = 'Standardized mortality by country due to\nmalignant neoplasms in the EU27+CC in 2021'
xlabel = 'Age-weighted mortality rate (deaths / 100k inhabitants)'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC2A_MORT_NUTS1.sql'
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario2a_bar.png'
    
with open(query_path) as file:
    sql_query = file.readlines()

for l, line in enumerate(sql_query):
    if 'n.levl_code = 1' in line:
        sql_query[l] = line.replace('n.levl_code = 1', 'n.levl_code = 0')
        
sql_query += [' order by m.obs_value desc']

sql_query = ''.join(sql_query)
    
with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
barpos = range(df.shape[0]) 
barsize = df['mortality_rate_cancer'].values
n_countries = df.shape[0]
ytick_labels = [c + ' (' + str(i + 1) + ')' for i, c in zip(range(n_countries), df['CNTR_CODE'].values)]

# # add EU average on top with empty space below
avg = barsize.mean()
barpos = np.arange(df.shape[0] + 1)
barpos += 1
barpos[0] = 0
barsize = [avg] + list(barsize)
ytick_labels = ['EU average'] + ytick_labels

plt.figure(figsize=(5, 8))
ax = plt.gca()
barchart(barpos, barsize,
    ax=ax,
    orientation='horizontal',
    ytick_labels=ytick_labels,
    classborders=classborders,
    cmap=cmap,
    cmap_cutoff=0,
    title=title,
    xlabel=xlabel,
    axis_fontsize=11,
    background_color='0.95',
    invert_yaxis=True,
    show=False)
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')

## SCENARIO 2B (HISTOGRAMS)

colors = [
    '0.5',
    '#ae3935',
    '#d1d13f',
    '#338fb4'
    ]
suptitle = 'Standardized mortality by cause of death in the EU27+CC in 2021'
titles = [
    'All causes of death',
    'Malignant neoplasms',
    'Diseases of the circulatory system',
    'Other causes of death'
    ]
xlabel = 'Age-weighted mortality rate (deaths / 100k inhabitants)'
ylabel = 'Number of NUTS1 regions'
# classborders = [120, 1800]
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario2b_hist.png'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC2B_MORT_TYPE_NUTS1.sql'

with open(query_path) as file:
    sql_query = file.readlines()

sql_query = ''.join(sql_query)

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
xvals = [
    df['mortality_rate_cancer'].values,
    df['mortality_rate_cardiovascular'].values,
    df['mortality_rate_other'].values
    ]
xvals = [xvals[0] + xvals[1] + xvals[2]] + xvals
avgs = [
    int(xvals[0].mean()),
    int(xvals[1].mean()),
    int(xvals[2].mean()),
    int(xvals[3].mean())
    ]
fig = plt.figure(figsize=(8, 7))
gs = fig.add_gridspec(3, 3,
    width_ratios=[0.05, 1, 1],
    height_ratios=[1, 1, 0.05],
    hspace=0.4)

plt_axes = [
    fig.add_subplot(gs[0, 1]),
    fig.add_subplot(gs[0, 2]),
    fig.add_subplot(gs[1, 1]),
    fig.add_subplot(gs[1, 2])
    ]

for n, (color, title, x, avg, ax) in enumerate(zip(colors, titles, xvals, avgs, plt_axes)):
        
    # if n == 0:
    #     line_ha = 'right'
    #     line_label = 'EU average = {}  '.format(avg)
    # else:
    #     line_ha = 'left' 
    #     line_label = '  EU average = {}'.format(avg) 
    
    histogram(x,
        ax=ax,
        bins=30,
        # classborders=classborders,
        line=avg,
        line_label='  EU average = {}'.format(avg),
        line_labelsize=9,
        line_pos=16,
        line_ha='left',
        title=title,
        title_fontsize=11,
        color=color,
        background_color='0.95',
        show=False)
    
    ax.set_yticks([0, 5, 10, 15, 20])
    ax.set_ylim(0, 20)

# shared y-label 
ax_ylabel = fig.add_subplot(gs[:2, 0])
ax_ylabel.text(0.5, 0.5, ylabel,
    rotation=90,
    fontsize=11,
    va='center',
    ha='center')
ax_ylabel.grid(False)
ax_ylabel.axis('off')

# shared x-label 
ax_xlabel = fig.add_subplot(gs[2, 1:])
ax_xlabel.text(0.5, 1.0, xlabel,
    fontsize=11,
    va='center',
    ha='center')
ax_xlabel.grid(False)
ax_xlabel.axis('off')

# super title
fig.suptitle(suptitle)
    
plt.tight_layout()
# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')


## SCENARIO 2B (BAR CHART NATIONAL AVERAGE)

title = 'Standardized mortality by country and\ncause of death in the EU27+CC in 2021'
xlabel = 'Age-weighted mortality rate (deaths / 100k inhabitants)'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC2B_MORT_TYPE_NUTS1.sql'
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario2b_bar.png'
color_cancer = '#ae3935'
color_cardio = '#d1d13f'
color_other = '#338fb4'
colors = [
    color_cancer,
    color_cardio,
    color_other
]
labels = [
    'Malignant neoplasms (cancer)',
    'Diseases of the circulatory system',
    'Other causes of death'
]

with open(query_path) as file:
    sql_query = file.readlines()

for l, line in enumerate(sql_query):
    if 'n.levl_code = 1' in line:
        sql_query[l] = line.replace('n.levl_code = 1', 'n.levl_code = 0')
        
sql_query += [' order by mortality_rate_total desc']

sql_query = ''.join(sql_query)
    
with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
barpos = range(df.shape[0]) 
barsize_cancer = df['mortality_rate_cancer'].values
barsize_cardio = df['mortality_rate_cardiovascular'].values
barsize_other = df['mortality_rate_other'].values
n_countries = df.shape[0]
ytick_labels = [c + ' (' + str(i + 1) + ')' for i, c in zip(range(n_countries), df['CNTR_CODE'].values)]

# add EU average on top with empty space below
avg_cancer = barsize_cancer.mean()
avg_cardio = barsize_cardio.mean()
avg_other = barsize_other.mean()
barpos = np.arange(df.shape[0] + 1)
barpos += 1
barpos[0] = 0
barsize_cancer = [avg_cancer] + list(barsize_cancer)
barsize_cardio = [avg_cardio] + list(barsize_cardio)
barsize_other = [avg_other] + list(barsize_other)
ytick_labels = ['EU average'] + ytick_labels

plt.figure(figsize=(5, 8))
ax = plt.gca()
barsizes = [barsize_cancer, barsize_cardio, barsize_other]
left = np.zeros(len(barsize_cancer))

for b, (barsize, color, label) in enumerate(zip(barsizes, colors, labels)):
    
    if b > 0:
        left += barsizes[b - 1]

    barchart(barpos, barsize,
        ax=ax,
        label=label,
        left=left,
        orientation='horizontal',
        ytick_labels=ytick_labels,
        color=color,
        title=title,
        xlabel=xlabel,
        axis_fontsize=11,
        background_color='0.95',
        invert_yaxis=True,
        show=False)

ax.legend(
    title='Cause of death',
    fontsize=8)

# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')

## SCENARIO 3 (SCATTERPLOT)

title = 'Standardized mortality due to DCS vs. NO$_2$ air pollution\nexposure in the EU27+CC (excl. Turkey) in 2021 on NUTS2-level'
xlabel = 'Age-weighted mortality rate due to Diseases\n of the Circulatory System [deaths / 100k inhabitants]'
ylabel = '% of population exposed to high NO$_2$\nair pollution (yearly mean $\geq$ 10 $\mu$g / m$^3$)'
classborders1 = [140, 240, 340, 1340]
classborders2 = [0.01, 60.01, 85, 100]
color_low1 = '#ebebff'
color_high1 = '#6fa4ff'
color_low2 = '#ffeee6'
color_high2 = '#ff5454'
classborder_linewidth = 0.5
classborder_color = '0.3'
classborder_linestyle = '--'
eurovoc_map = {
    'N-Europe': ['IS', 'DK', 'FI', 'NO', 'SE', 'EL', 'EE', 'LV', 'LT'],
    'Central & E-Europe': ['CZ', 'HR', 'HU', 'PL', 'RO', 'SI', 'SK', 'BA', 'MD', 'MK', 'AL', 'RS', 'ME', 'BG'],
    'S-Europe': ['EL', 'ES', 'IT', 'CY', 'MT', 'PT'],
    'W-Europe': ['BE', 'DE', 'IE', 'LU', 'FR', 'NL', 'CH', 'AT', 'UK', 'LI'],
    # 'Outside Europe': ['TR']
}
eurovoc_markers = ['^', '>', 'v', '<', 'X']
eurovoc_colors = ['blue', 'yellow', 'red', 'green', 'gray']
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario3_scatter.png'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC3A_MORT_NO2EXP_NUTS2.sql'

with open(query_path) as file:
    sql_query = file.readlines()

sql_query = ''.join(sql_query)

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)

df = df.loc[df['CNTR_CODE'].values != 'TR']

mort = df['mortality_rate_cardio'].values
depr = df['no2_2021_pat'].values
cntr = df['CNTR_CODE'].values

eurovoc = np.empty(len(cntr), dtype=np.dtypes.StrDType)

for region, countries in zip(eurovoc_map.keys(), eurovoc_map.values()):
    con = np.isin(cntr, countries)
    eurovoc[con] = region
    
eurovoc[eurovoc == None] = 'Outside Europe'

# print(cntr[eurovoc == 'Outside Europe'])

cmap1 = create_interpolated_cmap(color_low1, color_high1)
cmap2 = create_interpolated_cmap(color_low2, color_high2)

fig = plt.figure(figsize=(8, 8))
gs = fig.add_gridspec(2, 2,
    width_ratios=[0.2, 1],
    height_ratios=[1, 0.2])

ax_scatter =  fig.add_subplot(gs[0, 1])

for e, ev in enumerate(eurovoc_map.keys()):

    con = eurovoc == ev
    mort_tmp = mort[con]
    depr_tmp = depr[con]
    scatterplot(mort_tmp, depr_tmp,
        ax=ax_scatter,
        labels=ev,
        x_classborders=classborders1,
        y_classborders=classborders2,
        classborder_linewidth=classborder_linewidth,
        classborder_color=classborder_color,
        z_color=[eurovoc_colors[e]],
        x_cmap=cmap1,
        y_cmap=cmap2,
        size=15,
        marker=eurovoc_markers[e],
        # title=title,
        background_color='0.95',
        tick_size=0,
        tick_fontsize=0,
        grid=False,
        show=False
        )
    
ax_scatter.set_title(title, pad=15)

handles = []
for label, marker, col in zip(eurovoc_map.keys(), eurovoc_markers, eurovoc_colors):
    handles.append(
        plt.Line2D([0], [0],
            color='0.05',
            linewidth=0,
            linestyle='-',
            marker=marker,
            markersize=4,
            markerfacecolor=col,
            markeredgewidth=0.1,
            label=label)
        )

ax_scatter.legend(
    title='EuroVoc sub-region',
    title_fontsize=8,
    handles=handles,
    fontsize=7,
    bbox_to_anchor=(0.9, 0.1),
    bbox_transform=ax_scatter.transAxes,
    loc='lower right')

ax_hist_y = fig.add_subplot(gs[0, 0], sharey=ax_scatter)
histogram(depr,
    orientation='horizontal',
    ylabel=ylabel,
    axis_fontsize=11,
	ax=ax_hist_y,
	bins=30,
	classborders=classborders2,
	cmap=cmap2,
	background_color='0.95',
	invert_xaxis=True,
	show=False)

ax_hist_y.set_yticks(classborders2, minor=True)
ax_hist_y.set_xticks([10, 30], minor=False)
ax_hist_y.grid(
	visible=True,
	which='minor',
	axis='y',
	linewidth=classborder_linewidth,
	linestyle=classborder_linestyle,
	color=classborder_color,
	zorder=1
	)

ax_hist_x = fig.add_subplot(gs[1, 1], sharex=ax_scatter)
histogram(mort,
    xlabel=xlabel,
    axis_fontsize=11,
	ax=ax_hist_x,
	bins=30,
	classborders=classborders1,
	cmap=cmap1,
	background_color='0.95',
	invert_yaxis=True,
	show=False)

ax_hist_x.set_xticks(classborders1, minor=True)
ax_hist_x.set_yticks([20, 40, 60], minor=False)
ax_hist_x.grid(
	visible=True,
	which='minor',
	axis='x',
	linewidth=classborder_linewidth,
	linestyle=classborder_linestyle,
	color=classborder_color,
	zorder=1
	)

ax_nuts = fig.add_subplot(gs[1, 0])
ax_nuts.axis('off')
ax_nuts.text(0.5, 0.5, 'NUTS2\nregions',
    ha='center',
    va='center',
    rotation=45,
    fontsize=10)

plt.subplots_adjust(hspace=0, wspace=0)

# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')

## SCENARIO 3 (BAR CHART NATIONAL AVERAGE)

title = 'Standardized mortality due to DCS vs. NO$_2$ air pollution\nexposure by country in the EU27+CC (excl. Turkey) in 2021'
xlabel1 = 'Age-weighted mortality rate due\nto Diseases of the Circulatory\nSystem [deaths / 100k inhabitants]'
xlabel2 = '% of population exposed to\nhigh NO2 air pollution\n(yearly mean $\geq$ 10 $\mu$g / m3)'
classborders1 = [140, 240, 340, 1340]
classborders2 = [0.01, 60.01, 85, 100]
color_low1 = '#ebebff'
color_high1 = '#6fa4ff'
color_low2 = '#ffeee6'
color_high2 = '#ff5454'
classborder_linewidth = 0.5
classborder_color = '0.7'
classborder_linestyle = '--'
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario3_bar.png'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC3A_MORT_NO2EXP_NUTS2.sql'

with open(query_path) as file:
    sql_query = file.readlines()
    
for l, line in enumerate(sql_query):
    if 'n.levl_code = 2' in line:
        sql_query[l] = line.replace('n.levl_code = 2', 'n.levl_code = 0')
        
sql_query += [' order by mortality_rate_cardio desc']

sql_query = ''.join(sql_query)

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
cmap1 = create_interpolated_cmap(color_low1, color_high1)
cmap2 = create_interpolated_cmap(color_low2, color_high2)

df.dropna(
    subset=['mortality_rate_cardio', 'no2_2021_pat'],
    axis=0,
    inplace=True)

# remove Turkey from dataset
df = df.loc[df['CNTR_CODE'].values != 'TR']

barpos = range(df.shape[0])
barsize_mort = df['mortality_rate_cardio'].values
barsize_no2 = df['no2_2021_pat'].values
n_countries = df.shape[0]
ytick_labels1 = [c + ' (' + str(i + 1) + ')' for i, c in zip(range(n_countries), df['CNTR_CODE'].values)]
sortind = np.argsort(-1 * barsize_no2, kind='stable')
ytick_labels2 = [c + ' (' + str(i + 1) + ')' for i, c in zip(sortind.argsort(), df['CNTR_CODE'].values)]

# add EU average on top with empty space below
avg_mort = barsize_mort.mean()
avg_no2 = barsize_no2.mean()
barpos = np.arange(df.shape[0] + 1)
barpos += 1
barpos[0] = 0
ytick_labels1 = ['EU average'] + ytick_labels1
ytick_labels2 = ['EU average'] + ytick_labels2
barsize_mort = [avg_mort] + list(barsize_mort)
barsize_no2 = [avg_no2] + list(barsize_no2)

f, axes = plt.subplots(1, 2,
    figsize=(7, 8),
    # sharey=True
    )

barchart(barpos, barsize_mort,
    ax=axes[0],
    orientation='horizontal',
    ytick_labels=ytick_labels1,
    classborders=classborders1,
    cmap=cmap1,
    xlabel=xlabel1,
    axis_fontsize=9,
    background_color='0.95',
    invert_xaxis=True,
    invert_yaxis=True,
    show=False)

axes[0].grid(
    visible=True,
    which='major',
    axis='y',
    linewidth=0.5,
    linestyle='--',
    color='0.7'
    )  

barchart(barpos, barsize_no2,
    ax=axes[1],
    orientation='horizontal',
    ytick_labels=ytick_labels2,
    classborders=classborders2,
    cmap=cmap2,
    xlabel=xlabel2,
    axis_fontsize=9,
    tick_right=True,
    background_color='0.95',
    invert_yaxis=True,
    show=False)

axes[1].grid(
    visible=True,
    which='major',
    axis='y',
    linewidth=0.5,
    linestyle='--',
    color='0.7'
    ) 

plt.suptitle(title)
plt.tight_layout()
plt.subplots_adjust(hspace=0, wspace=0)

# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')

## SCENARIO 4 (SCATTERPLOT)

title = 'Standardized mortality due to DCS (NUTS1-2021) vs. Noise\nexposure (NUTS3-2016) in the EU27+CC+UK (excl. Turkey)'
xlabel = 'Age-weighted mortality rate due to Diseases\n of the Circulatory System [deaths / 100k inhabitants]'
ylabel = 'Population-weighted complement of the\nQuietness Suitability Index (unitless)'
classborders1 = [120, 240, 370, 1250]
classborders2 = [0.0001, 0.4001, 0.50001, 0.65, 0.85, 1.0001]
color1 = '0.8'
hatch = [None, '//', '////']
# color_low2 = '#f1eef6'
# color_high2 = '#980043'
classborder_linewidth = 0.5
classborder_color = '0.3'
classborder_linestyle = '--'
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario4_scatter.png'
query_mort_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC4A_MORT_NUTS1.sql'
query_noise_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC4A_NOISE_NUTS3.sql'

with open(query_mort_path) as file:
    sql_mort_query = file.readlines()

sql_mort_query = ''.join(sql_mort_query)

with open(query_noise_path) as file:
    sql_noise_query = file.readlines()

sql_noise_query = ''.join(sql_noise_query)

with engine.connect() as connection:
    df_mort = pd.read_sql_query(sql_mort_query, connection)
    df_noise = pd.read_sql_query(sql_noise_query, connection)
    

noise_nuts3 = df_noise['nuts_id'].values
noise_nuts1 = [n[:3] for n in noise_nuts3]
df_noise['nuts_id'] = noise_nuts1

df = pd.merge(df_mort, df_noise,
    how='left',
    on='nuts_id')

mort = df['mort_dcs'].values
noise = df['qsic_2021_pwp'].values


# cmap2 = create_interpolated_cmap(color_low2, color_high2)
cmap2 = plt.get_cmap('PuRd')

fig = plt.figure(figsize=(8, 8))
gs = fig.add_gridspec(2, 2,
    width_ratios=[0.2, 1],
    height_ratios=[1, 0.2])

ax_scatter =  fig.add_subplot(gs[0, 1])

scatterplot(mort, noise,
    ax=ax_scatter,
    x_classborders=classborders1,
    y_classborders=classborders2,
    classborder_linewidth=classborder_linewidth,
    classborder_color=classborder_color,
    z_color='0.05',
    y_cmap=cmap2,
    y_cmap_cutoff=0.1,
    size=3,
    # title=title,
    background_color='0.95',
    tick_size=0,
    tick_fontsize=0,
    grid=False,
    show=False
    )
ax_scatter.set_title(title, pad=15)


# add custom class backgrounds as overlays of color and hatch
for n in range(len(classborders1) - 1):
    
    for m in range(len(classborders2) - 1):
        
        xpol = [
            classborders1[n],
            classborders1[n],
            classborders1[n + 1],
            classborders1[n + 1],
            classborders1[n]
        ]
        ypol = [
            classborders2[m],
            classborders2[m + 1],
            classborders2[m + 1],
            classborders2[m],
            classborders2[m]
        ]
        
        facecolor = cmap2(float(m) / (len(classborders2) - 1))
        pols = ax_scatter.fill(xpol, ypol, 
            facecolor='none',
            hatch=hatch[n],
            edgecolor=str(0.05 + 0.9 * (1 - np.mean((facecolor)))),
            zorder=0)
        
        pols[0].set_linewidth(0.01)
                

ax_hist_y = fig.add_subplot(gs[0, 0], sharey=ax_scatter)
histogram(noise,
    orientation='horizontal',
    ylabel=ylabel,
    axis_fontsize=11,
	ax=ax_hist_y,
	bins=30,
	classborders=classborders2,
	cmap=cmap2,
    cmap_cutoff=0.1,
	background_color='0.95',
	invert_xaxis=True,
	show=False)

ax_hist_y.set_yticks(classborders2, minor=True)
ax_hist_y.set_xticks([20, 60, 100, 140], minor=False)
ax_hist_y.grid(
	visible=True,
	which='minor',
	axis='y',
	linewidth=classborder_linewidth,
	linestyle=classborder_linestyle,
	color=classborder_color,
	zorder=1
	)

ax_hist_x = fig.add_subplot(gs[1, 1], sharex=ax_scatter)
histogram(mort,
    xlabel=xlabel,
    axis_fontsize=11,
	ax=ax_hist_x,
	bins=30,
	classborders=classborders1,
	color='0.8',
    hatch=hatch,
	background_color='0.95',
	invert_yaxis=True,
	show=False)

ax_hist_x.set_xticks(classborders1, minor=True)
ax_hist_x.set_yticks([70, 120, 170, 220], minor=False)
ax_hist_x.grid(
	visible=True,
	which='minor',
	axis='x',
	linewidth=classborder_linewidth,
	linestyle=classborder_linestyle,
	color=classborder_color,
	zorder=1
	)

ax_nuts = fig.add_subplot(gs[1, 0])
ax_nuts.axis('off')
ax_nuts.text(0.4, 0.4, 'NUTS3$^*$\nregions',
    ha='center',
    va='center',
    fontsize=9,
    rotation=45)
ax_nuts.text(0.5, -0.3, '$^*$: NUTS1 mortality values were\njoined to the spatially matching\nNUTS3 noise exposure values',
    ha='center',
    va='center',
    fontsize=7)

plt.subplots_adjust(hspace=0, wspace=0)

# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')


## SCENARIO 5 (SCATTERPLOT)

title = 'Standardized mortality due to DCS vs. NO$_2$ air pollution\nexposure vs. socio-economic deprivation in the \nEU27+CC (excl. Turkey) in 2021 on NUTS2-level'
xlabel = 'Age-weighted mortality rate due to Diseases\n of the Circulatory System [deaths / 100k inhabitants]'
ylabel = '% of population exposed to high NO$_2$\nair pollution (yearly mean $\geq$ 10 $\mu$g / m$^3$)'
classborders1 = [140, 250, 400, 1340]
classborders2 = [0.01, 60.01, 85, 100.01]
classborders3 = [0, 2, 5, 10, 50]
color_low1 = '#ebebff'
color_high1 = '#6fa4ff'
color_low2 = '#ffeee6'
color_high2 = '#ff5454'
markers3 = ['v', 'o', '^', 'D']
markers3_zcolors = ['#f7fcf5', '#b2e0ab', '#3da75a', '#00441b']
class3_labels = ['Very low: < 2%', 'Low: 2% - 5%', 'Moderate: 5% - 10%', 'High: $\geq$ 10%']
classborder_linewidth = 0.5
classborder_color = '0.3'
classborder_linestyle = '--'
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_graphs\scenario5_scatter.png'
query_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Queries\SC5A_MORT_NO2EXP_DEPR_NUTS2.sql'

with open(query_path) as file:
    sql_query = file.readlines()

sql_query = ''.join(sql_query)

with engine.connect() as connection:
    df = pd.read_sql_query(sql_query, connection)
    
mort = df['mortality_rate_cardio'].values
no2 = df['no2_2021_pat'].values
depr = df['deprivation'].values
cntr = df['CNTR_CODE'].values

cmap1 = create_interpolated_cmap(color_low1, color_high1)
cmap2 = create_interpolated_cmap(color_low2, color_high2)

fig = plt.figure(figsize=(8, 8))
gs = fig.add_gridspec(2, 2,
    width_ratios=[0.2, 1],
    height_ratios=[1, 0.2])

ax_scatter =  fig.add_subplot(gs[0, 1])

for n in range(len(classborders3) - 1):
    
    con1 = depr >= classborders3[n]
    con2 = depr < classborders3[n + 1]
    mort_tmp = mort[con1 & con2]
    no2_tmp = no2[con1 & con2]

    scatterplot(mort_tmp, no2_tmp,
        ax=ax_scatter,
        labels=class3_labels[n],
        x_classborders=classborders1,
        y_classborders=classborders2,
        classborder_linewidth=classborder_linewidth,
        classborder_color=classborder_color,
        z_color=markers3_zcolors[n],
        marker=markers3[n],
        marker_linewidth=0.2,
        marker_edgecolor='0.7',
        x_cmap=cmap1,
        y_cmap=cmap2,
        size=16,
        # title=title,
        background_color='0.95',
        tick_size=0,
        tick_fontsize=0,
        grid=False,
        show=False
        )

ax_scatter.set_title(title, pad=15)
patches = []
for label, col, marker in zip(class3_labels, markers3_zcolors, markers3):
    patches.append(
        plt.Line2D([0], [0],
            color='0.2',
            linewidth=0,
            linestyle='-',
            marker=marker,
            markersize=4,
            markerfacecolor=col,
            markeredgewidth=0.2,
            label=label)
        )

ax_scatter.legend(
    title='% of population living\nin severe socio-\neconomic deprivation',
    title_fontsize=8,
    handles=patches[::-1],
    fontsize=7,
    bbox_to_anchor=(0.9, 0.1),
    bbox_transform=ax_scatter.transAxes,
    loc='lower right')

ax_hist_y = fig.add_subplot(gs[0, 0], sharey=ax_scatter)
histogram(no2,
    orientation='horizontal',
    ylabel=ylabel,
    axis_fontsize=11,
	ax=ax_hist_y,
	bins=30,
	classborders=classborders2,
	cmap=cmap2,
	background_color='0.95',
	invert_xaxis=True,
	show=False)

ax_hist_y.set_yticks(classborders2, minor=True)
ax_hist_y.set_xticks([10, 20], minor=False)
ax_hist_y.grid(
	visible=True,
	which='minor',
	axis='y',
	linewidth=classborder_linewidth,
	linestyle=classborder_linestyle,
	color=classborder_color,
	zorder=1
	)

ax_hist_x = fig.add_subplot(gs[1, 1], sharex=ax_scatter)
histogram(mort,
    xlabel=xlabel,
    axis_fontsize=11,
	ax=ax_hist_x,
	bins=30,
	classborders=classborders1,
	cmap=cmap1,
	background_color='0.95',
	invert_yaxis=True,
	show=False
    )

ax_hist_x.set_xticks(classborders1, minor=True)
ax_hist_x.set_yticks([20, 40, 60], minor=False)
ax_hist_x.grid(
	visible=True,
	which='minor',
	axis='x',
	linewidth=classborder_linewidth,
	linestyle=classborder_linestyle,
	color=classborder_color,
	zorder=1
	)

ax_nuts = fig.add_subplot(gs[1, 0])
ax_nuts.axis('off')
ax_nuts.text(0.5, 0.5, 'NUTS2\nregions',
    ha='center',
    va='center',
    rotation=45,
    fontsize=10
    )

plt.subplots_adjust(hspace=0, wspace=0)

# plt.show()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')