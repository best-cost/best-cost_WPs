## MODULES
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.colors as mcolors
from matplotlib import rc
from mpl_toolkits.axisartist.grid_helper_curvelinear import GridHelperCurveLinear
from mpl_toolkits.axisartist import Subplot
from plot_functions import generate_bivariate_legend, create_interpolated_cmap

rc('font',**{'family':'serif','serif':['Arial']})
rc('text', usetex=False)

## BIVARIATE LEGEND SCENARIO 3

color1_low = '#ebebff'
color1_high = '#6fa4ff'
color2_low = '#ffeee6'
color2_high = '#ff5454'
varname1 = 'Age-weighted$^{**}$ yearly mortality\ndue to DCS [deaths / 100k inhabitants]'
varname2 = 'Population exposed to high$^{***}$\nNO$_2$ air pollution [% of pop.]'
classnames1 = ['Low', 'Med.', 'High']
classnames2 = ['Low', 'Med.', 'High']
classlabels1 = ['L', 'M', 'H']
classlabels2 = ['L', 'M', 'H']
classborders1 = [140, 240, 340, 1340]
classborders2 = [0, 60, 85, 100]
n_classes = 3
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_legends\SC3A_MORT_DEPR_2021_NUTS2_bivar_legend.png'

ax = plt.gca()

# make the interpolated colormaps
cmap1 = create_interpolated_cmap(color1_low, color1_high)
cmap2 = create_interpolated_cmap(color2_low, color2_high)
        
generate_bivariate_legend(
    cmap1,
    cmap2,
    varname1=varname1,
    varname2=varname2,
    varfontsize=15,
    classfontsize=13,
    classlabelfontsize=12,
    borderfontsize=13,
    classborders1=classborders1,
    classborders2=classborders2,
    classnames1=classnames1,
    classnames2=classnames2,
    classlabels1=classlabels1,
    classlabels2=classlabels2,
    ax=ax,
    show=False
    )

plt.tight_layout()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')

## TRIVARIATE LEGEND SCENARIO 5

color1_low = '#ebebff'
color1_high = '#6fa4ff'
color2_low = '#ffeee6'
color2_high = '#ff5454'
varname1 = 'Age-weighted$^{d}$ yearly mortality\ndue to DCS [deaths / 100k inhab.]'
varname2 = 'Population exposed to high$^{e}$\nNO$_2$ air pollution [% of pop.]'
classnames1 = ['Low', 'Med.', 'High']
classnames2 = ['Low', 'Med.', 'High']
classlabels1 = ['L', 'M', 'H']
classlabels2 = ['L', 'M', 'H']
classborders1 = [140, 250, 400, 1340]
classborders2 = [0, 60, 85, 100]
n_classes = 3
out_path = r'E:\_Projecten\2210529-01_HEU_BEST_COST\WP3\Mapping\Exported_legends\SC5A_MORT_NO2EXP_DEPR_2021_NUTS2_bivar_legend.png'

ax = plt.gca()

# make the interpolated colormaps
cmap1 = create_interpolated_cmap(color1_low, color1_high)
cmap2 = create_interpolated_cmap(color2_low, color2_high)
        
generate_bivariate_legend(
    cmap1,
    cmap2,
    varname1=varname1,
    varname2=varname2,
    varfontsize=16,
    classfontsize=13,
    borderfontsize=13,
    classlabelfontsize=12,
    classborders1=classborders1,
    classborders2=classborders2,
    classnames1=classnames1,
    classnames2=classnames2,
    classlabels1=classlabels1,
    classlabels2=classlabels2,
    ax=ax,
    show=False
    )

plt.tight_layout()
plt.savefig(out_path,
    dpi=300,
    bbox_inches='tight')