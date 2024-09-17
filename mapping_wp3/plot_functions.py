## MODULES

import matplotlib.pyplot as plt
import numpy as np
import matplotlib.colors as mcolors
from matplotlib import rc
from mpl_toolkits.axisartist.grid_helper_curvelinear import GridHelperCurveLinear
from mpl_toolkits.axisartist import Subplot

rc('font',**{'family':'serif','serif':['Arial']})
rc('text', usetex=False)

## FUNCTIONS

def create_interpolated_cmap(color1, color2):
    
    cmap = mcolors.LinearSegmentedColormap.from_list('custom_cmap', [color1, color2])
    
    return cmap


def generate_bivariate_legend(cmap1, cmap2,
    varname1=None,
    varname2=None,
    n_classes=3,
    ax=None,
    classnames1=None,
    classnames2=None,
    classborders1=None,
    classborders2=None,
    classlabels1=None,
    classlabels2=None,
    show=False,
    varfontsize=20,
    classfontsize=15,
    classlabelfontsize=10,
    borderfontsize=15):
    
    if ax is None:
        ax = plt.gca()
    
    # generate vertical and horizontal color grids
    colors1 = cmap1(np.linspace(0, 1, num=n_classes)).reshape(1, -1, 4)
    colors2 = cmap2(np.linspace(0, 1, num=n_classes)).reshape(-1, 1, 4)
    grid1 = np.repeat(colors1, n_classes, axis=0)
    grid2 = np.repeat(colors2, n_classes, axis=1)
    grid = grid1 * grid2
    
    ax.imshow(grid, origin='lower')
    
    if varname1 is not None:
        ax.set_xlabel(varname1,
            fontsize=varfontsize,
            labelpad=20,
            linespacing=2)
    if varname2 is not None:
        ax.set_ylabel(varname2,
            fontsize=varfontsize,
            labelpad=20,
            linespacing=2)
    
    if classborders1 is None:
        ax.set_xticks([])
    else:
        ax.set_xticks(np.linspace(-0.5, n_classes - 0.5, num=n_classes + 1))
        ax.set_xticklabels(classborders1, size=borderfontsize)
        
    if classborders2 is None:
        ax.set_yticks([])
    else:
        ax.set_yticks(np.linspace(-0.5, n_classes - 0.5, num=n_classes + 1))
        ax.set_yticklabels(classborders2, size=borderfontsize)
    
    if classnames1 is not None:
        minor_tick_positions = np.arange(n_classes)
        ax.xaxis.set_minor_locator(plt.FixedLocator(minor_tick_positions))
        ax.set_xticklabels(classnames1,
            minor=True,
            size=classfontsize)
        ax.tick_params(
            axis='x',
            which='minor',
            pad=20,
            size=0)
        
    if classnames2 is not None:
        minor_tick_positions = np.arange(n_classes)
        ax.yaxis.set_minor_locator(plt.FixedLocator(minor_tick_positions))
        ax.set_yticklabels(classnames2,
            minor=True,
            size=classfontsize)
        ax.tick_params(
            axis='y',
            which='minor',
            pad=20,
            size=0)
        
    if classlabels1 is not None and classlabels2 is not None:
        
        if len(classlabels1) != n_classes or len(classlabels2) != n_classes:
            raise ValueError('length of classlabels must equal n_classes')
        
        for x in range(n_classes):
            for y in range(n_classes):
                ax.text(x, y, '{}{}'.format(classlabels1[x], classlabels2[y]),
                    ha='center',
                    va='center',
                    fontsize=classlabelfontsize)
    
    if show:
        plt.tight_layout()
        plt.show()


def histogram(x,
    ax=None,
    density=False,
    scale_n=1,
    bins=10,
    orientation='vertical',
    classborders=None,
    classborders_plotlines=False,
    classborder_color='0.2',
    classborder_linestyle='-',
    classborder_linewidth=1,
    color=None,
    hatch=None,
    cmap=None,
    cmap_cutoff=0.1,
    title=None,
    xlabel=None,
    ylabel=None,
    title_fontsize=12,
    axis_fontsize=10,
    tick_fontsize=8,
    tick_right=False,
    tick_top=False,
    classborder_fontsize=8,
    invert_xaxis=False,
    invert_yaxis=False,
    grid=True,
    line=None,
    line_label=None,
    line_labelsize=8,
    line_linestyle='--',
    line_linewidth=1,
    line_color='0.2',
    line_ha=None,
    line_pos=None,
    background_color=None,
    show=True
    ):
    
    if ax is None:
        ax = plt.gca()
        
    if classborders is None:
        n_classes = 1
        classborders = [x.min(), x.max()]
    else:
        n_classes = len(classborders) - 1
        
    if cmap is not None:
        color = [cmap(c) for c in np.linspace(0 + cmap_cutoff, 1 - cmap_cutoff, num=n_classes)]
    elif color is not None:
        if type(color) is not list:
            color = [color] * n_classes
        elif len(color) != n_classes:
            raise ValueError('the length of the color list must match the number of classes')
    else:
        color = [None] * n_classes
        
    if hatch is not None:
        if type(hatch) is not list:
            hatch = [hatch] * n_classes
        elif len(hatch) != n_classes:
            raise ValueError('the length of the hatch list must match the number of classes')
    else:
        hatch = [None] * n_classes
        
    if invert_xaxis:
        ax.invert_xaxis()
        
    if invert_yaxis:
        ax.invert_yaxis()
        
    n, bins, patches = ax.hist(x,
        density=density,
        bins=bins,
        range=(classborders[0], classborders[-1]),
        orientation=orientation,
        lw=1.0,
        ec='0.2',
        zorder=3  # min zorder of 3 needed to plot over grid lines (unclear why)
        )
    
    bincenters = np.diff(bins) / 2 + bins[:-1]
    classlabels = np.digitize(bincenters, classborders) - 1
    
    for classlabel, patch in zip(classlabels, patches):
        patch.set_facecolor(color[classlabel])
        patch.set_hatch(hatch[classlabel])
        
    ax.tick_params(
        axis='both',
        which='major',
        labelsize=tick_fontsize)
    
    ax.tick_params(
        axis='both',
        which='minor',
        size=0)
    
    if classborders is not None and classborders_plotlines:
        if (orientation is None) or (orientation == 'vertical'):
            ax.vlines(classborders, 0, n.max(),
                colors=classborder_color,
                linestyles=classborder_linestyle,
                linewidth=classborder_linewidth)
        elif orientation == 'horizontal' and classborders_plotlines:
             ax.hlines(classborders, 0, n.max(),
                colors=classborder_color,
                linestyles=classborder_linestyle,
                linewidth=classborder_linewidth)  
             
    def formatter(inp, pos):
        del pos
        return str(np.around(inp * scale_n, 1))

    if scale_n != 1:
        if (orientation is None) or (orientation == 'vertical'):
            ax.yaxis.set_major_formatter(formatter)
        else:
            ax.xaxis.set_major_formatter(formatter) 
            
    if grid:
        if (orientation is None) or (orientation == 'vertical'):
            axis = 'y'
        else:
            axis = 'x'      
        ax.grid(
            visible=True,
            which='major',
            axis=axis,
            linewidth=0.5,
            linestyle=':',
            color='0.5',
            zorder=0
            )
        
    if line:
        if (orientation is None) or (orientation == 'vertical'):
            ylim = [min(n), max(n)]
            ax.set_xticks([line], minor=True)
            ax.grid(
                visible=True,
                which='minor',
                axis='x',
                linewidth=line_linewidth,
                linestyle=line_linestyle,
                color=line_color,
                zorder=10
                )           
            if line_label is not None:
                if line_pos:
                    ytext = line_pos
                else:
                    ytext = ylim[1]
                if line_ha is None:
                    line_ha = 'left'
                ax.text(line, ytext, line_label,
                    fontsize=line_labelsize,
                    ha=line_ha
                    )
        elif orientation == 'horizontal':
            xlim = [min(n), max(n)]
            ax.set_yticks([line], minor=True)
            ax.grid(
                visible=True,
                which='minor',
                axis='y',
                linewidth=line_linewidth,
                linestyle=line_linestyle,
                color=line_color,
                zorder=10
                ) 
            if line_label is not None:
                if line_pos:
                    xtext = line_pos
                else:
                    xtext = xlim[1]
                if line_ha is None:
                    line_ha = 'left'
                ax.text(xtext, line, line_label,
                    fontsize=line_labelsize,
                    ha=line_ha
                    )
    
    if tick_right:
        ax.yaxis.tick_right()
        ax.yaxis.set_label_position("right")
    if tick_top:
        ax.xaxis.tick_top()
        ax.xaxis.set_label_position("top")
    
    if title is not None:
        ax.set_title(title, fontsize=title_fontsize)
        
    if xlabel is not None:
        ax.set_xlabel(xlabel, fontsize=axis_fontsize)
        
    if ylabel is not None:
        ax.set_ylabel(ylabel, fontsize=axis_fontsize)
        
    if background_color is not None:
        ax.set_facecolor(background_color)
        
    if show:
        plt.show()
        
        
def barchart(barpos, barsize,
    ax=None,
    orientation='vertical',
    width=0.8,
    height=0.8,
    bottom=0.0,
    left=0.0,
    label=None,
    xerr=None,
    yerr=None,
    ecolor='0.5',
    capsize=0.0,
    error_kw={},
    classborders=None,
    color=None,
    cmap=None,
    cmap_cutoff=0.1,
    title=None,
    xlabel=None,
    ylabel=None,
    xtick_labels=None,
    ytick_labels=None,
    title_fontsize=12,
    axis_fontsize=10,
    tick_fontsize=8,
    tick_right=False,
    tick_top=False,
    invert_xaxis=False,
    invert_yaxis=False,
    grid=True,
    line=None,
    line_label=None,
    line_labelsize=8,
    line_linestyle='--',
    line_linewidth=1,
    line_color='0.2',
    line_pos=None,
    line_ha=None,
    background_color=None,
    show=True
    ):

    if ax is None:
        ax = plt.gca()
        
    if classborders is None:
        n_classes = 1
        classborders = [min(barsize), max(barsize) + 1]
    else:
        n_classes = len(classborders) - 1
        
    if cmap is not None:
        color = [cmap(c) for c in np.linspace(0 + cmap_cutoff, 1 - cmap_cutoff, num=n_classes)]
    elif color is not None:
        if type(color) is not list:
            color = [color] * n_classes
        elif len(color) != n_classes:
            raise ValueError('the length of the color list must match the number of classes')
    else:
        color = [None] * n_classes
        
    if invert_xaxis:
        ax.invert_xaxis()
        
    if invert_yaxis:
        ax.invert_yaxis()
        
    classvalues = np.digitize(barsize, classborders) - 1
    
    for classvalue in np.unique(classvalues):
        
        ind = np.where(classvalues == classvalue)[0]
        barpos_tmp = np.array(barpos)[ind]
        barsize_tmp = np.array(barsize)[ind]
        
        if yerr is not None:
            yerr_tmp = np.array(yerr)[ind]
        else:
            yerr_tmp = None
            
        if xerr is not None:
            xerr_tmp = np.array(xerr)[ind]
        else:
            xerr_tmp = None
        
        if  (orientation is None) or (orientation == 'vertical'):
            barcontainer = ax.bar(barpos_tmp, barsize_tmp,
                width=width,
                bottom=bottom,
                color=color[classvalue],
                linewidth=1.0,
                edgecolor='0.2',
                xerr=xerr_tmp,
                yerr=yerr_tmp,
                ecolor=ecolor,
                capsize=capsize,
                error_kw=error_kw,
                label=label,
                zorder=3  # min zorder of 3 needed to plot over grid lines (unclear why)
                )
        else:
            barcontainer = ax.barh(barpos_tmp, barsize_tmp,
                height=height,
                left=left,
                color=color[classvalue],
                linewidth=1.0,
                edgecolor='0.2',
                xerr=xerr_tmp,
                yerr=yerr_tmp,
                ecolor=ecolor,
                capsize=capsize,
                error_kw=error_kw,
                label=label,
                zorder=6  # min zorder of 3 needed to plot over grid lines (unclear why)
                )        
          
    ax.tick_params(
        axis='both',
        which='major',
        labelsize=tick_fontsize)
    
    ax.tick_params(
        axis='both',
        which='minor',
        size=0)
    
    if xtick_labels is not None:
        ax.set_xticks(barpos)
        ax.set_xticklabels(xtick_labels)

    if ytick_labels is not None:
        ax.set_yticks(barpos)
        ax.set_yticklabels(ytick_labels)           
             
    if grid:
        if (orientation is None) or (orientation == 'vertical'):
            axis = 'y'
        else:
            axis = 'x'      
        ax.grid(
            visible=True,
            which='major',
            axis=axis,
            linewidth=0.5,
            linestyle=':',
            color='0.5',
            zorder=0
            )
    
    if line:
        
        if (orientation is None) or (orientation == 'vertical'):
            xlim = [min(barpos), max(barpos)]
            ax.set_yticks([line], minor=True)
            ax.grid(
                visible=True,
                which='minor',
                axis='y',
                linewidth=line_linewidth,
                linestyle=line_linestyle,
                color=line_color,
                zorder=1
                )           
            if line_label is not None:
                if line_pos:
                    xtext = line_pos
                else:
                    xtext = xlim[1]
                if line_ha is None:
                    line_ha = 'right'
                ax.text(xtext, line, line_label,
                    fontsize=line_labelsize,
                    ha=line_ha,
                    va='bottom')
                
        elif orientation == 'horizontal':
            ylim = [min(barpos), max(barpos)]
            ax.set_xticks([line], minor=True)
            ax.grid(
                visible=True,
                which='minor',
                axis='x',
                linewidth=line_linewidth,
                linestyle=line_linestyle,
                color=line_color,
                zorder=1
                ) 
            if line_label is not None:
                if line_pos:
                    ytext = line_pos
                else:
                    ytext = ylim[1]
                ax.text(line, ytext, line_label,
                    fontsize=line_labelsize,
                    ha='left',
                    va='bottom')
    
    if tick_right:
        ax.yaxis.tick_right()
        ax.yaxis.set_label_position("right")
    if tick_top:
        ax.xaxis.tick_top()
        ax.xaxis.set_label_position("top")
    
    if title is not None:
        ax.set_title(title,
            fontsize=title_fontsize,
            linespacing=1.5)
        
    if xlabel is not None:
        ax.set_xlabel(xlabel,
            fontsize=axis_fontsize,
            linespacing=1.5)
        
    if ylabel is not None:
        ax.set_ylabel(ylabel,
            fontsize=axis_fontsize,
            linespacing=1.5)
        
    if background_color is not None:
        ax.set_facecolor(background_color)
        
    if show:
        plt.show()
        
        
def scatterplot(x, y,
    z=None,
    labels=None,
    ax=None,
    x_classborders=None,
    y_classborders=None,
    z_classborders=None,
    classborder_linewidth=1,
    classborder_linestyle='--',
    classborder_color='0.5',
    marker='o',
    marker_linewidth=0.05,
    marker_edgecolor='0.1',
    size=1,
    x_color=None,
    x_cmap=None,
    x_cmap_cutoff=0,
    y_color=None,
    y_cmap=None,
    y_cmap_cutoff=0,
    z_color=None,
    z_cmap=None,
    z_cmap_cutoff=0,
    title=None,
    xlabel=None,
    ylabel=None,
    title_fontsize=12,
    axis_fontsize=10,
    label_fontsize=8,
    tick_fontsize=8,
    tick_size=3,
    tick_right=False,
    tick_top=False,
    invert_xaxis=False,
    invert_yaxis=False,
    grid=True,
    background_color=None,
    show=True):
    
    if ax is None:
        ax = plt.gca()
        
    if x_classborders is None:
        x_n_classes = 1
        x_classborders = [min(x), max(x)]
    else:
        x_n_classes = len(x_classborders) - 1
        
    if x_cmap is not None:
        x_color = [x_cmap(c) for c in np.linspace(0 + x_cmap_cutoff, 1 - x_cmap_cutoff, num=x_n_classes)]
    elif x_color is not None:
        if type(x_color) is not list:
            x_color = [x_color] * n_classes
        elif len(x_color) != x_n_classes:
            raise ValueError('the length of the color list must match the number of classes')
    else:
        x_color = [None] * x_n_classes
        
    if y_classborders is None:
        y_n_classes = 1
        y_classborders = [min(y), max(y)]
    else:
        y_n_classes = len(y_classborders) - 1
        
    if y_cmap is not None:
        y_color = [y_cmap(c) for c in np.linspace(0 + y_cmap_cutoff, 1 - y_cmap_cutoff, num=y_n_classes)]
    elif y_color is not None:
        if type(y_color) is not list:
            y_color = [y_color] * y_n_classes
        elif len(y_color) != y_n_classes:
            raise ValueError('the length of the color list must match the number of classes')
    else:
        y_color = [None] * y_n_classes
        
    if (z is not None) and (z_classborders is None):
        z_n_classes = 1
        z_classborders = [min(z), max(z)]
    elif (z is not None) and (z_classborder is not None):
        z_n_classes = len(z_classborders) - 1
    elif z is None:
        z_n_classes = 1
        z_classborders = None
        
    if (z is not None) and (z_cmap is not None):
        z_color = np.array([z_cmap(c) for c in np.linspace(0 + z_cmap_cutoff, 1 - z_cmap_cutoff, num=z_n_classes)])
        z_class = np.digitize(z, z_classborders) - 1
        z_class = z_class.astype(int)
        z_color = z_color(z_class)
        
    if invert_xaxis:
        ax.invert_xaxis()
        
    if invert_yaxis:
        ax.invert_yaxis()
        
    ax.scatter(x, y,
        marker=marker,
        s=size,
        facecolors=z_color,
        edgecolors=marker_edgecolor,
        linewidths=marker_linewidth,
        zorder=6
        )
    
    ax.tick_params(
        axis='both',
        which='major',
        labelsize=tick_fontsize,
        size=tick_size)
    
    ax.tick_params(
        axis='both',
        which='minor',
        size=0)
    
    if len(x_classborders) > 2:

        ax.set_xticks(x_classborders, minor=True)
        ax.grid(
            visible=True,
            which='minor',
            axis='x',
            linewidth=classborder_linewidth,
            linestyle=classborder_linestyle,
            color=classborder_color,
            zorder=1
            )
            
    if len(y_classborders) > 2:
        
        ax.set_yticks(y_classborders, minor=True)
        ax.grid(
            visible=True,
            which='minor',
            axis='y',
            linewidth=classborder_linewidth,
            linestyle=classborder_linestyle,
            color=classborder_color,
            zorder=1
            )
        
    con1 = x_color != [None] * x_n_classes
    con2 = x_color != [None] * y_n_classes
    con3 = x_classborders is not None
    con4 = y_classborders is not None
    
    if (con1 or con2) and con3 and con4:
        
        for n, xc in enumerate(x_color):
            
            for m, yc in enumerate(y_color):
                
                if yc is None:
                    cmix = xc
                if xc is None:
                    cmix = yc
                else:
                    cmix = np.array(xc) * np.array(yc)
                xpol = [
                    x_classborders[n],
                    x_classborders[n],
                    x_classborders[n + 1],
                    x_classborders[n + 1],
                    x_classborders[n]
                ]
                ypol = [
                    y_classborders[m],
                    y_classborders[m + 1],
                    y_classborders[m + 1],
                    y_classborders[m],
                    y_classborders[m]
                ]
                ax.fill(xpol, ypol, 
                    facecolor=cmix,
                    zorder=0)
                
    
    if grid:     
        ax.grid(
            visible=True,
            which='major',
            axis='both',
            linewidth=0.5,
            linestyle=':',
            color='0.5',
            zorder=3
            )
        
    if tick_right:
        ax.yaxis.tick_right()
        ax.yaxis.set_label_position("right")
    if tick_top:
        ax.xaxis.tick_top()
        ax.xaxis.set_label_position("top")
    
    if title is not None:
        ax.set_title(title,
            fontsize=title_fontsize,
            linespacing=1.5)
        
    if xlabel is not None:
        ax.set_xlabel(xlabel,
            fontsize=axis_fontsize)
        
    if ylabel is not None:
        ax.set_ylabel(ylabel,
            fontsize=axis_fontsize)
        
    if background_color is not None:
        ax.set_facecolor(background_color)
        
    if show:
        plt.show()