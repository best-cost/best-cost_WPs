# Title and description

#' Get population attributable fraction
#'
#' Calculates the population attributable fraction of an environmental stressor
#' @param crf_conc
#' Risk estimate of the concentration response function for a specific concentration. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param prop_pop_exp
#' Proportion (from 0 to 1) of population exposed to the environmental stressor. Per default = 1 (i.e. 100\% of population is exposed).
#' @return
#' This function returns a \code{value} corresponding to the population attributable fraction
#' @examples
#' get_paf(crf = 1.062, prop_pop_exposed = 1)
#' @author Alberto Castro
#' @note Experimental function
#' @export
get_paf <-
  function(crf_conc, prop_pop_exp){
    # Source: https://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-1-1
    paf <- (sum(prop_pop_exp * (crf_conc-1))) / (1+(sum(prop_pop_exp *(crf_conc-1))))

    return(paf)
  }


