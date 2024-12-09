#' Get population attributable fraction

#' @description Calculates the population attributable fraction of a health outcome due to exposure to an environmental stressor
#' @inheritParams attribute
#' @param rr_conc \code{Numerical value} Risk estimate of the concentration response function for a specific concentration. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @return
#' This function returns a \code{value} corresponding to the population attributable fraction
#' @examples
#' get_paf(rr = 1.062, prop_pop_exposed = 1)
#' @author Alberto Castro
#' @note Experimental function
#' @export

get_paf <-
  function(rr_conc, prop_pop_exp){
    # Sources:
    # WHO 2003 a: Prüss-Üstün_2003_Assessing the environmental burden of disease at national and local levels)
    # WHO 2003 b: Murray_2003_Comparative quantification of health risks Conceptual framework and methodological issues
    # GBD 2019
    # paf <- (sum(prop_pop_exp * (rr_conc-1))) / (1+(sum(prop_pop_exp *(rr_conc-1))))

    # Instead of calculating PAF as above, just use the PIF
    # but with no effect in the second scenario
    # (same result using paf and pif for comparison with no effect)

    paf <-
      healthiar::get_pop_fraction(rr_conc_1 = rr_conc,
                                 rr_conc_2 = rep(1, length(rr_conc)),
                                 prop_pop_exp_1 = prop_pop_exp,
                                 prop_pop_exp_2 = rep(1, length(prop_pop_exp)))



    return(paf)
  }


