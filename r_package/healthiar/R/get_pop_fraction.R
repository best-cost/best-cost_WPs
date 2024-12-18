#' Get population attributable or impact fraction

#' @description Calculates the population impact fraction of a health outcome due to exposure to an environmental stressor
#' @param rr_conc_1 \code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 1. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param rr_conc_2 \code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 2. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param prop_pop_exp_1 \code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).
#' @param prop_pop_exp_2 \code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).
#' @return
#' This function returns a \code{value} corresponding to the population attributable fraction
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

get_pop_fraction <-
  function(rr_conc_1, rr_conc_2, prop_pop_exp_1, prop_pop_exp_2){

    ## Check that sum of prop_pop_exp = 1 (100% of population)!

    ## Source:
    ## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC156894/

    ## pop_fraction = ( sum of the pairwise products of vector containing the proportion of
    ## the population exposed per exposure band and the corresponding vector
    ## containing the risk at each exposure band ) minus
    ## ( sum of the pairwise products of same proportion vector and risk at reference level (i.e. 1) ) divided by
    ## sum of the first products
    pop_fraction <- (sum(prop_pop_exp_1 * rr_conc_1) - sum(prop_pop_exp_2 * rr_conc_2)) / (sum(prop_pop_exp_1 * rr_conc_1))

    return(pop_fraction)
  }


