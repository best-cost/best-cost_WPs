# Title and description

#' Get population attributable fraction
#'
#' Calculates the population attributable fraction of an environmental stressor
#' @param crfConc
#' Concentration response function after re-scaling the concentration-response function from the increment value in the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual population exposure).
#' @param pep
#' Proportion (from 0 to 1) of population exposed to the environmental stressor. Per default = 1 (i.e. 100\% of population is exposed).
#' @return
#' This function returns a \code{value} corresponding to the population attributable fraction
#' @examples
#' get_paf(pep = 1, metric = "Premature deaths")
#' @author Alberto Castro
#' @note Experimental function
#' @export
get_paf <-
  function(crfConc, pep=1){
    paf <- (pep*(crfConc-1))/(1+(pep*(crfConc-1)))

    return(paf)
  }

# Proposals for renaming parameters ####
# "crfConc" might be renamed to ...
# "pep" might be renamed to "prop_exp_pop" (other ideas welcome!)
