# Title and description

#' Re-scale the concentration-response function
#'
#' Re-scale the concentration-response function from the increment value in the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual population exposure)
#' @param crf
#' Data frame containing the concentration-response function as in the epidemiological study, i.e. per the usual concentration increase. The data frame must contain the mean, lower and upper bound of the concentration-response function.
#' @param exp
#' Population exposure to the stressor (e.g. annual population-weighted mean).
#' @param cutoff \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param crf_per
#' Increment of the concentration-response function as in the literature (e.g. for PM2.5 10 or 5 ug/m3).
#' @param method
#' Method to re-scale the concentration-response function based on the assumed form of the function. Likely values: "log-linear" or "linear".
#' @return
#' This function returns three \code{values} corresponding to mean, lower bound and upper bound of the concentration-response function.
#' @examples
#' rescale_crf(crf = pep = 1, metric = "Premature deaths")
#' @author Alberto Castro
#' @note Experimental function
#' @export
rescale_crf <-
  function(crf, exp, cutoff, crf_per, method){
    if(method == "loglinear"){
      # CRF for the specific concentration
      # (exposure minus counterfactural)
      # as in EKL study (Castro et al. 2020)
      # and CITIES study (Khomenko 2021, suppl. Materials)
      crf_conc <-
        exp(log(crf) * (exp-cutoff)/crf_per)
    }

    if(method == "linear"){
      # Different way to obtain the CRF corresponding to a specific exposure
      # STE-2000 used this approach
      crf_conc <-
        1+( (crf-1) * (exp-cutoff)/crf_per )
    }

    return(crf_conc)

  }

# Comment line 5 ####
# from "Re-scale the concentration-response function" it is not clear to me what the output is: RR or OR?
# Comment line 13 ####
# the term "Increment of the concentration-response function" is unclear to me. Is the SIZE of the increment meant (e.g. 5 or 10 ug/m3) or the RR CHANGE per air pollution increment?
