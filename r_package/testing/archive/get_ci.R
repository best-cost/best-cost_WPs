# ARCHIVED ON 2024-08-22 #######################################################
# FIRST DRAFT OF SUMMARY UNCERTAINTY IMPLEMENTATION BASED ON SCIENSANO CODE

# Determine confidence interval for years lived with disability

#' Get confidence interval
#'
#' Calculate the summary confidence interval of years lived with disability with input sampling
#' @param paf \code{Numerical value} showing the population attributable fraction (paf)
#' @param bhd_central,bhd_lower,bhd_upper \code{Numeric value} showing the central estimate, the lower bound and the upper bound of the 95% confidence interval of the baseline health data (bhd) (e.g. prevalence of the health outcome in the population).
#' @param dw_central,dw_lower,dw_upper Three \code{Numeric value} showing the disability weights (dw) (central estimate, lower and upper 95% confidence intervals) associated with the morbidity health outcome
#' @param duration_central,duration_lower,duration_upper Three \code{Numeric value} showing the duration (central estimate, lower and upper 95% confidence intervals) of the morbidity health outcome
#' @return
#' This function returns a \code{vector} containing the 95% confidence intervals of the inputted YLD impact
#' @examples
#' TBD
#' @author Axel Luyten
#' @note Experimental function
#' @export
#' @import prevalence
#' @import dplyr

get_ci <- function(paf = 0.1,
                   bhd_central = 100000, bhd_lower = 50000, bhd_upper = 150000,
                   dw_central = 0.5, dw_lower = 0.25, dw_upper = 0.75,
                   duration_central = 1, duration_lower = 0.5, duration_upper = 1.5){

  # Functions from base R
  ?set.seed
  ?rbeta # {stats}
  ?with
  # Function from {prevalence}
  ?betaExpert

  set.seed(2503)

  # Variables
  paf <- 0.1
  bhd_central <- 100000
  bhd_lower <- 50000
  bhd_upper <- 150000
  dw_central <- 0.5
  dw_lower <- 0.25
  dw_upper <- 0.75
  duration_central <- 1
  duration_lower <- 0.5
  duration_upper <- 1.5

  # Prevalence sampling ####

  # prev <- runif_with_seed(1000, bhd_lower, bhd_upper, 1)
  # prev <- as.data.frame(prev)
  # prev_mean <- mean(prev)
  # prev_sd <- sd(prev)
  # n <- 1000
  #
  # rbeta2 <- function(n, prev_mean, prev_sd){
  #   var <- prev_sd^2
  #   # alpha parameter for beta distribution (i.e. number of successes)
  #   alpha <- prev_mean * ( ( prev_mean * ( 1 - prev_mean ) / var ) - 1 )
  #   # beta parameter for beta distribution (i.e. number of failures)
  #   beta <- (1 - prev_mean) * ( ( prev_mean * ( 1 - prev_mean) / var ) - 1)
  #   rbeta(n, alpha, beta)
  # }
  #
  # prev_sim <- with(prev, mapply(rbeta2, n = n, prev_mean = prev_mean, prev_sd = prev_sd))

  # Disability weight sampling ####
  ## Based on central estimatem lower and upper 95% CI's
  dw_data <- data.frame(dw = dw_central,
                           dw_lower = dw_lower,
                           dw_upper = dw_upper)
  dw_par <- with(dw_data, mapply(prevalence::betaExpert,
                                 dw_central,
                                 dw_lower,
                                 dw_upper,
                                 method = "mean"))

  dw_sim <- mapply(rbeta, dw_par["alpha",], dw_par["beta",], n=n)

  ## Average DW




}
