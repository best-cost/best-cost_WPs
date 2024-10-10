#' Get Monte Carlo confidence intervals

#' @description
#' Determine summary uncertainty based on at least one variable with central, lower and upper estimate
#' @inheritParams attribute
#' @return
#' This function returns a Monte Carlo summary uncertainty for the attributable health impacts.
#' @import dplyr
#' @import purrr
#' @export
#' @author Axel Luyten
#' @note Experimental function
#' @keywords internal
#' @examples
#' TBD

get_ci <- function(rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
                   exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
                   cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
                   bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
                   dw_central = NULL, dw_lower = NULL, dw_upper = NULL
                   # dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper # To be added later (cf. #313)
                   ){

  # Set seed for reproducibility
  set.seed(123)

  # Define standard deviations (sd)
  sd_rr <- (rr_upper - rr_lower) / (2 * 1.96)
  sd_exp <- (exp_upper - exp_lower) / (2 * 1.96)
  sd_cutoff <- (cutoff_upper - cutoff_lower) / (2 * 1.96)
  sd_bhd <- (bhd_upper - bhd_lower) / (2 * 1.96)
  sd_dw <- (dw_upper - dw_lower) / (2 * 1.96)

  # Simulate values ############################################################
  n_simulations <- 1000
  samples_rr <- rnorm(n_simulations, mean = rr_central, sd = sd_rr)
  samples_exp <- rnorm(n_simulations, mean = exp_central, sd = sd_exp)
  samples_cutoff <- rnorm(n_simulations, mean = cutoff_central, sd = sd_cutoff)
  samples_bhd <- rnorm(n_simulations, mean = bhd_central, sd = sd_bhd)
  samples_dw <- rnorm(n_simulations, mean = dw_central, sd = sd_dw)

  # Calculations ###############################################################
  ## get_risk


  ## get_pop_fraction

  ## Multiply PAFs with bhd & dw

  ## Calculate 95% CI ##########################################################

  # Output results #############################################################




}
