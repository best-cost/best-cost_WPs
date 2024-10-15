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


get_ci_old <- function(paf = 0.1,
                   bhd_central = 100000, bhd_lower = 50000, bhd_upper = 150000,
                   dw_central = 0.5, dw_lower = 0.25, dw_upper = 0.75,
                   duration_central = 1, duration_lower = 0.5, duration_upper = 1.5){

  # Variables (for developing code)
  # paf <- 0.1
  # bhd_central <- 100000 * paf
  # bhd_lower <- 50000 * paf
  # bhd_upper <- 150000 * paf
  # dw_central <- 0.5
  # dw_lower <- 0.25
  # dw_upper <- 0.75
  # duration_central <- 1
  # duration_lower <- 0.5
  # duration_upper <- 1.5

  # Set seed for reproducibility
  set.seed(123)

  # Calculate standard deviations
  sd_bhd <- (bhd_upper - bhd_lower) / (2 * 1.96)
  sd_dw <- (dw_upper - dw_lower) / (2 * 1.96)
  sd_dur <- (duration_upper - duration_lower) / (2 * 1.96)

  # Monte Carlo simulation
  n_simulations <- 100000
  bhd_samples <- rnorm(n_simulations, mean = bhd_central, sd = sd_bhd)
  dw_samples <- rnorm(n_simulations, mean = dw_central, sd = sd_dw)
  duration_samples <- rnorm(n_simulations, mean = duration_central, sd = sd_dur)

  # Calculate YLD for each simulation
  yld_samples <- bhd_samples * dw_samples * duration_samples

  # Calculate 95% confidence interval
  yld_ci <- quantile(yld_samples, probs = c(0.025, 0.975))

  # Output results
  cat("Central estimate for YLD", bhd_central * dw_central * duration_central, "\n")
  cat("95% CI for YLD: [", round(yld_ci[1], 2), ", ", round(yld_ci[2], 2), "]\n", sep = "")

  return(c(bhd_central * dw_central * duration_central, round(yld_ci[1], 2), round(yld_ci[2], 2)))
}
