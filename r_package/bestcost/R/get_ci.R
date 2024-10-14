#' Get Monte Carlo confidence intervals

#' @description
#' Determine summary uncertainty based on at least one variable with central, lower and upper estimate
#' @inheritParams attribute
#' @return
#' This function returns a Monte Carlo summary uncertainty for the attributable health impacts.
#' @import dplyr
#' @import purrr
#' @importFrom stats quantile
#' @importFrom prevalence betaExpert
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
                   dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
                   erf_shape,
                   erf_increment,
                   prop_pop_exp
                   # dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper # To be added later (cf. #313)
                   ){

  # Set seed for reproducibility
  set.seed(123)

  # Set number of simulations
  n_sim <- 1000

  dat <- tibble(rr = rep(NA, times = n_sim),
                erf_increment = rep(erf_increment, times = n_sim),
                erf_shape = erf_shape,
                exp = rep(NA, times = n_sim),
                cutoff = rep(NA, times = n_sim),
                bhd = rep(NA, times = n_sim),
                dw = rep(NA, times = n_sim),
                rr_conc = rep(NA, times = n_sim),
                paf = rep(NA, times = n_sim),
                paf_weighted = rep(NA, times = n_sim),
                impact = rep(NA, times = n_sim)
  )

  # Define standard deviations (sd) & simulate values ##########################
  if (!is.null(rr_lower)){
    sd_rr <- (rr_upper - rr_lower) / (2 * 1.96)
    dat <- dat |>
      dplyr::mutate(rr = rnorm(n_sim, mean = rr_central, sd = sd_rr))
    }
  if (!is.null(exp_lower)) {
    sd_exp <- (exp_upper - exp_lower) / (2 * 1.96)
    dat <- dat |>
      dplyr::mutate(exp = rnorm(n_sim, mean = exp_central, sd = sd_exp))
    }
  if (!is.null(cutoff_lower)) {
    sd_cutoff <- (cutoff_upper - cutoff_lower) / (2 * 1.96)
    dat <- dat |>
      dplyr::mutate(cutoff = rnorm(n_sim, mean = cutoff_central, sd = sd_cutoff))
    }
  if (!is.null(bhd_lower)) {
    sd_bhd <- (bhd_upper - bhd_lower) / (2 * 1.96)
    dat <- dat |>
      dplyr::mutate(bhd = rnorm(n_sim, mean = bhd_central, sd = sd_bhd))
    }
  if (!is.null(dw_lower)) {
    sd_dw <- (dw_upper - dw_lower) / (2 * 1.96)
    dat <- dat |>
      dplyr::mutate(dw = rnorm(n_sim, mean = dw_central, sd = sd_dw))
    }


  # Calculations ###############################################################
  ## get_risk

  dat <- dat |>
    dplyr::mutate(
      rr_conc = purrr::pmap(
        list(rr = rr, exp = exp, cutoff = cutoff, erf_increment = erf_increment, erf_shape = erf_shape),
        function(rr, exp, cutoff, erf_increment, erf_shape){
            rr_conc <- get_risk(rr = rr,
                                exp = exp,
                                cutoff = cutoff,
                                erf_increment = erf_increment,
                                erf_shape = erf_shape,
                                erf_eq = NULL)
            return(rr_conc)
          }
      )
  )

  dat$rr <- unlist(dat$rr)


  ## get_pop_fraction
  dat <- dat |>
    dplyr::mutate(
      paf = purrr::pmap(
        list(rr_conc = rr_conc, prop_pop_exp = prop_pop_exp),
        function(rr_conc, prop_pop_exp){
          paf <- get_pop_fraction(rr_conc_1 = rr_conc,
                             rr_conc_2 = 1,
                             prop_pop_exp_1 = prop_pop_exp,
                             prop_pop_exp_2 = prop_pop_exp)
          return(paf)
        }
      )
    )

  dat$paf <- unlist(dat$paf)

  ## Multiply PAFs with bhd & dw
  if (!is.na(dat$dw[1])) {
    dat <- dat |>
      dplyr::mutate(paf_weighted= paf * dw) |>
      dplyr::mutate(impact = paf_weighted * bhd)
  } else {
    dat <- dat |>
      dplyr::mutate(impact = paf * bhd)
  }


  ## Calculate 95% CI ##########################################################
  ci <- quantile(x = dat |> dplyr::pull(impact) |> unlist(), probs = c(0.025, 0.5, 0.975))

  # Output results #############################################################
  ci <- unname(ci) # Unname here (to remove percentiles from the names vector
  ci <- tibble(lower_estimate = ci[1],
                      central_estimate = ci[2],
                      upper_estimate = ci[3])
  return(ci)

}
