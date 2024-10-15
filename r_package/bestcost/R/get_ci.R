#' Get Monte Carlo confidence intervals

#' @description
#' Determine summary uncertainty based on at least one variable with central, lower and upper estimate
#' @inheritParams attribute
#' @return
#' This function returns a Monte Carlo summary uncertainty for the attributable health impacts.
#' @import dplyr
#' @import purrr
#' @importFrom tibble tibble
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
                   erf_shape = NULL,
                   erf_increment = NULL,
                   erf_eq = NULL,
                   prop_pop_exp = NULL,
                   approach_risk,
                   pop_exp = NULL # in absolute risk case
                   ){

  user_options <- options()
  options(digits = 15) # Make sure that no rounding occurs

  # Set seed for reproducibility
  set.seed(123)

  # Set number of simulations
  n_sim <- 1000

  # Calculations ###############################################################

  ## Relative risk ########################################################

  if (approach_risk == "relative_risk") {

    # Create tibble to store simulated values & results in
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

    ## Define standard deviations (sd) & simulate values #######################

    if (!is.null(rr_lower)){
      sd_rr <- (rr_upper - rr_lower) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(rr = rnorm(n_sim, mean = rr_central, sd = sd_rr))
    } else {
      dat <- dat |>
        dplyr::mutate(rr = rr_central)}

    if (!is.null(exp_lower)) {
      sd_exp <- (exp_upper - exp_lower) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(exp = rnorm(n_sim, mean = exp_central, sd = sd_exp))
    } else {
      dat <- dat |>
        dplyr::mutate(exp = exp_central)}

    if (!is.null(cutoff_lower)) {
      sd_cutoff <- (cutoff_upper - cutoff_lower) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(cutoff = rnorm(n_sim, mean = cutoff_central, sd = sd_cutoff))
    } else {
      dat <- dat |>
        dplyr::mutate(cutoff = cutoff_central)}

    if (!is.null(bhd_lower)) {
      sd_bhd <- (bhd_upper - bhd_lower) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(bhd = rnorm(n_sim, mean = bhd_central, sd = sd_bhd))
    }else {
      dat <- dat |>
        dplyr::mutate(bhd = bhd_central)}

    if (!is.null(dw_lower)) {
      sd_dw <- (dw_upper - dw_lower) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(dw = rnorm(n_sim, mean = dw_central, sd = sd_dw))
    } else {
      dat <- dat |>
        dplyr::mutate(dw = dw_central)
    }

    dat <- dat |>
      # get_risk
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


    dat <- dat |>
      # get_pop_fraction
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

    ## Multiply PAFs with bhd (& dw)
    if ( !is.null(dw_central) & "dw" %in% names(dat) ) {
      dat <- dat |>
        dplyr::mutate(paf_weighted= paf * dw) |>
        dplyr::mutate(impact_total = paf_weighted * bhd)
    } else {
      dat <- dat |>
        dplyr::mutate(impact_total = paf * bhd)
    }

  # Absolute risk ##############################################################
  } else if (approach_risk == "absolute_risk"){

    # Create tibble to store simulated values & results in
    dat <- tibble::tibble(
      row_id = 1:n_sim) |>   # Add an ID column for the 1000 rows

      # Simulate 1000 exposure values (normal distribution) for each noise band
      # using the corresponding values of exp_lower & exp_upper
      dplyr::bind_cols(
        purrr::map(seq_along(exp_central),
            ~ tibble::tibble(
              !!paste0("exp_", .x) := rnorm(n_sim,
                                            mean = exp_central[.x],
                                            sd = (exp_upper[.x] - exp_lower[.x]) / (2 * 1.96)),
              !!paste0("pop_", .x) := pop_exp[.x]
            )) %>%
          purrr::reduce(bind_cols))

    # Calculate risks
    dat <- dat |>
      dplyr::mutate(
        dplyr::across(.cols = dplyr::starts_with("exp_"),
                      .fns = ~ bestcost::get_risk(exp = .x, erf_eq = erf_eq) / 100,
                      .names = "risk_{str_remove(.col, 'exp_')}")
      )


    # Calculate impact_X = risk_X * pop_X
    dat <- dat |>
      dplyr::mutate(
        # Use purrr::map2 to iterate over corresponding "risk_" and "pop_" columns
        dplyr::across(dplyr::starts_with("risk_"), ~ as.numeric(.x) * as.numeric(dat[[gsub("risk_", "pop_", dplyr::cur_column())]]),
               .names = "impact_{str_remove(.col, 'risk_')}")) |>
      # Sum impacts
      dplyr::mutate(impact_total = rowSums(across(starts_with("impact_"))))

  }


  ## Calculate 95% CI of impact ################################################
  ci <- quantile(x = dat |> dplyr::pull(impact_total) |> unlist(), probs = c(0.025, 0.5, 0.975))

  # Output results #############################################################
  ci <- unname(ci) # Unname to remove percentiles from the names vector
  ci <- tibble(central_estimate = ci[2],
               lower_estimate = ci[1],
               upper_estimate = ci[3])

  on.exit(options(user_options))

  return(ci)

}
