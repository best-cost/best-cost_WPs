#' Get Monte Carlo confidence intervals

#' @description Determine summary uncertainty (based on at least one variable
#' with central, lower and upper estimate) based on attribute() or compare()
#' function output
#' @inheritParams attribute
#' @return
#' This function returns a Monte Carlo summary uncertainty for the attributable health impacts.
#' @import dplyr
#' @import purrr
#' @importFrom tibble tibble
#' @importFrom stats quantile
#' @export
#' @author Axel Luyten
#' @note Experimental function
#' @keywords internal
#' @examples
#' TBD

include_summary_uncertainty <- function(
    res) {

  # Set options
  user_options <- options()
  options(digits = 15) # Make sure that no rounding occurs

  # Set seed for reproducibility
  set.seed(123)

  # Set number of simulations
  n_sim <- 100

  # Relative risk ##############################################################

  if (unique(res[["detailed"]][["raw"]]$approach_risk) == "relative_risk") {

    ## Define helper function for RR gamma distribution with optimization)
    ## define gamma fitting and drawing functions

    # MOVE TO FUNCTION CALL OF HELPER FUNCTIONS
    # vector_rr_ci <- c(rr_lower, rr_upper)

    vector_probabilities <- c(0.025, 0.975)
    par <- 2 # shape parameter of the gamma distribution

    f_gamma <-
      function(par, central_estimate, vector_propabilities, lower_estimate, upper_estimate) {
        qfit <- qgamma(p = vector_propabilities, shape = par, rate = par / central_estimate)
        return(sum((qfit - c(lower_estimate, upper_estimate))^2))
      }

    optim_gamma <-
      function(central_estimate, lower_estimate, upper_estimate) {
        vector_propabilities <- c(0.025, 0.975)
        f <- optimize(f = f_gamma,
                      interval = c(0, 1e9),
                      central_estimate = central_estimate,
                      vector_propabilities = vector_probabilities,
                      lower_estimate = lower_estimate,
                      upper_estimate = upper_estimate)
        return(c(f$minimum, f$minimum / central_estimate))
      }

    sim_gamma <-
      function(n_sim, central_estimate, lower_estimate, upper_estimate) {
        fit <- optim_gamma(central_estimate, lower_estimate, upper_estimate)
        rgamma(n = n_sim, fit[1], fit[2])
      }

    ## Create empty tibble to store simulated values & results in
    dat <- tibble(rr = rep(NA, times = n_sim),
                  erf_increment = rep(res[["main"]] |>
                                        pull(erf_increment) |>
                                        first(), times = n_sim),
                  erf_shape = rep(res[["main"]] |>
                                    pull(erf_shape) |>
                                    first(), times = n_sim),
                  exp = rep(NA, times = n_sim),
                  cutoff = rep(NA, times = n_sim),
                  bhd = rep(NA, times = n_sim),
                  dw = rep(NA, times = n_sim),
                  rr_conc = rep(NA, times = n_sim),
                  paf = rep(NA, times = n_sim),
                  paf_weighted = rep(NA, times = n_sim),
                  prop_pop_exp = rep(res[["main"]] |>
                                       pull(prop_pop_exp) |>
                                       first(), times = n_sim)
                  #impact = rep(NA, times = n_sim)

    )

    ## Identify input variables with 95% uncertainty & simulate values #########

    ### rr #####################################################################

    if(length(grep("lower", res[["detailed"]][["raw"]][["erf_ci"]])) > 0)  {

      dat <- dat |>
        # Gamma distribution with optimization to generate simulated RR's
        dplyr::mutate(
          rr = sim_gamma(
            n_sim = n_sim,
            central_estimate =
              res[["detailed"]][["raw"]] |>
              dplyr::filter(erf_ci == "central") |>
              dplyr::pull(rr) |>
              dplyr::first(),
            lower_estimate =
              res[["detailed"]][["raw"]] |>
              filter(erf_ci == "lower") |>
              pull(rr) |>
              first(),
            upper_estimate =
              res[["detailed"]][["raw"]] |>
              filter(erf_ci == "upper") |>
              pull(rr) |>
              first()
          )
        )

      # Normal distribution (initial implementation)
      # sd_rr <- # (rr_upper - rr_lower) / (2 * 1.96)
      #   (res[["detailed"]][["raw"]] |> filter(erf_ci == "upper") |> pull(rr) |> first()
      #    -
      #      res[["detailed"]][["raw"]] |> filter(erf_ci == "lower") |> pull(rr) |>  first())
      # / (2 * 1.96)
      #
      # dat <- dat |>
      #   dplyr::mutate(
      #     rr = rnorm(
      #       n_sim,
      #       mean = res[["detailed"]][["raw"]] |>
      #         filter(erf_ci == "central") |>
      #         pull(rr) |>
      #         first(),
      #       sd = sd_rr
      #     )
      #   )

    } else {
      dat <- dat |>
        dplyr::mutate(
          rr = res[["detailed"]][["raw"]] |>
            filter(erf_ci == "central") |>
            pull(rr) |>
            first()
        )
    }

    ### exp ####################################################################

    if (length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0) {
      sd_exp <- #(exp_upper - exp_lower) / (2 * 1.96)
      (res[["detailed"]][["raw"]] |> filter(exp_ci == "upper") |> pull(exp) |> first() -
          res[["detailed"]][["raw"]] |> filter(exp_ci == "lower") |> pull(exp) |>  first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          exp = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
                      filter(exp_ci == "central") |>
                      pull(exp) |>
                      first(),
            sd = sd_exp))
    } else {
      dat <- dat |>
        dplyr::mutate(exp = res[["detailed"]][["raw"]] |>
                        filter(exp_ci == "central") |>
                        pull(exp) |>
                        first())}

    ### cutoff #################################################################

    if (length(grep("lower", res[["detailed"]][["raw"]][["cutoff_ci"]])) > 0) {
      sd_cutoff <- #(cutoff_upper - cutoff_lower) / (2 * 1.96)
        (res[["detailed"]][["raw"]] |> filter(cutoff_ci == "upper") |> pull(cutoff) |> first() -
           res[["detailed"]][["raw"]] |> filter(cutoff_ci == "lower") |> pull(cutoff) |>  first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          cutoff = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
              filter(cutoff_ci == "central") |>
              pull(cutoff) |>
              first(),
            sd = sd_cutoff))
    } else {
      dat <- dat |>
        dplyr::mutate(cutoff = res[["detailed"]][["raw"]] |>
                        filter(cutoff_ci == "central") |>
                        pull(cutoff) |>
                        first())}

    ### bhd ####################################################################

    if (length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) {
      sd_bhd <- #(bhd_upper - bhd_lower) / (2 * 1.96)
        (res[["detailed"]][["raw"]] |> filter(bhd_ci == "upper") |> pull(bhd) |> first() -
           res[["detailed"]][["raw"]] |> filter(bhd_ci == "lower") |> pull(bhd) |>  first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          bhd = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
              filter(bhd_ci == "central") |>
              pull(bhd) |>
              first(),
            sd = sd_bhd))
    } else {
      dat <- dat |>
        dplyr::mutate(bhd = res[["detailed"]][["raw"]] |>
                        filter(bhd_ci == "central") |>
                        pull(bhd) |>
                        first())}

    ### dw #####################################################################

    if (length(grep("lower", res[["detailed"]][["raw"]][["dw_ci"]])) > 0) {

      # Using beta distribution using prevalence::betaExpert()
      # dw_sim <- prevalence::betaExpert(dw_central, dw_lower, dw_upper, method = "mean")
      # dat <- dat |>
      #   dplyr::mutate(
      #       dw = rbeta(
      #         n = 1000,
      #         shape1 = as.numeric(unname(dw_sim["alpha"])),
      #         shape2 = as.numeric(unname(dw_sim["beta"]))))

      # Using beta distribution using qbeta()
      # dat <- dat |>
      #   dplyr::mutate(dw = sim_beta(n_sim = n_sim,
      #                               dw_central = dw_central,
      #                               vector_dw_ci = vector_dw_ci))

      # Using normal distribution
      sd_dw <- #(dw_upper - dw_lower) / (2 * 1.96)
        (res[["detailed"]][["raw"]] |> filter(dw_ci == "upper") |> pull(dw) |> first() -
           res[["detailed"]][["raw"]] |> filter(dw_ci == "lower") |> pull(dw) |>  first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          dw = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
              filter(dw_ci == "central") |>
              pull(dw) |>
              first(),
            sd = sd_dw))
    } else if ("dw" %in% names(res[["detailed"]][["raw"]])) {
      dat <- dat |>
        dplyr::mutate(dw = res[["detailed"]][["raw"]] |>
                        filter(dw_ci == "central") |>
                        pull(dw) |>
                        first())}

    # Determine rr_conc using call to healthiar::get_risk()
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

    ## Determine PAF via call to get_pop_fraction()
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

    ## Lifetable ###############################################################

    # To be added... ############

    ## Multiply PAFs with bhd (& dw, if applicable)
    if ( (length(grep("lower", res[["detailed"]][["raw"]][["dw_ci"]])) > 0) & "dw" %in% names(dat) ) {
      dat <- dat |>
        dplyr::mutate(paf_weighted= paf * dw) |>
        dplyr::mutate(impact_total = paf_weighted * bhd)

    } else if (!grepl("from_lifetable", res[["main"]]$health_metric[1])) {

      dat <- dat |>
        dplyr::mutate(impact_total = paf * bhd)
    }


  # Absolute risk ##############################################################
} else if (unique(res[["detailed"]][["raw"]]$approach_risk) == "absolute_risk") {

  # Exposure vectors for simulation below
  exp_central <- res[["detailed"]][["raw"]] |>
    filter(exp_ci == "central") |>
    pull(exp) |>
    first() |>
    unlist(x = _)
  exp_lower <- res[["detailed"]][["raw"]] |>
    filter(exp_ci == "lower") |>
    pull(exp) |>
    first() |>
    unlist(x = _)
  exp_upper <- res[["detailed"]][["raw"]] |>
    filter(exp_ci == "upper") |>
    pull(exp) |>
    first() |>
    unlist(x = _)
  pop_exp <- res[["detailed"]][["raw"]] |>
    filter(exp_ci == "central") |>
    pull(pop_exp) |>
    unlist(x = _)

  # Create (empty) tibble to store simulated values & results in
  dat <- tibble::tibble(
    row_id = 1:n_sim) |>

    # Simulate 1000 exposure values (normal distribution) for each noise band
    # using the corresponding values of exp_lower & exp_upper
    dplyr::bind_cols(
      purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
        res[["detailed"]][["raw"]] |>
          filter(exp_ci == "central") |>
          pull(exp) |>
          first() |>
          unlist(x = _)),
        .f = ~ tibble::tibble(
          !!paste0("exp_", .x) :=                     # .x refers to the xth element of the vector
            rnorm(n_sim,
                  mean = exp_central[.x],
                  sd = (exp_upper[.x] - exp_lower[.x]) / (2 * 1.96)),
          !!paste0("pop_", .x) := pop_exp[.x])) |>
        purrr::reduce(bind_cols))

  # Calculate risk for each noise band
  dat <- dat |>
    dplyr::mutate(
      dplyr::across(.cols = dplyr::starts_with("exp_"),
                    .fns = ~ healthiar::get_risk(exp = .x, erf_eq = res[["main"]]$erf_eq |> first(x = _)) / 100,
                    .names = "risk_{str_remove(.col, 'exp_')}")
    )

  # Calculate impact per noise band (impact_X = risk_X * pop_X)
  dat <- dat |>
    dplyr::mutate(
      # Use purrr::map2 to iterate over corresponding "risk_" and "pop_" columns
      dplyr::across(dplyr::starts_with("risk_"), ~ as.numeric(.x) * as.numeric(dat[[gsub("risk_", "pop_", dplyr::cur_column())]]),
                    .names = "impact_{str_remove(.col, 'risk_')}")) |>
    # Sum impacts across noise bands to obtain total impact
    dplyr::mutate(impact_total = rowSums(across(starts_with("impact_"))))

}

  # Determine 95% CI of impact ################################################

  # Non-lifetable
  if (!grepl("from_lifetable", res[["main"]]$health_metric[1])) {
    ci <- quantile(x = dat |> dplyr::pull(impact_total) |> unlist(),
                   probs = c(0.025, 0.5, 0.975))

    ci <- unname(ci) # Unname to remove percentiles from the names vector
    ci <- tibble(central_estimate = ci[2],
                 lower_estimate = ci[1],
                 upper_estimate = ci[3])

    # Lifetable
  } else if (grepl("from_lifetable", res[["main"]]$health_metric[1])) {

    ci_male <- quantile(
      x = dat$detailed$step_by_step_from_lifetable |> filter(sex == "male") |> dplyr::pull(impact) |> unlist(),
      probs = c(0.025, 0.5, 0.975)) |>
      unname()
    ci_male <- tibble(
      sex = "male",
      central_estimate = ci_male[2],
      lower_estimate = ci_male[1],
      upper_estimate = ci_male[3])

    ci_female <- quantile(
      x = dat$detailed$step_by_step_from_lifetable |> filter(sex == "female") |> dplyr::pull(impact) |> unlist(),
      probs = c(0.025, 0.5, 0.975)) |>
      unname()
    ci_female <- tibble(
      sex = "female",
      central_estimate = ci_female[2],
      lower_estimate = ci_female[1],
      upper_estimate = ci_female[3])

    ci_total <- quantile(
      x = dat$detailed$step_by_step_from_lifetable |> filter(sex == "total") |> dplyr::pull(impact) |> unlist(),
      probs = c(0.025, 0.5, 0.975)) |>
      unname()
    ci_total <- tibble(
      sex = "total",
      central_estimate = ci_total[2],
      lower_estimate = ci_total[1],
      upper_estimate = ci_total[3])

    ci <- rbind(ci_male,
                ci_female,
                ci_total)
  }

  on.exit(options(user_options))

  res[["detailed"]][["uncertainty"]] <- ci

  return(res)

}

#   get_ci <- function(
#     rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
#     exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
#     cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
#     bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
#     dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
#     erf_shape = NULL,
#     erf_increment = NULL,
#     erf_eq = NULL,
#     prop_pop_exp = NULL,
#     approach_risk,
#     # Lifetable
#     year_of_analysis = NULL,
#     input = NULL, # contains column "lifetable_with_pop_nest"
#     health_metric = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     approach_exposure = NULL,
#     pop_exp = NULL) { # in absolute risk case
#
# }
