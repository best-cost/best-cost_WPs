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

  # Determine number of geographic units
  if (length(grep("geo_id", names(res[["detailed"]][["raw"]]))) > 0) {

    n_geo <- max(res[["detailed"]][["raw"]]$geo_id_raw)

  } else {

      n_geo <- 1
      }


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
    dat <- tibble(
      geo_id_raw = rep(1:n_geo, each = n_sim),
      rr = rep(NA, times = n_sim*n_geo),
      erf_increment = rep(res[["detailed"]][["raw"]] |>
        # rep(res[["main"]] |> # Gave error for iteration pathway, as erf_increment not included in main output
                            pull(erf_increment) |>
                            first(), times = n_sim*n_geo),
      erf_shape = rep(res[["detailed"]][["raw"]] |>
                        # rep(res[["main"]] |> # Gave error for iteration pathway, as erf_increment not included in main output
                        pull(erf_shape) |>
                        first(), times = n_sim*n_geo),
      exp = rep(NA, times = n_sim*n_geo),
      cutoff = rep(NA, times = n_sim*n_geo),
      bhd = rep(NA, times = n_sim*n_geo),
      dw = rep(NA, times = n_sim*n_geo),
      rr_conc = rep(NA, times = n_sim*n_geo),
      paf = rep(NA, times = n_sim*n_geo),
      # paf_weighted = rep(NA, times = n_sim*n_geo),
      prop_pop_exp = rep(NA, times = n_sim*n_geo) # works for both single exp and exp dist
      # prop_pop_exp = rep(res[["main"]] |> # only works for single exp
      #                      pull(prop_pop_exp) |>
      #                      first(), times = n_sim*n_geo)
                  #impact = rep(NA, times = n_sim*n_geo)

    )

    ## Identify input variables with 95% uncertainty & simulate values #########

    ### rr #####################################################################

    # rr CIs & both single and multiple geo unit case
    if ( length(grep("lower", res[["detailed"]][["raw"]][["erf_ci"]])) > 0 )  {

      dat <- dat |>
        # Gamma distribution with optimization to generate simulated RR's
        dplyr::mutate(
          rr = sim_gamma(
            n_sim = n_sim*n_geo,
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
      #       n_sim*n_geo,
      #       mean = res[["detailed"]][["raw"]] |>
      #         filter(erf_ci == "central") |>
      #         pull(rr) |>
      #         first(),
      #       sd = sd_rr
      #     )
      #   )

    # No rr CIs & both single and multiple geo unit case
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

    #### Single exposure #######################################################

    # exp CIs, single geo unit
    if (( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
        ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean") ) &
        ( max(dat$geo_id_raw) == 1 )) {

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

    # No exp CIs, single geo unit
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) == 1 ) ) {

      dat <- dat |>
        dplyr::mutate(exp = res[["detailed"]][["raw"]] |>
                        filter(exp_ci == "central") |>
                        pull(exp) |>
                        first())

    # exp CIs, multiple geo units
    } else if ( ( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      # For each geo unit, fit a normal distribution and assign to dat
      # Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_exp_ci <- res[["detailed"]][["raw"]] |>
        select(geo_id_raw, exp_ci, exp) |>
        distinct() |>
        pivot_wider(
          names_from = exp_ci,
          names_prefix = "exp_",
          values_from = exp)

      simulated_data <- dat_with_exp_ci %>%
        rowwise() %>%
        mutate(
          # Generate 100 simulated values for each row
          exp_simulated = list(
            rnorm(
              100,
              mean = exp_central,
              sd = (exp_upper - exp_lower) / (2 * 1.96)
            )
          )
        ) %>%
        ungroup() %>%
        # Expand each row so each simulated value has its own row
        unnest(exp_simulated) %>%
        # Rename for clarity
        rename(exp = exp_simulated) %>%
        # Keep only relevant columns
        select(geo_id_raw, exp)

      dat <- dat |>
        select(-exp) |>
        bind_cols(simulated_data |> select(-geo_id_raw)) |>
        relocate(exp, .after = erf_shape)



    # No exp CIs, multiple geo units
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                # ( unique(res[["main"]]$exposure_type == "population_weighted_mean" ) ) & # Gave error, as no column "exposure_type" in the main iteration output
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      test <- dat |>
        select(-exp) |>
        left_join(
          x = _,
          y = res[["detailed"]][["raw"]] |>
            select(exp, geo_id_raw) |>
            distinct(),
          by = "geo_id_raw"
        ) |>
        relocate(exp, .after = erf_shape)

    }

    #### Exposure distribution #################################################

    # Exp dist case with exp CIs, single geo unit
    if (( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
        ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
        ( max(dat$geo_id_raw) == 1 ) ) {

      # Vectors needed for simulation below
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
      prop_pop_exp <- res[["detailed"]][["raw"]] |>
        filter(exp_ci == "central") |>
        pull(prop_pop_exp) |>
        unlist(x = _)

      # Simulate nsim exposure values (normal distribution) for each exp cat
      # using the corresponding values of exp_lower & exp_upper
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

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
              !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
            purrr::reduce(bind_cols))

      # Merge dat & dat_exp_dist
      dat <- cbind(dat, dat_exp_dist) |>
        select(-exp)

      # Exp dist case with no exp CIs, single geo unit
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
               ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
               ( max(dat$geo_id_raw) == 1 ) ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      exp_central <- res[["detailed"]][["raw"]] |>
        filter(exp_ci == "central") |>
        pull(exp) |>
        first() |>
        unlist(x = _)
      prop_pop_exp <- res[["detailed"]][["raw"]] |>
        filter(exp_ci == "central") |>
        first() |>
        pull(prop_pop_exp) |>
        unlist(x = _)

      # Create a column for each exposure categories and each prop_pop_exp value
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              filter(exp_ci == "central") |>
              pull(exp) |>
              first() |>
              unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central[.x],                     # .x refers to the xth element of the vector
              !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
            purrr::reduce(bind_cols))

      # Merge dat & dat_exp_dist
      dat <- cbind(dat, dat_exp_dist) |>
        select(-exp)

    # Exp dist case no exp CIs, multiple geo units
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
               ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
               ( max(dat$geo_id_raw) > 1 ) ) {

      # Add code

    # Exp dist case with exp CIs, multiple geo units
    } else if (( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
               ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
               ( max(dat$geo_id_raw) > 1 ) ) {

      # Add code

    }

    ### cutoff #################################################################

    # cutoff CIs, both single and multiple geo unit case
    if ( length(grep("lower", res[["detailed"]][["raw"]][["cutoff_ci"]])) > 0 ) {

      sd_cutoff <- #(cutoff_upper - cutoff_lower) / (2 * 1.96)
        (res[["detailed"]][["raw"]] |> filter(cutoff_ci == "upper") |> pull(cutoff) |> first() -
           res[["detailed"]][["raw"]] |> filter(cutoff_ci == "lower") |> pull(cutoff) |>  first()) / (2 * 1.96)

      dat <- dat |>
        dplyr::mutate(
          cutoff = rnorm(
            n_sim * n_geo,
            mean = res[["detailed"]][["raw"]] |>
              filter(cutoff_ci == "central") |>
              pull(cutoff) |>
              first(),
            sd = sd_cutoff))

    # No cutoff CIs, both single and multiple geo unit case
    } else if ( !length(grep("lower", res[["detailed"]][["raw"]][["cutoff_ci"]])) > 0 ) {

      dat <- dat |>
        dplyr::mutate(cutoff = res[["detailed"]][["raw"]] |>
                        filter(cutoff_ci == "central") |>
                        pull(cutoff) |>
                        first())
    }

    ### bhd ####################################################################

    # bhd CIs & single geo unit
    if ( (length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
      ( max(dat$geo_id_raw) == 1 ) ) {

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

    # No bhd CIs & single geo unit
    } else if ( !(length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) == 1 ) ) {
      dat <- dat |>
        dplyr::mutate(bhd = res[["detailed"]][["raw"]] |>
                        filter(bhd_ci == "central") |>
                        pull(bhd) |>
                        first())

    # bhd CIs & multiple geo units
    } else if ( (length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      # For each geo unit, fit a normal distribution and assign to dat
      # Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_bhd_ci <- res[["detailed"]][["raw"]] |>
        select(geo_id_raw, bhd_ci, bhd) |>
        distinct() |>
        pivot_wider(
          names_from = bhd_ci,
          names_prefix = "bhd_",
          values_from = bhd) |>
        # Assign new value to bhd_upper so that sd is not 0!!!
        mutate(bhd_upper = bhd_lower + 10000)

      simulated_data <- dat_with_bhd_ci %>%
        rowwise() %>%
        mutate(
          # Generate 100 simulated values for each row
          bhd_simulated = list(
            rnorm(
              100,
              mean = bhd_central,
              sd = (bhd_upper - bhd_lower) / (2 * 1.96)
            )
          )
        ) %>%
        ungroup() %>%
        # Expand each row so each simulated value has its own row
        unnest(bhd_simulated) %>%
        # Rename for clarity
        rename(bhd = bhd_simulated) %>%
        # Keep only relevant columns
        select(geo_id_raw, bhd)

      dat <- dat |>
        select(-bhd) |>
        bind_cols(simulated_data |> select(-geo_id_raw)) |>
        relocate(bhd, .after = cutoff)

    # No bhd CI's & multiple geo units
    } else if ( !(length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      dat <- dat |>
        select(-bhd) |>
        left_join(
          x = _,
          y = res[["detailed"]][["raw"]] |>
            select(bhd, geo_id_raw) |>
            distinct(),
          by = "geo_id_raw"
        ) |>
        relocate(bhd, .after = cutoff)

    }

    ### dw #####################################################################

    # dw CIs, both single and multiple geo unit case
    if ( (length(grep("lower", res[["detailed"]][["raw"]][["dw_ci"]])) > 0) &
         ( max(dat$geo_id_raw) == 1 ) ) {

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

    # No dw CIs, both single and multiple geo unit case
    } else if ( ("dw" %in% names(res[["detailed"]][["raw"]])) &
                ( max(dat$geo_id_raw) == 1 ) ) {
      dat <- dat |>
        dplyr::mutate(dw = res[["detailed"]][["raw"]] |>
                        filter(dw_ci == "central") |>
                        pull(dw) |>
                        first())

    # No dw inputted, both single and multiple geo unit case
    } else if ( !( "dw" %in% names(res[["detailed"]][["raw"]]) ) ) {

      dat <- dat |>
        dplyr::mutate(dw = 1)

    }

    ## rr_conc ################################################################

    ### Single exposure case ##################################################
    if ( ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) ) {

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

    ### Exposure distribution case ############################################
    } else if ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution" ) ) {

      # Calc rr_conc for each exp cat
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., until (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              filter(exp_ci == "central") |>
              pull(exp) |>
              first() |>
              unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("rr_conc_", .x) :=                     # .x refers to the xth element of the vector
                get_risk(rr = dat$rr,
                         exp = dat[[!!paste0("exp_", .x)]],
                         cutoff = dat$cutoff[1],
                         erf_increment = dat$erf_increment[1],
                         erf_shape = dat$erf_shape[1],
                         erf_eq = NULL)
              )
            )
          )



    }

    ## PAF #####################################################################
    ## Determine PAF via call to get_pop_fraction()

    if ( ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) ) {

      dat <- dat |>
        dplyr::mutate(
          paf = purrr::pmap(
            list(rr_conc = rr_conc, prop_pop_exp = res[["detailed"]][["raw"]]$prop_pop_exp |> first(x = _)),
            function(rr_conc, prop_pop_exp){
              paf <- healthiar::get_pop_fraction(rr_conc_1 = rr_conc,
                                      rr_conc_2 = 1,
                                      prop_pop_exp_1 = prop_pop_exp,
                                      prop_pop_exp_2 = prop_pop_exp)
              return(paf)
            }
          )
        )

      dat$paf <- unlist(dat$paf)

    } else if ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution" ) ) {

      # Determine product_x = rr_conc_x * prop_pop_exp_x
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., until (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              filter(exp_ci == "central") |>
              pull(exp) |>
              first() |>
              unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("product_", .x) := dat[[!!paste0("rr_conc_", .x)]] * dat[[!!paste0("prop_pop_exp_", .x)]]
            )
          )
        )

      # WORKING: CALCULATE PAF FOLLOWING EXCEL EXAMPLE FROM NIPH ####
      # excel located here: ..\best-cost\r_package\testing\input\noise_niph
      dat <- dat |>
        mutate(sum_product = rowSums(across(contains("product_")))) |>
        mutate(paf = ( sum_product - 1 ) / sum_product)


      # NOT WORKING: CALCULATE PAF WITH healthiar::get_pop_fraction() ####
      # to calculate paf per exp band
      # dat <- dat |>
      #   select(-c(rr_conc, paf, paf_weighted, prop_pop_exp))
      #
      # # Initial try (calculate a paf for each exposure category)
      # dat <- dat |>
      #   dplyr::rowwise(data = _) |> # Calculate PAF row by row
      #   dplyr::mutate(
      #     # Use dplyr::across to iterate over corresponding "rr_conc_" and "prop_pop_exp_" pairs
      #     dplyr::across(.cols = dplyr::starts_with("rr_conc_"),
      #                   .fns = ~ healthiar::get_pop_fraction(
      #                     rr_conc_1 = as.numeric(.x),
      #                     rr_conc_2 = 1,
      #                     prop_pop_exp_1 = as.numeric(dat[[gsub("rr_conc_", "prop_pop_exp_", dplyr::cur_column())]]),
      #                     prop_pop_exp_2 = as.numeric(dat[[gsub("rr_conc_", "prop_pop_exp_", dplyr::cur_column())]])),
      #                   .names = "paf_{str_remove(.col, 'rr_conc_')}")) |>
      #   # Sum impacts across noise bands to obtain total impact
      #   dplyr::mutate(paf = rowSums(across(starts_with("paf_"))))

    }


    ## Lifetable ###############################################################

    # See arguments needed for lifetable pathway below
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


    ### To be added... ############

    ## Get impact ##############################################################
    ## Multiply PAFs with bhd (& dw, if applicable)

    ### Non-lifetable case ####
    if ( !grepl("from_lifetable", res[["detailed"]][["raw"]]$health_metric[1]) ) {

      dat <- dat |>
        dplyr::mutate(impact_total = paf * bhd)

    ### YLD case (dw present)
    } else if ( (length(grep("lower", res[["detailed"]][["raw"]][["dw_ci"]])) > 0) ) {

      dat <- dat |>
        dplyr::mutate(impact_total= impact_total * dw)

      }

  # Absolute risk ##############################################################
  } else if (unique(res[["detailed"]][["raw"]]$approach_risk) == "absolute_risk") {

    # Vectors needed for simulation below
    exp_central <- res[["detailed"]][["raw"]] |>
      filter(exp_ci == "central") |>
      pull(exp) |>
      # first() |>
      unlist(x = _)
    exp_lower <- res[["detailed"]][["raw"]] |>
      filter(exp_ci == "lower") |>
      pull(exp) |>
      # first() |>
      unlist(x = _)
    exp_upper <- res[["detailed"]][["raw"]] |>
      filter(exp_ci == "upper") |>
      pull(exp) |>
      # first() |>
      unlist(x = _)
    pop_exp <- res[["detailed"]][["raw"]] |>
      filter(exp_ci == "central") |>
      pull(pop_exp) |>
      unlist(x = _)

    ## Simulate values #########################################################
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
            # first() |>
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

    ### Get impact ##############################################################
    # Calculate impact per noise band (impact_X = risk_X * pop_X)
    dat <- dat |>
      dplyr::mutate(
        # Use purrr::map2 to iterate over corresponding "risk_" and "pop_" columns
        dplyr::across(dplyr::starts_with("risk_"), ~ as.numeric(.x) * as.numeric(dat[[gsub("risk_", "pop_", dplyr::cur_column())]]),
                      .names = "impact_{str_remove(.col, 'risk_')}")) |>
      # Sum impacts across noise bands to obtain total impact
      dplyr::mutate(impact_total = rowSums(across(starts_with("impact_"))))

  }

  # Determine 95% CI of impact #################################################

  # Non-lifetable
  if (!grepl("from_lifetable", res[["detailed"]][["raw"]]$health_metric[1])) {
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
