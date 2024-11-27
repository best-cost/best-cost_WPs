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
#' @importFrom tidyr unnest_wider unnest pivot_wider
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
  if ( length(grep("geo_id", names(res[["detailed"]][["raw"]]))) > 0 ) {

    n_geo <- as.numeric(max(res[["detailed"]][["raw"]]$geo_id_raw))

  } else {

      n_geo <- 1
      }


  # Relative risk ##############################################################

  if ( unique(res[["detailed"]][["raw"]]$approach_risk) == "relative_risk" ) {

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
    dat <- tibble::tibble(
      geo_id_raw = rep(1:n_geo, each = n_sim),
      rr = rep(NA, times = n_sim*n_geo),
      erf_increment = rep(res[["detailed"]][["raw"]] |>
        # rep(res[["main"]] |> # Gave error for iteration pathway, as erf_increment not included in main output
                            dplyr::pull(erf_increment) |>
                            dplyr::first(), times = n_sim*n_geo),
      erf_shape = rep(res[["detailed"]][["raw"]] |>
                        # rep(res[["main"]] |> # Gave error for iteration pathway, as erf_increment not included in main output
                        dplyr::pull(erf_shape) |>
                        dplyr::first(), times = n_sim*n_geo),
      exp = rep(NA, times = n_sim*n_geo),
      cutoff = rep(NA, times = n_sim*n_geo),
      bhd = rep(NA, times = n_sim*n_geo),
      dw = rep(NA, times = n_sim*n_geo),
      rr_conc = rep(NA, times = n_sim*n_geo),
      paf = rep(NA, times = n_sim*n_geo),
      # paf_weighted = rep(NA, times = n_sim*n_geo),
      prop_pop_exp = rep(NA, times = n_sim*n_geo) # works for both single exp and exp dist
      # prop_pop_exp = rep(res[["main"]] |> # only works for single exp
      #                      dplyr::pull(prop_pop_exp) |>
      #                      dplyr::first(), times = n_sim*n_geo)
                  #impact = rep(NA, times = n_sim*n_geo)

    )

    # * Identify input variables with 95% uncertainty & simulate values #########

    # * * rr #####################################################################

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
              dplyr::filter(erf_ci == "lower") |>
              dplyr::pull(rr) |>
              dplyr::first(),
            upper_estimate =
              res[["detailed"]][["raw"]] |>
              dplyr::filter(erf_ci == "upper") |>
              dplyr::pull(rr) |>
              dplyr::first()
          )
        )

      # Normal distribution (initial implementation)
      # sd_rr <- # (rr_upper - rr_lower) / (2 * 1.96)
      #   (res[["detailed"]][["raw"]] |> dplyr::filter(erf_ci == "upper") |> dplyr::pull(rr) |> dplyr::first()
      #    -
      #      res[["detailed"]][["raw"]] |> dplyr::filter(erf_ci == "lower") |> dplyr::pull(rr) |>  dplyr::first())
      # / (2 * 1.96)
      #
      # dat <- dat |>
      #   dplyr::mutate(
      #     rr = rnorm(
      #       n_sim*n_geo,
      #       mean = res[["detailed"]][["raw"]] |>
      #         dplyr::filter(erf_ci == "central") |>
      #         dplyr::pull(rr) |>
      #         dplyr::first(),
      #       sd = sd_rr
      #     )
      #   )

    # No rr CIs & both single and multiple geo unit case
    } else {

      dat <- dat |>
        dplyr::mutate(
          rr = res[["detailed"]][["raw"]] |>
            dplyr::filter(erf_ci == "central") |>
            dplyr::pull(rr) |>
            dplyr::first()
        )
    }

    # * * exp ####################################################################

    # * * * Single exposure #######################################################

    # exp CIs, single geo unit
    if (( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
        ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean") ) &
        ( max(dat$geo_id_raw) == 1 )) {

      sd_exp <- #(exp_upper - exp_lower) / (2 * 1.96)
      (res[["detailed"]][["raw"]] |> dplyr::filter(exp_ci == "upper") |> dplyr::pull(exp) |> dplyr::first() -
          res[["detailed"]][["raw"]] |> dplyr::filter(exp_ci == "lower") |> dplyr::pull(exp) |>  dplyr::first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          exp = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
                      dplyr::filter(exp_ci == "central") |>
                      dplyr::pull(exp) |>
                      dplyr::first(),
            sd = sd_exp))

    # No exp CIs, single geo unit
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) == 1 ) ) {

      dat <- dat |>
        dplyr::mutate(exp = res[["detailed"]][["raw"]] |>
                        dplyr::filter(exp_ci == "central") |>
                        dplyr::pull(exp) |>
                        dplyr::first())

    # exp CIs, multiple geo units
    } else if ( ( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      # For each geo unit, fit a normal distribution and assign to dat
      # Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_exp_ci <- res[["detailed"]][["raw"]] |>
        dplyr::select(geo_id_raw, exp_ci, exp) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = exp_ci,
          names_prefix = "exp_",
          values_from = exp)

      simulated_data <- dat_with_exp_ci %>%
        rowwise() %>%
        dplyr::mutate(
          # Generate 100 simulated values for each row
          exp_simulated = list(
            rnorm(
              100,
              mean = exp_central,
              sd = (exp_upper - exp_lower) / (2 * 1.96)
            )
          )
        ) %>%
        dplyr::ungroup() %>%
        # Expand each row so each simulated value has its own row
        tidyr::unnest(exp_simulated) %>%
        # dplyr::rename for clarity
        dplyr::rename(exp = exp_simulated) %>%
        # Keep only relevant columns
        dplyr::select(geo_id_raw, exp)

      dat <- dat |>
        dplyr::select(-exp) |>
        dplyr::bind_cols(simulated_data |> dplyr::select(-geo_id_raw)) |>
        dplyr::relocate(exp, .after = erf_shape)



    # No exp CIs, multiple geo units
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                # ( unique(res[["main"]]$exposure_type == "population_weighted_mean" ) ) & # Gave error, as no column "exposure_type" in the main iteration output
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      test <- dat |>
        dplyr::select(-exp) |>
        dplyr::left_join(
          x = _,
          y = res[["detailed"]][["raw"]] |>
            dplyr::select(exp, geo_id_raw) |>
            dplyr::distinct(),
          by = "geo_id_raw"
        ) |>
        dplyr::relocate(exp, .after = erf_shape)

    }

    # * * *  Exposure distribution #################################################

    # * * *  * Exp dist case with exp CIs, single geo unit ##########################
    if ( ( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
         ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
         ( max(dat$geo_id_raw) == 1 ) ) {

      # Vectors needed for simulation below
      exp_central <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      exp_lower <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "lower") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      exp_upper <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "upper") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      prop_pop_exp <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::pull(prop_pop_exp) |>
        base::unlist(x = _)

      # Simulate nsim exposure values (normal distribution) for each exp cat
      # using the corresponding values of exp_lower & exp_upper
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              dplyr::pull(exp) |>
              dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) :=                     # .x refers to the xth element of the vector
                rnorm(n_sim,
                      mean = exp_central[.x],
                      sd = (exp_upper[.x] - exp_lower[.x]) / (2 * 1.96)),
              !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
            purrr::reduce(bind_cols))

      # Merge dat & dat_exp_dist
      dat <- cbind(dat, dat_exp_dist) |>
        dplyr::select(-exp)

      # * * * * Exp dist case with no exp CIs, single geo unit ####
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
                ( max(dat$geo_id_raw) == 1 ) ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      exp_central <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      prop_pop_exp <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::first() |>
        dplyr::pull(prop_pop_exp) |>
        base::unlist(x = _)

      # Create a column for each exposure categories and each prop_pop_exp value
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              dplyr::pull(exp) |>
              dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central[.x],                     # .x refers to the xth element of the vector
              !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
            purrr::reduce(bind_cols))

      # Merge dat & dat_exp_dist
      dat <- cbind(dat, dat_exp_dist) |>
        dplyr::select(-exp)

      # * * * * Exp dist case no exp CIs, multiple geo units #######################
    } else if ( !( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      # Extract exposure values per geo_id_raw and save in a sub-list

      dat <- dat %>%
        select(-exp, -prop_pop_exp) |>
        left_join(res[["detailed"]][["raw"]] |>
                    dplyr::filter(erf_ci == "central") |>
                    dplyr::select(geo_id_raw, exp, prop_pop_exp),
                  by = "geo_id_raw") %>%
        tidyr::unnest_wider(c(exp, prop_pop_exp), names_sep = "_")

      # * * * *  Exp dist case with exp CIs, multiple geo units #######################
    } else if ( ( length(grep("lower", res[["detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      # Goal: Generate a tibble with 1000 simulations per exposure category for each geo_id

      # Vectors needed for simulation of exposure values (exp_central, exp_lower, exp_upper & prop_pop_exp central)
      # NOTE: exp central, lower & upper has to be compiled for each geo unit

      exp_central <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::select(geo_id_raw, exp_central = exp)

      exp_lower <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "lower") |>
        dplyr::select(geo_id_raw, exp_lower = exp)

      exp_upper <- res[["detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "upper") |>
        dplyr::select(geo_id_raw, exp_upper = exp)

      dat_exp <- cbind(exp_central,
                       exp_lower |> select(-geo_id_raw),
                       exp_upper |> select(-geo_id_raw)
                       )

      prop_pop_exp <- res[["detailed"]][["raw"]] |>
        dplyr::select(geo_id_raw, prop_pop_exp) |>
        distinct(geo_id_raw, .keep_all = TRUE)

      # prop_pop_exp <- res[["detailed"]][["raw"]] |>
      #   dplyr::filter(exp_ci == "central") |>
      #   dplyr::first() |>
      #   dplyr::pull(prop_pop_exp) |>
      #   base::unlist(x = _)

      # Create vectors of column names
      exp_columns <- paste0("exp_", dat_exp |>
                              filter(geo_id_raw == 1) |>
                              pull(exp_central) |>
                              unlist() |>
                              seq_along())
      prop_columns <- paste0("prop_pop_exp_", dat_exp |>
                               filter(geo_id_raw == 1) |>
                               pull(exp_central) |>
                               unlist() |>
                               seq_along())

      # Create empty tibble to be filled in loop below
      # @ AC: sorry for the loop : P
      dat_sim <- tibble(
        geo_id_raw = numeric(0)  # Initialize geo_id_raw as numeric
      ) |>
        bind_cols(
          set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
          set_names(rep(list(numeric(0)), length(prop_columns)), prop_columns)
        )

      for (i in exp_central$geo_id_raw){
      # for (i in 1){ ## To test

        temp <- tibble::tibble(
          geo_id_raw = rep(i, times = n_sim)) |>
          bind_cols(
            purrr::map(
              .x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
                dat_exp |>
                  dplyr::filter(geo_id_raw == i) |>
                  dplyr::pull(exp_central) |>
                  base::unlist(x = _)
              ),
              .f = ~ tibble::tibble(
                !!paste0("exp_", .x) :=
                  # For each exposure category generate n_sim simulated values
                  rnorm(
                    n_sim,
                    mean = dat_exp |> filter(geo_id_raw == i) |> pull(exp_central) |> unlist(x = _) |> nth(.x) ,
                    sd = ( dat_exp |> filter(geo_id_raw == i) |> pull(exp_upper) |> unlist(x = _) |> nth(.x) -
                             dat_exp |> filter(geo_id_raw == i) |> pull(exp_lower) |> unlist(x = _) |> nth(.x) ) / (2 * 1.96) # Formula: exp_upper - exp_lower) / (2 * 1.96)
                  ),
                !!paste0("prop_pop_exp_", .x) := prop_pop_exp|> filter(geo_id_raw == i) |> pull(prop_pop_exp) |> unlist(x = _) |> nth(.x)
              )) |>
              purrr::reduce(bind_cols)
          )

        dat_sim <- dat_sim |>
          bind_rows(temp)

      }

      # Add simulated values to dat tibble
      dat <- dat |>
        bind_cols(dat_sim |> select(-geo_id_raw)) |>
        select(-exp)

      # INITIAL DEVELOPMENT CODE
      ## Generate 1000 simulations for 1st exp category
      # test <- rnorm(
      #   n_sim,
      #   mean = dat_exp$exp_central[1] |> unlist(x = _) |> nth(1),
      #   sd = (dat_exp$exp_upper[1] |> unlist(x = _) |> nth(1) - dat_exp$exp_lower[1] |> unlist(x = _) |> nth(1) ) / (2 * 1.96) # Formula: exp_upper - exp_lower) / (2 * 1.96)
      # )
      ## Generate a tibble with 1000 simulations per exposure category for one geo_id
      # test_central <- tibble::tibble(
      #   row_id = 1:(n_sim)) |>
      #     bind_cols(
      #     purrr::map(
      #       .x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
      #         dat_exp |>
      #           dplyr::filter(geo_id_raw == 1) |>
      #           dplyr::pull(exp_central) |>
      #           base::unlist(x = _)
      #         ),
      #       .f = ~ tibble::tibble(
      #         !!paste0("exp_", .x) :=
      #               # For each exposure category generate n_sim simulated values
      #               rnorm(
      #                 n_sim,
      #                 mean = dat_exp$exp_central[1] |> unlist(x = _) |> nth(.x) ,
      #                 sd = ( dat_exp$exp_upper[1] |> unlist(x = _) |> nth(.x) - dat_exp$exp_lower[1] |> unlist(x = _) |> nth(.x) ) / (2 * 1.96) # Formula: exp_upper - exp_lower) / (2 * 1.96)
      #                 ),
      #         !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x]
      #       )) |>
      #       purrr::reduce(bind_cols)
      #   )
      ## Code copied from other section
      ## Create a column for each exposure categories and each prop_pop_exp value
      # dat_exp_dist <- tibble::tibble(
      #   row_id = 1:n_sim) |>
      #
      #   dplyr::bind_cols(
      #     purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
      #       res[["detailed"]][["raw"]] |>
      #         dplyr::filter(exp_ci == "central") |>
      #         dplyr::pull(exp) |>
      #         dplyr::first() |>
      #         base::unlist(x = _)),
      #       .f = ~ tibble::tibble(
      #         !!paste0("exp_", .x) := exp_central[.x],                     # .x refers to the xth element of the vector
      #         !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
      #       purrr::reduce(bind_cols))
      #
      # # Merge dat & dat_exp_dist
      # dat <- cbind(dat, dat_exp_dist) |>
      #   dplyr::select(-exp)


    }

    # * * cutoff ##############################################################

    # * * * cutoff CIs, both single and multiple geo unit case ####################
    if ( length(grep("lower", res[["detailed"]][["raw"]][["cutoff_ci"]])) > 0 ) {

      sd_cutoff <- #(cutoff_upper - cutoff_lower) / (2 * 1.96)
        (res[["detailed"]][["raw"]] |> dplyr::filter(cutoff_ci == "upper") |> dplyr::pull(cutoff) |> dplyr::first() -
           res[["detailed"]][["raw"]] |> dplyr::filter(cutoff_ci == "lower") |> dplyr::pull(cutoff) |>  dplyr::first()) / (2 * 1.96)

      dat <- dat |>
        dplyr::mutate(
          cutoff = rnorm(
            n_sim * n_geo,
            mean = res[["detailed"]][["raw"]] |>
              dplyr::filter(cutoff_ci == "central") |>
              dplyr::pull(cutoff) |>
              dplyr::first(),
            sd = sd_cutoff))

    # * * * No cutoff CIs, both single and multiple geo unit case #################
    } else if ( !length(grep("lower", res[["detailed"]][["raw"]][["cutoff_ci"]])) > 0 ) {

      dat <- dat |>
        dplyr::mutate(cutoff = res[["detailed"]][["raw"]] |>
                        dplyr::filter(cutoff_ci == "central") |>
                        dplyr::pull(cutoff) |>
                        dplyr::first())
    }

    # * * bhd ####################################################################

    # * * * bhd CIs & single geo unit #############################################
    if ( (length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
      ( max(dat$geo_id_raw) == 1 ) ) {

      sd_bhd <- #(bhd_upper - bhd_lower) / (2 * 1.96)
        (res[["detailed"]][["raw"]] |> dplyr::filter(bhd_ci == "upper") |> dplyr::pull(bhd) |> dplyr::first() -
           res[["detailed"]][["raw"]] |> dplyr::filter(bhd_ci == "lower") |> dplyr::pull(bhd) |>  dplyr::first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          bhd = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
              dplyr::filter(bhd_ci == "central") |>
              dplyr::pull(bhd) |>
              dplyr::first(),
            sd = sd_bhd))

    # * * * No bhd CIs & single geo unit ##########################################
    } else if ( !(length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) == 1 ) ) {
      dat <- dat |>
        dplyr::mutate(bhd = res[["detailed"]][["raw"]] |>
                        dplyr::filter(bhd_ci == "central") |>
                        dplyr::pull(bhd) |>
                        dplyr::first())

    # * * * bhd CIs & multiple geo units ##########################################
    } else if ( (length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      # For each geo unit, fit a normal distribution and assign to dat
      # Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_bhd_ci <- res[["detailed"]][["raw"]] |>
        dplyr::select(geo_id_raw, bhd_ci, bhd) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = bhd_ci,
          names_prefix = "bhd_",
          values_from = bhd) |>
        # Assign new value to bhd_upper so that sd is not 0!!!
        dplyr::mutate(bhd_upper = bhd_lower + 10000)

      simulated_data <- dat_with_bhd_ci %>%
        rowwise() %>%
        dplyr::mutate(
          # Generate 100 simulated values for each row
          bhd_simulated = list(
            rnorm(
              100,
              mean = bhd_central,
              sd = (bhd_upper - bhd_lower) / (2 * 1.96)
            )
          )
        ) %>%
        dplyr::ungroup() %>%
        # Expand each row so each simulated value has its own row
        tidyr::unnest(bhd_simulated) %>%
        # dplyr::rename for clarity
        dplyr::rename(bhd = bhd_simulated) %>%
        # Keep only relevant columns
        dplyr::select(geo_id_raw, bhd)

      dat <- dat |>
        dplyr::select(-bhd) |>
        dplyr::bind_cols(simulated_data |> dplyr::select(-geo_id_raw)) |>
        dplyr::relocate(bhd, .after = cutoff)

    # * * * No bhd CI's & multiple geo units ######################################
    } else if ( !(length(grep("lower", res[["detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      dat <- dat |>
        dplyr::select(-bhd) |>
        dplyr::left_join(
          x = _,
          y = res[["detailed"]][["raw"]] |>
            dplyr::select(bhd, geo_id_raw) |>
            dplyr::distinct(),
          by = "geo_id_raw"
        ) |>
        dplyr::relocate(bhd, .after = cutoff)

    }

    # * * dw #####################################################################

    # * * * dw CIs, both single and multiple geo unit case ########################
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
        (res[["detailed"]][["raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first() -
           res[["detailed"]][["raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          dw = rnorm(
            n_sim,
            mean = res[["detailed"]][["raw"]] |>
              dplyr::filter(dw_ci == "central") |>
              dplyr::pull(dw) |>
              dplyr::first(),
            sd = sd_dw))

    # * * * No dw CIs, both single and multiple geo unit case #####################
    } else if ( ("dw" %in% names(res[["detailed"]][["raw"]])) &
                ( max(dat$geo_id_raw) == 1 ) ) {
      dat <- dat |>
        dplyr::mutate(dw = res[["detailed"]][["raw"]] |>
                        dplyr::filter(dw_ci == "central") |>
                        dplyr::pull(dw) |>
                        dplyr::first())

    # * * * No dw inputted, both single and multiple geo unit case ################
    } else if ( !( "dw" %in% names(res[["detailed"]][["raw"]]) ) ) {

      dat <- dat |>
        dplyr::mutate(dw = 1)

    }

    # * rr_conc ################################################################

    # * * Single exposure case ##################################################
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

    dat$rr <- base::unlist(dat$rr)

    # * * Exposure distribution case ############################################
    } else if ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution" ) ) {

      # Calc rr_conc for each exp cat
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., until (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              dplyr::pull(exp) |>
              dplyr::first() |>
              base::unlist(x = _)),
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

    # * PAF #####################################################################


    # * * Single exposure case ###################################################

    ## Determine PAF via call to get_pop_fraction()
    if ( ( unique(res[["detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) ) {

      dat <- dat |>
        dplyr::mutate(
          paf = purrr::pmap(
            list(rr_conc = rr_conc, prop_pop_exp = res[["detailed"]][["raw"]]$prop_pop_exp |> dplyr::first(x = _)),
            function(rr_conc, prop_pop_exp){
              paf <- healthiar::get_pop_fraction(rr_conc_1 = rr_conc,
                                      rr_conc_2 = 1,
                                      prop_pop_exp_1 = prop_pop_exp,
                                      prop_pop_exp_2 = prop_pop_exp)
              return(paf)
            }
          )
        )

      dat$paf <- base::unlist(dat$paf)

    # * * Exposure distribution case #############################################

    } else if ( unique(res[["detailed"]][["raw"]]$exposure_type == "exposure_distribution" ) ) {

      # Determine product_x = rr_conc_x * prop_pop_exp_x
      # i.e. intermediate step towards PAF calculation
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., until (nr. of exposure categories)
            res[["detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              dplyr::pull(exp) |>
              dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("product_", .x) := dat[[!!paste0("rr_conc_", .x)]] * dat[[!!paste0("prop_pop_exp_", .x)]]
            )
          )
        )

      # * * * WORKING: CALCULATE PAF FOLLOWING EXCEL EXAMPLE FROM NIPH ############
      # excel located here: ..\best-cost\r_package\testing\input\noise_niph
      # NOTE 2024-11-26: paf matches the result in the Excel sheet "Relative_risk_IHD_WHO_2003a" exactly
      dat <- dat |>
        dplyr::mutate(sum_product = rowSums(across(contains("product_")))) |>
        dplyr::mutate(paf = ( sum_product - 1 ) / sum_product)


      # * * * NOT WORKING: CALCULATE PAF WITH healthiar::get_pop_fraction() #######
      # to calculate paf per exp band
      # dat <- dat |>
      #   dplyr::select(-c(rr_conc, paf, paf_weighted, prop_pop_exp))
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

    # *  Get impact ##############################################################
    ## Multiply PAFs with bhd (& dw, if applicable)

    # * * Non-lifetable case #####################################################
    if ( !grepl("from_lifetable", res[["detailed"]][["raw"]]$health_metric[1]) ) {

      dat <- dat |>
        dplyr::mutate(impact_total = paf * bhd * dw, .after = paf)
    }

    # Absolute risk ##############################################################

  } else if ( unique(res[["detailed"]][["raw"]]$approach_risk) == "absolute_risk" ) {

    # Vectors needed for simulation below
    exp_central <- res[["detailed"]][["raw"]] |>
      dplyr::filter(exp_ci == "central") |>
      dplyr::pull(exp) |>
      # dplyr::first() |>
      base::unlist(x = _)
    exp_lower <- res[["detailed"]][["raw"]] |>
      dplyr::filter(exp_ci == "lower") |>
      dplyr::pull(exp) |>
      # dplyr::first() |>
      base::unlist(x = _)
    exp_upper <- res[["detailed"]][["raw"]] |>
      dplyr::filter(exp_ci == "upper") |>
      dplyr::pull(exp) |>
      # dplyr::first() |>
      base::unlist(x = _)
    pop_exp <- res[["detailed"]][["raw"]] |>
      dplyr::filter(exp_ci == "central") |>
      dplyr::pull(pop_exp) |>
      base::unlist(x = _)

    ## Simulate values #########################################################
    # Create (empty) tibble to store simulated values & results in
    dat <- tibble::tibble(
      row_id = 1:(n_sim*n_geo),
      geo_id_raw = rep(1:n_geo, each = n_sim)) |>

      # Simulate 1000 exposure values (normal distribution) for each noise band
      # using the corresponding values of exp_lower & exp_upper
      dplyr::bind_cols(
        purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
          res[["detailed"]][["raw"]] |>
            dplyr::filter(exp_ci == "central") |>
            dplyr::pull(exp) |>
            # dplyr::first() |>
            base::unlist(x = _)),
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
                      .fns = ~ healthiar::get_risk(exp = .x, erf_eq = res[["main"]]$erf_eq |> dplyr::first(x = _)) / 100,
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

  # * Non-lifetable ############################################################

  # * * Single geo unit ########################################################

  if ( ( max(dat$geo_id_raw) == 1 ) ) {

    ci <- quantile(x = dat |> dplyr::pull(impact_total) |> base::unlist(),
                   probs = c(0.025, 0.5, 0.975))

    ci <- unname(ci) # Unname to remove percentiles from the names vector
    ci <- tibble(central_estimate = ci[2],
                 lower_estimate = ci[1],
                 upper_estimate = ci[3])

    # * * Multiple geo units ###################################################

  } else if ( max(dat$geo_id_raw) > 1 ) {

    # Impact per geo unit
    impact_per_geo_unit <- dat |>
      group_by(geo_id_raw) |>
      summarize(
        impact_central = quantile(
          x = impact_total,
          probs = c(0.5)
          ),
        impact_lower = quantile(
          x = impact_total,
          probs = c(0.025)
        ),
        impact_upper = quantile(
          x = impact_total,
          probs = c(0.975)
        )
        )

    res[["detailed"]][["uncertainty"]][["raw"]] <- impact_per_geo_unit

    # Impact aggregated
    ci <- impact_per_geo_unit |>
      summarize(
        central_estimate = sum(impact_central),
        lower_estimate = sum(impact_lower),
        upper_estimate = sum(impact_upper)
        )

  }

  # Output #####################################################################
  on.exit(options(user_options))

  res[["detailed"]][["uncertainty"]][["main"]] <- ci

  res[["detailed"]][["uncertainty"]][["dat"]] <- dat # to check interim results during development

  return(res)

}
