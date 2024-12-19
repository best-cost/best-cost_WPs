#' Get Monte Carlo confidence intervals

#' @description Determine summary uncertainty (based on at least one variable
#' with central, lower and upper estimate) based on attribute() or compare()
#' function output.
#' @param results \code{variable} in which the results of an attribute function is stored.
#' @param n_sim \code{numeric value} indicating the number of simulations to be performed.
#' @return
#' This function returns confidence intervals for the attributable health impacts using a Monte Carlo simulation.
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
    results,
    n_sim
    ) {

  ## SCRIPT STRUCTURE
  ## For each variable with a confidence interval a distribution is fitted, and
  ## then n_sim values are simulated based on the distribution.
  ## Then n_sim "new" impacts are calculated using the simulated values.
  ## These steps are seperated for RR and AR pathways (see script outline).
  ## Depending on the calculation pathway specified by the user in the attribute
  ## call (singe exp vs exp dist; iteration yes/no), different code blocks are
  ## triggered.
  ## Lastly, the confidence intervals are determined based on the n_sim "new"
  ## impact vlalues (same code for RR and AR pathways)

  ## Set options
  user_options <- options()
  options(digits = 15) # Make sure that no rounding occurs

  ## Set seed for reproducibility
  set.seed(123)

  ## Determine number of geographic units
  if ( length(grep("geo_id", names(results[["health_detailed"]][["raw"]]))) > 0 ) {

    n_geo <- as.numeric(max(results[["health_detailed"]][["raw"]]$geo_id_raw))

  } else {

      n_geo <- 1

      }

  # Define betaExpert function #################################################
  ## Copied from source code of prevalence::betaExpert() here:
  ### https://github.com/cran/prevalence/blob/master/R/betaExpert.R

  betaExpert <-
    function(best, lower, upper, p = 0.95, method = "mode"){

      ## functions to optimize ~ mode
      f_mode <-
        function(x, mode, p, target){
          return(
            sum(
              (qbeta(p = p,
                     shape1 = x,
                     shape2 = (x * (1 - mode) + 2 * mode - 1) / mode) -
                 target) ^ 2
            ))
        }

      f_mode_zero <-
        function(x, p, target){
          return((qbeta(p = p, shape1 = 1, shape2 = x) - target) ^ 2)
        }

      f_mode_one <-
        function(x, p, target){
          return((qbeta(p = p, shape1 = x, shape2 = 1) - target) ^ 2)
        }

      ## functions to optimize ~ mean
      f_mean <-
        function(x, mean, p, target){
          return(
            sum(
              (qbeta(p = p,
                     shape1 = x,
                     shape2 = (x * (1 - mean)) / mean) -
                 target) ^ 2
            ))
        }

      ## define 'target' and 'p'
      if (!missing(lower) & missing(upper)){
        target <- lower
        p <- 1 - p
      } else if (!missing(upper) & missing(lower)){
        target <- upper
      } else if (!missing(upper) & !missing(lower)){
        target <- c(lower, upper)
        p <- c(0, p) + (1 - p) / 2
      }

      ## derive a and b (=shape1 and shape2)
      if (method == "mode"){
        if (best == 0){
          a <- 1
          b <- optimize(f_mode_zero, c(0, 1000), p = p, target = target)$minimum
        } else if (best == 1) {
          a <- optimize(f_mode_one, c(0, 1000), p = p, target = target)$minimum
          b <- 1
        } else {
          a <- optimize(f_mode, c(0, 1000),
                        mode = best, p = p, target = target)$minimum
          b <- (a * (1 - best) + 2 * best - 1) / best
        }
      } else if (method == "mean"){
        a <- optimize(f_mean, c(0, 1000),
                      mean = best, p = p, target = target)$minimum
        b <- (a * (1 - best)) / best
      }

      ## create 'out' dataframe
      out <- list(alpha = a, beta = b)
      class(out) <- "betaExpert"

      ## return 'out'
      return(out)
    }

  # Relative risk ##############################################################

  if ( unique(results[["health_detailed"]][["raw"]]$approach_risk) == "relative_risk" ) {

    ## Define helper functions for fitting a gamma distribution with optimization
    ## for the relative risk.
    ### NOTE: the functions were adapted from those provided by Sciensano

    ## Set gamma distribution specs
    vector_probabilities <- c(0.025, 0.975)
    par <- 2 ## shape parameter of the gamma distribution

    ## Fit gamma distribution
    f_gamma <-
      function(par, central_estimate, vector_propabilities, lower_estimate, upper_estimate) {
        qfit <- qgamma(p = vector_propabilities, shape = par, rate = par / central_estimate)
        return(sum((qfit - c(lower_estimate, upper_estimate))^2))
      }

    ## Optimize gamma distribution
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

    ## Simulate values based on optimized gamma distribution
    sim_gamma <-
      function(n_sim, central_estimate, lower_estimate, upper_estimate) {
        fit <- optim_gamma(central_estimate, lower_estimate, upper_estimate)
        rgamma(n = n_sim, fit[1], fit[2]) }

    ## Create empty tibble to store simulated values & results in
    dat <-
      tibble::tibble(
        geo_id_raw =
          rep(1:n_geo,
              each = n_sim),
        rr =
          rep(NA, times = n_sim*n_geo),
        erf_increment =
          rep(results[["health_detailed"]][["raw"]] |>
                dplyr::pull(erf_increment) |>
                dplyr::first(),
              times = n_sim*n_geo),
        erf_shape =
          rep(results[["health_detailed"]][["raw"]] |>
                dplyr::pull(erf_shape) |>
                dplyr::first(),
              times = n_sim*n_geo),
        exp =
          rep(NA,
              times = n_sim*n_geo),
        cutoff =
          rep(NA,
              times = n_sim*n_geo),
        bhd =
          rep(NA,
              times = n_sim*n_geo),
        dw =
          rep(NA,
              times = n_sim*n_geo),
        rr_conc =
          rep(NA,
              times = n_sim*n_geo),
        paf =
          rep(NA,
              times = n_sim*n_geo),
        prop_pop_exp =
          rep(NA,
              times = n_sim*n_geo)
      )

    # * Simulate values for input variables ####################################
    ## based on the provided 95% confidence intervals

    # * * rr ###################################################################

    # * * * rr CIs, both single and multiple geo unit case #####################
    if ( length(grep("lower", results[["health_detailed"]][["raw"]][["erf_ci"]])) > 0 )  {

      dat <- dat |>
        # Gamma distribution with optimization to generate simulated RR's
        dplyr::mutate(
          rr = sim_gamma(
            n_sim = n_sim*n_geo,
            central_estimate =
              results[["health_detailed"]][["raw"]] |>
              dplyr::filter(erf_ci == "central") |>
              dplyr::pull(rr) |>
              dplyr::first(),
            lower_estimate =
              results[["health_detailed"]][["raw"]] |>
              dplyr::filter(erf_ci == "lower") |>
              dplyr::pull(rr) |>
              dplyr::first(),
            upper_estimate =
              results[["health_detailed"]][["raw"]] |>
              dplyr::filter(erf_ci == "upper") |>
              dplyr::pull(rr) |>
              dplyr::first()
          )
        )

    # * * * No rr CIs, both single and multiple geo unit case ##################
    } else {

      dat <- dat |>
        dplyr::mutate(
          rr = results[["health_detailed"]][["raw"]] |>
            dplyr::filter(erf_ci == "central") |>
            dplyr::pull(rr) |>
            dplyr::first()
        )
    }

    # * * exp ##################################################################

    # * * * Single exposure ####################################################

    # * * * * exp CIs, single geo unit #########################################
    if (
      ( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "population_weighted_mean") ) &
      ( max(dat$geo_id_raw) == 1 )
      ) {

      ## Determine standard deviation (sd) based on the formula:
      ## (exp_upper - exp_lower) / (2 * 1.96)
      sd_exp <-
      (results[["health_detailed"]][["raw"]] |> dplyr::filter(exp_ci == "upper") |> dplyr::pull(exp) |> dplyr::first() -
          results[["health_detailed"]][["raw"]] |> dplyr::filter(exp_ci == "lower") |> dplyr::pull(exp) |>  dplyr::first()) / (2 * 1.96)

      ## Simulate values
      dat <- dat |>
        dplyr::mutate(
          exp = rnorm(
            n_sim,
            mean = results[["health_detailed"]][["raw"]] |>
                      dplyr::filter(exp_ci == "central") |>
                      dplyr::pull(exp) |>
                      dplyr::first(),
            sd = sd_exp))

    # * * * * No exp CIs, single geo unit ######################################
    } else if (
      !( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
      ( max(dat$geo_id_raw) == 1 )
      ) {

      ## Assign central value to all rows as no CI's present for exp variable
      dat <- dat |>
        dplyr::mutate(exp = results[["health_detailed"]][["raw"]] |>
                        dplyr::filter(exp_ci == "central") |>
                        dplyr::pull(exp) |>
                        dplyr::first())

    # * * * * exp CIs, multiple geo units ######################################
    } else if ( ( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
                ( max(dat$geo_id_raw) > 1 ) ) {

      ## For each geo unit, fit a normal distribution and assign to dat
      ## Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_exp_ci <- results[["health_detailed"]][["raw"]] |>
        dplyr::select(geo_id_raw, exp_ci, exp) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = exp_ci,
          names_prefix = "exp_",
          values_from = exp)

      simulated_data <- dat_with_exp_ci %>%
        rowwise() %>%
        dplyr::mutate(
          ## Generate n_sim simulated values for each row
          exp = list(
            rnorm(
              100,
              mean = exp_central,
              sd = (exp_upper - exp_lower) / (2 * 1.96)
            )
          )) %>%
        dplyr::ungroup() %>%
        ## Expand each row so each simulated value has its own row
        tidyr::unnest(exp) %>%
        ## Keep only relevant columns
        dplyr::select(geo_id_raw, exp)

      ## Add simulated exposure values to dat tibble
      dat <- dat |>
        dplyr::select(-exp) |>
        dplyr::bind_cols(
          simulated_data |> dplyr::select(-geo_id_raw)
          ) |>
        dplyr::relocate(exp, .after = erf_shape)



    # * * * * No exp CIs, multiple geo units ###################################
    } else if (
      !( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) &
      ( max(dat$geo_id_raw) > 1 )
      ) {

      ## Add central exposure value of each geo unit to dat tibble using left_join
      dat <- dat |>
        dplyr::select(-exp) |>
        dplyr::left_join(
          x = _,
          y = results[["health_detailed"]][["raw"]] |>
            dplyr::select(exp, geo_id_raw) |>
            dplyr::distinct(),
          by = "geo_id_raw"
        ) |>
        dplyr::relocate(exp, .after = erf_shape)

    }

    # * * *  Exposure distribution #############################################

    # * * *  * exp CIs, single geo unit #####################
    if (
      ( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
      ( max(dat$geo_id_raw) == 1 )
      ) {

      ## Vectors needed for simulation below
      exp_central <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      exp_lower <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "lower") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      exp_upper <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "upper") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      prop_pop_exp <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::pull(prop_pop_exp) |>
        base::unlist(x = _)

      # Simulate nsim exposure values (normal distribution) for each exp cat
      # using the corresultsponding values of exp_lower & exp_upper
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
            results[["health_detailed"]][["raw"]] |>
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

      # * * * * No exp CIs, single geo unit ####################################
    } else if ( !( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
                ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
                ( max(dat$geo_id_raw) == 1 ) ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      exp_central <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::pull(exp) |>
        dplyr::first() |>
        base::unlist(x = _)
      prop_pop_exp <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::first() |>
        dplyr::pull(prop_pop_exp) |>
        base::unlist(x = _)

      # Create a column for each exposure categories and each prop_pop_exp value
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., (nr. of exposure categories)
            results[["health_detailed"]][["raw"]] |>
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

      # * * * * No exp CIs, multiple geo units #################################
    } else if (
      !( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
      ( max(dat$geo_id_raw) > 1 )
      ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      # Extract exposure values per geo_id_raw and save in a sub-list

      dat <- dat %>%
        select(-exp, -prop_pop_exp) |>
        left_join(results[["health_detailed"]][["raw"]] |>
                    dplyr::filter(erf_ci == "central") |>
                    dplyr::select(geo_id_raw, exp, prop_pop_exp),
                  by = "geo_id_raw") %>%
        tidyr::unnest_wider(c(exp, prop_pop_exp), names_sep = "_")

      # * * * *  Exp CIs, multiple geo units ###################################
    } else if (
      ( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "exposure_distribution") ) &
      ( max(dat$geo_id_raw) > 1 )
      ) {

      # browser()

      ## Generate a tibble with 1000 simulations per exposure category for each geo_id

      ## Vectors needed for simulation of exposure values (exp_central, exp_lower, exp_upper & prop_pop_exp central)
      ### Pull exposures for all geo id's
      exp_central <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::select(geo_id_raw, exp_central = exp)
      exp_lower <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "lower") |>
        dplyr::select(geo_id_raw, exp_lower = exp)
      exp_upper <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "upper") |>
        dplyr::select(geo_id_raw, exp_upper = exp)
      dat_exp <- cbind(exp_central,
                       exp_lower |> select(-geo_id_raw),
                       exp_upper |> select(-geo_id_raw)
                       )
      prop_pop_exp <- results[["health_detailed"]][["raw"]] |>
        dplyr::select(geo_id_raw, prop_pop_exp) |>
        distinct(geo_id_raw, .keep_all = TRUE)

      ## Create vectors of column names
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

      ## Create empty tibble to be filled in loop below
      ### @ AC: sorry for the loop : /
      ###  I tried to avoid it, but failed. To be changed.
      dat_sim <- tibble(
        geo_id_raw = numeric(0)  # Initialize geo_id_raw as numeric
      ) |>
        bind_cols(
          set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
          set_names(rep(list(numeric(0)), length(prop_columns)), prop_columns)
        )

      ## Loop through geo ID's
      for (i in exp_central$geo_id_raw){

        ## Create temp tibble to store simulated values in
        temp <- tibble::tibble(
          geo_id_raw = rep(i, times = n_sim)) |>
          bind_cols(
            purrr::map(
              ## .x will take the values 1, 2, ..., (nr. of exposure categories)
              .x = seq_along(
                dat_exp |>
                  dplyr::filter(geo_id_raw == i) |>
                  dplyr::pull(exp_central) |>
                  base::unlist(x = _)
              ),
              .f = ~ tibble::tibble(
                !!paste0("exp_", .x) :=
                  ## For each exposure category generate n_sim simulated values
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

        ## Add simulated values of current iteration to dat_sim tabble
        dat_sim <- dat_sim |>
          bind_rows(temp)

      }

      # Add simulated values of all geo units to dat tibble
      dat <- dat |>
        bind_cols(dat_sim |> select(-geo_id_raw)) |>
        select(-exp)

    }

    # * * cutoff ###############################################################

    # * * * cutoff CIs, both single and multiple geo unit case ##################
    if ( length(grep("lower", results[["health_detailed"]][["raw"]][["cutoff_ci"]])) > 0 ) {

      ## Determine standard deviation (sd) based on the formula:
      ## (cutoff_upper - cutoff_lower) / (2 * 1.96)
      sd_cutoff <-
        (results[["health_detailed"]][["raw"]] |> dplyr::filter(cutoff_ci == "upper") |> dplyr::pull(cutoff) |> dplyr::first() -
           results[["health_detailed"]][["raw"]] |> dplyr::filter(cutoff_ci == "lower") |> dplyr::pull(cutoff) |>  dplyr::first()) / (2 * 1.96)

      dat <- dat |>
        dplyr::mutate(
          cutoff = rnorm(
            n_sim * n_geo,
            mean = results[["health_detailed"]][["raw"]] |>
              dplyr::filter(cutoff_ci == "central") |>
              dplyr::pull(cutoff) |>
              dplyr::first(),
            sd = sd_cutoff))

    # * * * No cutoff CIs, both single and multiple geo unit case ##############
    } else if ( !length(grep("lower", results[["health_detailed"]][["raw"]][["cutoff_ci"]])) > 0 ) {

      dat <- dat |>
        dplyr::mutate(cutoff = results[["health_detailed"]][["raw"]] |>
                        dplyr::filter(cutoff_ci == "central") |>
                        dplyr::pull(cutoff) |>
                        dplyr::first())
    }

    # * * bhd ##################################################################

    # * * * bhd CIs & single geo unit ##########################################
    if ( (length(grep("lower", results[["health_detailed"]][["raw"]][["bhd_ci"]])) > 0) &
      ( max(dat$geo_id_raw) == 1 ) ) {

      ## Determine standard deviation (sd) based on the formula:
      ## (bhd_upper - bhd_lower) / (2 * 1.96)
      sd_bhd <- #(bhd_upper - bhd_lower) / (2 * 1.96)
        (results[["health_detailed"]][["raw"]] |> dplyr::filter(bhd_ci == "upper") |> dplyr::pull(bhd) |> dplyr::first() -
           results[["health_detailed"]][["raw"]] |> dplyr::filter(bhd_ci == "lower") |> dplyr::pull(bhd) |>  dplyr::first()) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          bhd = rnorm(
            n_sim,
            mean = results[["health_detailed"]][["raw"]] |>
              dplyr::filter(bhd_ci == "central") |>
              dplyr::pull(bhd) |>
              dplyr::first(),
            sd = sd_bhd))

    # * * * No bhd CIs & single geo unit ##########################################
    } else if ( !(length(grep("lower", results[["health_detailed"]][["raw"]][["bhd_ci"]])) > 0) &
                ( max(dat$geo_id_raw) == 1 ) ) {

      dat <- dat |>
        dplyr::mutate(bhd = results[["health_detailed"]][["raw"]] |>
                        dplyr::filter(bhd_ci == "central") |>
                        dplyr::pull(bhd) |>
                        dplyr::first())

    # * * * bhd CIs & multiple geo units ##########################################
    } else if (
      (length(grep("lower", results[["health_detailed"]][["raw"]][["bhd_ci"]])) > 0) &
      ( max(dat$geo_id_raw) > 1 )
      ) {

      ## For each geo unit, fit a normal distribution and assign to dat
      ## Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_bhd_ci <- results[["health_detailed"]][["raw"]] |>
        dplyr::select(geo_id_raw, bhd_ci, bhd) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = bhd_ci,
          names_prefix = "bhd_",
          values_from = bhd)

      simulated_data <- dat_with_bhd_ci %>%
        rowwise() %>%
        dplyr::mutate(
          ## Generate n_sim simulated values for each row
          bhd = list(
            rnorm(
              100,
              mean = bhd_central,
              sd = (bhd_upper - bhd_lower) / (2 * 1.96)
            )
          )) %>%
        dplyr::ungroup() %>%
        ## Expand each row so each simulated value has its own row
        tidyr::unnest(bhd) %>%
        ## Keep only relevant columns
        dplyr::select(geo_id_raw, bhd)

      ## Add simulated exposure values to dat tibble
      dat <- dat |>
        dplyr::select(-bhd) |>
        dplyr::bind_cols(
          simulated_data |> dplyr::select(-geo_id_raw)
        ) |>
        dplyr::relocate(bhd, .after = cutoff)

    # * * * No bhd CI's & multiple geo units ######################################
    } else if (
      !(length(grep("lower", results[["health_detailed"]][["raw"]][["bhd_ci"]])) > 0) &
      ( max(dat$geo_id_raw) > 1 )
      ) {

      ## Add central exposure value of each geo unit to dat tibble using left_join
      dat <- dat |>
        dplyr::select(-bhd) |>
        dplyr::left_join(
          x = _,
          y = results[["health_detailed"]][["raw"]] |>
            dplyr::select(bhd, geo_id_raw) |>
            dplyr::distinct(),
          by = "geo_id_raw"
        ) |>
        dplyr::relocate(bhd, .after = cutoff)

    }

    # * * dw ###################################################################

    # * * * dw CIs, both single and multiple geo unit case #####################
    if ( (length(grep("lower", results[["health_detailed"]][["raw"]][["dw_ci"]])) > 0) &
         ( max(dat$geo_id_raw) == 1 ) ) {

      ## beta distribution using prevalence::betaExpert()
      ### Determine the alpha and beta parameters needed to fit beta distribution using the (source code of the) prevalence::betaExpert() function
      dw_sim <- betaExpert(
        ## dw_central
        results[["health_detailed"]][["raw"]] |>
          dplyr::filter(dw_ci == "central") |>
          dplyr::pull(dw) |>
          dplyr::first(),
        ## dw_lower
        results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first(),
        ##  dw_upper,
        results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first(),
        method = "mean")

      ### Simulate nsim disability weight values
      dat <- dat |>
        dplyr::mutate(
          dw = rbeta(
            n = n_sim,
            shape1 = as.numeric(unname(dw_sim["alpha"])),
            shape2 = as.numeric(unname(dw_sim["beta"]))))

      ## ALTERNATIVE: Using normal distribution
      ## Define standard deviation = (dw_upper - dw_lower) / (2 * 1.96)
      # sd_dw <-
      #   (results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first() -
      #      results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first()) / (2 * 1.96)
      # dat <- dat |>
      #   dplyr::mutate(
      #     dw = rnorm(
      #       n_sim,
      #       mean = results[["health_detailed"]][["raw"]] |>
      #         dplyr::filter(dw_ci == "central") |>
      #         dplyr::pull(dw) |>
      #         dplyr::first(),
      #       sd = sd_dw))

      ## ALTERNATIVE: Using beta distribution using qbeta()
      # dat <- dat |>
      #   dplyr::mutate(dw = sim_beta(n_sim = n_sim,
      #                               dw_central = dw_central,
      #                               vector_dw_ci = vector_dw_ci))

    # * * * No dw CIs, both single and multiple geo unit case ##################
    } else if ( ("dw" %in% names(results[["health_detailed"]][["raw"]])) &
                ( max(dat$geo_id_raw) == 1 ) ) {
      dat <- dat |>
        dplyr::mutate(dw = results[["health_detailed"]][["raw"]] |>
                        dplyr::filter(dw_ci == "central") |>
                        dplyr::pull(dw) |>
                        dplyr::first())

    # * * * No dw inputted, both single and multiple geo unit case #############
    } else if ( !( "dw" %in% names(results[["health_detailed"]][["raw"]]) ) ) {

      dat <- dat |>
        dplyr::mutate(dw = 1)

    }

    # * rr_conc ################################################################

    # * * Single exposure case #################################################
    if ( ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) ) {

    ## Calculate rr_conc using healthiar::get_risk
    dat <- dat |>
      dplyr::mutate(
        rr_conc = purrr::pmap(
          list(rr = rr, exp = exp, cutoff = cutoff, erf_increment = erf_increment, erf_shape = erf_shape),
          function(rr, exp, cutoff, erf_increment, erf_shape){
            rr_conc <- healthiar::get_risk(
              rr = rr,
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

    # * * Exposure distribution case ###########################################
    } else if ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "exposure_distribution" ) ) {

      # Calc rr_conc for each exp cat
      dat <- dat |>
        dplyr::bind_cols(
          ## .x will take the values 1, 2, ..., until (nr. of exposure categories)
          purrr::map(.x = seq_along(
            results[["health_detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              dplyr::pull(exp) |>
              dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("rr_conc_", .x) :=
                get_risk(rr = dat$rr,
                         exp = dat[[!!paste0("exp_", .x)]], # Selects xth element of the vector
                         cutoff = dat$cutoff[1],
                         erf_increment = dat$erf_increment[1],
                         erf_shape = dat$erf_shape[1],
                         erf_eq = NULL)
              )
            )
          )
    }

    # * PAF ####################################################################


    # * * Single exposure case #################################################

    ## Determine PAF with healthiar::get_pop_fraction()
    if ( ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "population_weighted_mean" ) ) ) {

      dat <- dat |>
        dplyr::mutate(
          paf = purrr::pmap(
            list(rr_conc = rr_conc, prop_pop_exp = results[["health_detailed"]][["raw"]]$prop_pop_exp |> dplyr::first(x = _)),
            function(rr_conc, prop_pop_exp){
              paf <- healthiar::get_pop_fraction(
                rr_conc_1 = rr_conc,
                rr_conc_2 = 1,
                prop_pop_exp_1 = prop_pop_exp,
                prop_pop_exp_2 = prop_pop_exp)
              return(paf)
            }
          )
        )

      dat$paf <- base::unlist(dat$paf)

    # * * Exposure distribution case ###########################################

    } else if ( unique(results[["health_detailed"]][["raw"]]$exposure_type == "exposure_distribution" ) ) {

      ## Determine product_x = rr_conc_x * prop_pop_exp_x
      ## This is an intermediate step towards PAF calculation
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(                      # .x will take the values 1, 2, ..., until (nr. of exposure categories)
            results[["health_detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              dplyr::pull(exp) |>
              dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("product_", .x) := dat[[!!paste0("rr_conc_", .x)]] * dat[[!!paste0("prop_pop_exp_", .x)]]
            )
          )
        )

      ## WORKING: CALCULATE PAF FOLLOWING EXCEL EXAMPLE FROM NIPH
      # Excel located here: ..\best-cost\r_package\testing\input\noise_niph
      # NOTE 2024-11-26: PAF matches the result in the Excel sheet "Relative_risk_IHD_WHO_2003a" exactly
      dat <- dat |>
        dplyr::mutate(sum_product = rowSums(across(contains("product_")))) |>
        dplyr::mutate(paf = ( sum_product - 1 ) / sum_product)


      ## NOT WORKING: CALCULATE PAF WITH healthiar::get_pop_fraction()
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

    # *  Get impact ############################################################
    ## Multiply PAFs with bhd

      dat <- dat |>
        dplyr::mutate(impact_total = paf * bhd * dw, .after = paf)
    ## If there is no dw used in the preceding attribute call, dw is set to 1 earlier in script

    # Absolute risk ############################################################

    # * Simulate values for input variables ####################################
  } else if ( unique(results[["health_detailed"]][["raw"]]$approach_risk) == "absolute_risk" ) {

    ## Create (empty) tibble to store simulated values & results in
    dat <- tibble::tibble(
      row_id = 1:(n_sim*n_geo),
      geo_id_raw = rep(1:n_geo, each = n_sim))

    # * * exp ##################################################################

    # * * * exp CI's & single geo unit #########################################

    if (
      (length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0) &
      ( max(dat$geo_id_raw) == 1 )
    ) {

      # browser()

      ## Vectors needed for simulation below
      exp_central <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      exp_lower <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "lower") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      exp_upper <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "upper") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      pop_exp <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(pop_exp) |>
        base::unlist(x = _)

      ## Simulate 1000 exposure values (normal distribution) for each noise band
      ## using the corresponding values of exp_lower & exp_upper
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(
            # .x will take the values 1, 2, ..., (nr. of exposure categories)
            results[["health_detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
              (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
              dplyr::filter(erf_ci == "central") |>
              dplyr::pull(exp) |>
              # dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := # Refers to the xth element of the vector
                rnorm(n_sim,
                      mean = exp_central[.x],
                      sd = (exp_upper[.x] - exp_lower[.x]) / (2 * 1.96)),
              !!paste0("pop_exp_", .x) := pop_exp[.x])) |>
            purrr::reduce(bind_cols))


    # * * * exp CI's & multiple geo units ######################################
    } else if (
      ( length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( max(dat$geo_id_raw) > 1 )
      ) {

      ## Create exp vectors needed for simulation below
      exp_central <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        select(geo_id_raw, exposure_dimension, exp) |>
        group_by(geo_id_raw) %>%
        summarize(exp_central = list(exp), .groups = "drop")
      exp_lower <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "lower") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        select(geo_id_raw, exposure_dimension, exp) |>
        group_by(geo_id_raw) %>%
        summarize(exp_lower = list(exp), .groups = "drop")
      exp_upper <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "upper") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        select(geo_id_raw, exposure_dimension, exp) |>
        group_by(geo_id_raw) %>%
        summarize(exp_upper = list(exp), .groups = "drop")
      ## Bind vectors
      dat_exp <- cbind(exp_central,
                       exp_lower |> select(-geo_id_raw),
                       exp_upper |> select(-geo_id_raw))
      ## Create vector with populations exposed
      pop_exp <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        select(geo_id_raw, pop_exp) |>
        group_by(geo_id_raw) %>%
        summarize(pop_exp = list(pop_exp), .groups = "drop")

      ## Create vectors of column names
      exp_columns <- paste0("exp_", dat_exp |>
                              filter(geo_id_raw == 1) |>
                              pull(exp_central) |>
                              unlist() |>
                              seq_along())
      pop_columns <- paste0("pop_exp_", dat_exp |>
                               filter(geo_id_raw == 1) |>
                               pull(exp_central) |>
                               unlist() |>
                               seq_along())

      ## Create empty tibble to be filled in loop below
      ## @ AC: sorry for the loop (again) : P
      dat_sim <- tibble(
        geo_id_raw = numeric(0)  # Initialize geo_id_raw as numeric
      ) |>
        bind_cols(
          set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
          set_names(rep(list(numeric(0)), length(pop_columns)), pop_columns)
        )

      for (i in exp_central$geo_id_raw){

        ## Create temp tibble to store simulated values in
        temp <- tibble::tibble(
          geo_id_raw = rep(i, times = n_sim)) |>
          bind_cols(
            purrr::map(
              # .x will take the values 1, 2, ..., (nr. of exposure categories)
              .x = seq_along(
                dat_exp |>
                  dplyr::filter(geo_id_raw == i) |>
                  dplyr::pull(exp_central) |>
                  base::unlist(x = _)
              ),
              .f = ~ tibble::tibble(
                !!paste0("exp_", .x) :=
                  ## For each exposure category generate n_sim simulated values
                  rnorm(
                    n_sim,
                    mean = dat_exp |> filter(geo_id_raw == i) |> pull(exp_central) |> unlist(x = _) |> nth(.x) ,
                    # Formula for standard deviation (sd): exp_upper - exp_lower) / (2 * 1.96)
                    sd = ( dat_exp |> filter(geo_id_raw == i) |> pull(exp_upper) |> unlist(x = _) |> nth(.x) -
                             dat_exp |> filter(geo_id_raw == i) |> pull(exp_lower) |> unlist(x = _) |> nth(.x) ) / (2 * 1.96)
                  ),
                !!paste0("pop_exp_", .x) := pop_exp|> filter(geo_id_raw == i) |> pull(pop_exp) |> unlist(x = _) |> nth(.x)
              )) |>
              purrr::reduce(bind_cols)
          )

        ## Add simulated values of current iteration to dat_sim tabble
        dat_sim <- dat_sim |>
          bind_rows(temp)

      }

      # Add simulated values of all geo units to dat tibble
      dat <- dat |>
        bind_cols(dat_sim |> select(-geo_id_raw))

      # * * * no exp CI's & single geo unit case ###############################
    } else if (
      ( !length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
      ( max(dat$geo_id_raw) == 1  )
    ) {


      # browser()

      ## Create exp and prop_exp vectors
      exp_central <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      pop_exp <- results[["health_detailed"]][["raw"]] |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(pop_exp) |>
        base::unlist(x = _)

      ## Add exp and prop_exp to the dat dataframe
      ### Values taken from the exp_central and pop_exp vectors created above
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(
            # .x will take the values 1, 2, ..., (nr. of exposure categories)
            results[["health_detailed"]][["raw"]] |>
              dplyr::filter(exp_ci == "central") |>
              (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
              (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
              dplyr::filter(erf_ci == "central") |>
              dplyr::pull(exp) |>
              # dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central[.x],
              !!paste0("pop_exp_", .x) := pop_exp[.x])) |>
            purrr::reduce(bind_cols))

      # * * * no exp CI's & multiple geo unit case #############################
    }  else if (
    ( !length(grep("lower", results[["health_detailed"]][["raw"]][["exp_ci"]])) > 0 ) &
    ( max(dat$geo_id_raw) > 1 )
    ) {

    ## Create exp and prop_exp vectors
    exp_central <- results[["health_detailed"]][["raw"]] |>
      dplyr::filter(exp_ci == "central") |>
      (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
      (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
      dplyr::filter(erf_ci == "central") |>
      select(geo_id_raw, exposure_dimension, exp) |>
      group_by(geo_id_raw) %>%
      summarize(exp_central = list(exp), .groups = "drop")

    pop_exp <- results[["health_detailed"]][["raw"]] |>
      dplyr::filter(exp_ci == "central") |>
      (\(x) if ("duration_ci" %in% colnames(x)) filter(x, duration_ci == "central") else x)() |>
      (\(x) if ("dw_ci" %in% colnames(x)) filter(x, dw_ci == "central") else x)() |>
      dplyr::filter(erf_ci == "central") |>
      select(geo_id_raw, pop_exp) |>
      group_by(geo_id_raw) %>%
      summarize(pop_exp = list(pop_exp), .groups = "drop")

    ## Create vectors of column names
    exp_columns <- paste0("exp_", exp_central |>
                            filter(geo_id_raw == 1) |>
                            pull(exp_central) |>
                            unlist() |>
                            seq_along())
    pop_columns <- paste0("pop_exp_", exp_central |>
                            filter(geo_id_raw == 1) |>
                            pull(exp_central) |>
                            unlist() |>
                            seq_along())

    ## Create empty tibble to be filled in loop below
    ## @ AC: sorry for the loop (again) : P
    dat_sim <- tibble(
      geo_id_raw = numeric(0)  # Initialize geo_id_raw as numeric
    ) |>
      bind_cols(
        set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
        set_names(rep(list(numeric(0)), length(pop_columns)), pop_columns)
      )

    for (i in exp_central$geo_id_raw){

      ## Create temp tibble to store simulated values in
      temp <- tibble::tibble(
        geo_id_raw = rep(i, times = n_sim)) |>
        bind_cols(
          purrr::map(
            # .x will take the values 1, 2, ..., (nr. of exposure categories)
            .x = seq_along(
              exp_central |>
                dplyr::filter(geo_id_raw == i) |>
                dplyr::pull(exp_central) |>
                base::unlist(x = _)
            ),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central |> filter(geo_id_raw == i) |> pull(exp_central) |> unlist(x = _) |> nth(.x),
              !!paste0("pop_exp_", .x) := pop_exp |> filter(geo_id_raw == i) |> pull(pop_exp) |> unlist(x = _) |> nth(.x)
            )) |>
            purrr::reduce(bind_cols)
        )

      ## Add simulated values of current iteration to dat_sim tabble
      dat_sim <- dat_sim |>
        bind_rows(temp)

    }

    # browser()

    # Add simulated values of all geo units to dat tibble
    dat <- dat |>
      bind_cols(dat_sim |> select(-geo_id_raw))

    }

    # * * dw ###################################################################

    # browser()

    # * * * dw CIs, both single and multiple geo unit case #####################
    if ( (length(grep("lower", results[["health_detailed"]][["raw"]][["dw_ci"]])) > 0) &
         ( max(dat$geo_id_raw) == 1 ) ) {

      ## beta distribution using prevalence::betaExpert()
      ### Determine the alpha and beta parameters needed to fit beta distribution using the (source code of the) prevalence::betaExpert() function
      dw_sim <- betaExpert(
        ## dw_central
        results[["health_detailed"]][["raw"]] |>
          dplyr::filter(dw_ci == "central") |>
          dplyr::pull(dw) |>
          dplyr::first(),
        ## dw_lower
        results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first(),
        ##  dw_upper,
        results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first(),
        method = "mean")

      ### Simulate nsim disability weight values
      dat <- dat |>
        dplyr::mutate(
          dw = rbeta(
            n = n_sim,
            shape1 = as.numeric(unname(dw_sim["alpha"])),
            shape2 = as.numeric(unname(dw_sim["beta"]))))

      ## ALTERNATIVE: Using normal distribution
      ## Define standard deviation = (dw_upper - dw_lower) / (2 * 1.96)
      # sd_dw <-
      #   (results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first() -
      #      results[["health_detailed"]][["raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first()) / (2 * 1.96)
      # dat <- dat |>
      #   dplyr::mutate(
      #     dw = rnorm(
      #       n_sim,
      #       mean = results[["health_detailed"]][["raw"]] |>
      #         dplyr::filter(dw_ci == "central") |>
      #         dplyr::pull(dw) |>
      #         dplyr::first(),
      #       sd = sd_dw))

      ## ALTERNATIVE: Using beta distribution using qbeta()
      # dat <- dat |>
      #   dplyr::mutate(dw = sim_beta(n_sim = n_sim,
      #                               dw_central = dw_central,
      #                               vector_dw_ci = vector_dw_ci))

      # * * * No dw CIs, both single and multiple geo unit case ################
    } else if ( ("dw" %in% names(results[["health_detailed"]][["raw"]])) ) {
      dat <- dat |>
        dplyr::mutate(dw = results[["health_detailed"]][["raw"]] |>
                        dplyr::filter(dw_ci == "central") |>
                        dplyr::pull(dw) |>
                        dplyr::first())

      # * * * No dw inputted, both single and multiple geo unit case ###########
    } else if ( !( "dw" %in% names(results[["health_detailed"]][["raw"]]) ) ) {

      dat <- dat |>
        dplyr::mutate(dw = 1)

    }

    # * * erf_eq #################################################################

    # * * * No erf_eq CI's, both single and multiple geo unit case ###############
    if ( length(grep("lower", results[["health_detailed"]][["raw"]][["erf_ci"]])) == 0 ) {

    ## Calculate risk for each noise band
    dat <- dat |>
      ## NOTE: not using rowwise & ungroup because results not altered but running time much longer
      # dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(.cols = dplyr::starts_with("exp_"),
                      .fns = ~ healthiar::get_risk(exp = .x, erf_eq = results[["health_detailed"]][["raw"]]$erf_eq |> dplyr::first(x = _)) / 100,
                      .names = "risk_{str_remove(.col, 'exp_')}")
      ) # |>
      # dplyr::ungroup()

    # * * * erf_eq CI's & multiple geo unit case ###############################
    } else if ( length(grep("lower", results[["health_detailed"]][["raw"]][["erf_ci"]])) > 0 ){

      ## For each exp category, create 3 risk (ri) columns: e.g. ri_1_central, ri_1_lower, ri_1_upper & add to dat
      ### For the columns ri_..._central use the erf_eq_central, for ri_..._lower use the erf_eq_lower, ...

      ## Calculate risk estimates for each exposure band
      dat <- dat |>
        ## NOTE: not using rowwise & ungroup because results not altered but running time much longer

        ### Central risk estimates
        # dplyr::rowwise() |>
        dplyr::mutate(
          dplyr::across(.cols = dplyr::starts_with("exp_"),
                        .fns = ~ healthiar::get_risk(exp = .x, erf_eq = results[["health_detailed"]][["raw"]] |> filter(erf_ci == "central") |> pull(erf_eq) |> first()) / 100,
                        .names = "ri_central_{str_remove(.col, 'exp_')}")
        ) |>
        ### Lower risk estimates
        dplyr::mutate(
          dplyr::across(.cols = dplyr::starts_with("exp_"),
                        .fns = ~ healthiar::get_risk(exp = .x, erf_eq = results[["health_detailed"]][["raw"]] |> filter(erf_ci == "lower") |> pull(erf_eq) |> first()) / 100,
                        .names = "ri_lower_{str_remove(.col, 'exp_')}")
        ) |>
        ### Upper risk estimates
        dplyr::mutate(
          dplyr::across(.cols = dplyr::starts_with("exp_"),
                        .fns = ~ healthiar::get_risk(exp = .x, erf_eq = results[["health_detailed"]][["raw"]] |> filter(erf_ci == "upper") |> pull(erf_eq) |> first()) / 100,
                        .names = "ri_upper_{str_remove(.col, 'exp_')}")
        ) # |>
        # dplyr::ungroup()

      ## For each noise band for each row fit a normal distribution using the risk_..._... columns and simulate 1 value (for that specific row)
      ## Corresponding ri_central_..., ri_lower_... and ri_upper_... columns are used
      dat <- dat |>
        rowwise() |>
        mutate(
          dplyr::across(
            .cols = dplyr::starts_with("ri_central_"),
            .fns = ~ rnorm(
            1, # Only 1 simulation
            mean = .x,
            sd = ( dat[[gsub("ri_central_", "ri_upper_", dplyr::cur_column())]] -
                     dat[[gsub("ri_central_", "ri_lower_", dplyr::cur_column())]] ) / (2 * 1.96) # Formula: exp_upper - exp_lower) / (2 * 1.96)
          ),
          .names = "risk_{str_remove(.col, 'ri_central_')}"
        )
        ) |>
        ungroup()
    }



    # * Get impact AR pathway ##################################################

    ## Calculate impact per noise band
    ### impact_X = risk_X * pop_X * dw
    dat <- dat |>
      dplyr::mutate(
        ## Iterate over corresponding "risk_" and "pop_" columns
        ### Also multiply with the disability weight
        dplyr::across(dplyr::starts_with("risk_"), ~ as.numeric(.x) * as.numeric(dat[[gsub("risk_", "pop_exp_", dplyr::cur_column())]]) * as.numeric(dw),
                      .names = "impact_{str_remove(.col, 'risk_')}")) |>
      # Sum impacts across noise bands to obtain total impact
      dplyr::mutate(impact_total = rowSums(across(starts_with("impact_"))))

  }

  # Determine 95% CI of impact #################################################

  # * Single geo unit ##########################################################

  if ( ( max(dat$geo_id_raw) == 1 ) ) {

    ## CI of aggregated impact
    ### Because there's only 1 geo unit the aggregated impact is the same as the geo unit impact
    ci <- quantile(x = dat |> dplyr::pull(impact_total) |> base::unlist(),
                   probs = c(0.025, 0.5, 0.975))

    ci <- unname(ci) # Unname to remove percentiles from the names vector
    ci <- tibble(central_estimate = ci[2],
                 lower_estimate = ci[1],
                 upper_estimate = ci[3])

  # * Multiple geo units ###################################################
  } else if ( max(dat$geo_id_raw) > 1 ) {

    # browser()

    ## CIs of impact per geo unit
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

    results[["uncertainty_detailed"]][["geo_specific"]] <- impact_per_geo_unit

    ## CIs of impact aggregated over geo units
    ci <- impact_per_geo_unit |>
      summarize(
        central_estimate = sum(impact_central),
        lower_estimate = sum(impact_lower),
        upper_estimate = sum(impact_upper)
        )

  }

  # Output #####################################################################
  on.exit(options(user_options))

  results[["uncertainty_main"]] <- ci

  results[["uncertainty_detailed"]][["raw"]] <- dat # to check interim results during development

  return(results)

}
