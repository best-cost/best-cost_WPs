# Title and description

#' Compile input

#' Compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)
#' @inheritParams attribute
#'
#' @return
#' This function returns a \code{data.frame} with all input data together
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export
#' @keywords internal

compile_input <-
  function(approach_risk = NULL,
           health_metric = NULL,
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = NULL,
           pop_exp = NULL,
           cutoff = NULL,
           rr_central, rr_lower = NULL, rr_upper = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_c_central = NULL, erf_c_lower = NULL, erf_c_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           min_age = NULL,
           max_age = NULL,
           geo_id_raw = NULL,
           geo_id_aggregated = NULL,
           info = NULL,
           disability_weight_central = NULL,
           corrected_discount_rate = NULL,
           duration = NULL,
           # And lifetable-related data...
           approach_exposure = NULL,
           approach_newborns = NULL,
           first_age_pop = NULL, last_age_pop = NULL,
           prob_natural_death_male = NULL, prob_natural_death_female = NULL,
           prob_total_death_male = NULL, prob_total_death_female = NULL,
           population_midyear_male = NULL, population_midyear_female = NULL,
           # For AirQ+ approach for lifetables
           deaths_male = NULL, deaths_female = NULL){

    # Check input data
    # stopifnot(exprs = {
      #length(exp) == length(prop_pop_exp)
      #is.null(min_age) == FALSE
      #is.null(max_age) == FALSE
    # })

    # If no geo_id_raw is provided (if is NULL) then assign some value.
    # geo_id_raw is needed to group results in case of multiple geo_ids

    if(is.null(geo_id_raw)){
      geo_id_raw <-
        as.character(ifelse(is.list({{exp_central}}), 1:length({{exp_central}}), 1))
    }




    # Input data in data frame

    # If the erf is defined by rr, increment, shape and cutoff

    if(is.null(erf_c_central)){
      # Input data in data frame
      # Compile rr data to assign categories
      erf_data <-
        # tibble instead of data.frame because tibble converts NULL into NA
        dplyr::tibble(
          erf_increment = erf_increment,
          erf_shape = erf_shape,
          cutoff = cutoff,
          rr_central = rr_central,
          rr_lower =  rr_lower,
          rr_upper = rr_upper)

    } else { # If it is defined by the erf function
      erf_data <-
        # tibble instead of data.frame because tibble converts NULL into NA
        dplyr::tibble(
          erf_c_central = erf_c_central,
          erf_c_lower = erf_c_lower,
          erf_c_upper = erf_c_upper)

    }

    # Store the length of the exposure distribution (to be used below)
    # Let's take the first element
    length_exp_dist <-
      ifelse(is.list(exp_central),
             length(exp_central[[1]]),
             length(exp_central))

    length_exp_list <-
      ifelse(is.list(exp_central),
             length(exp_central),
             1)


    input_wo_lifetable <-
      # Build a tibble instead  a data.frame because tibble converts NULL into NA
      dplyr::tibble(
        # First compile input data that are only geo-dependent,
        # ie. those which require adjustment to have the same dimension
        # as those with multiple dimension because of exposure distribution
        # Let's use rep() to ensure that there is dimension match
        geo_id_raw = rep(geo_id_raw, each = length_exp_dist),
        geo_id_aggregated = rep(geo_id_aggregated, each = length_exp_dist),
        bhd_central = rep(unlist(bhd_central), each = length_exp_dist),
        bhd_lower = rep(unlist(bhd_lower), each = length_exp_dist),
        bhd_upper = rep(unlist(bhd_lower), each = length_exp_dist),
        min_age = rep(min_age, each = length_exp_dist),
        max_age = rep(max_age, each = length_exp_dist),
        approach_exposure = rep(approach_exposure, each = length_exp_dist),
        approach_newborns = rep(approach_newborns, each = length_exp_dist),

        # Second those variables that will have length = 1 (no problematic)
        disability_weight_central = disability_weight_central,
        duration = duration,
        corrected_discount_rate = corrected_discount_rate,

        # Finally, those variables that are multi-dimensional (exposure distribution)
        exp_central = unlist(exp_central),
        exp_lower = unlist(exp_lower),
        exp_upper = unlist(exp_upper),
        prop_pop_exp = unlist(prop_pop_exp),
        pop_exp = unlist(pop_exp)) %>%

      # Add rr with a cross join to produce all likely combinations
      dplyr::bind_cols(., erf_data) %>%
      # Add additional (meta-)information
      bestcost:::add_info(df = ., info = info) %>%
      # Information derived from input data
      dplyr::mutate(
        # Add age_max and age_min (not needed without life table)
        age_range = ifelse(!is.null(max_age) & is.null(min_age), paste0("below", max_age + 1),
                           ifelse(!is.null(min_age) & is.null(max_age), paste0("from", min_age),
                                  NA)),
        # Add the approach_risk that refer to the function
        approach_risk = approach_risk,
        health_metric = health_metric,
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        # Using {{}} to call the argument instead of the column (same name)
        exposure_dimension =
          rep(1:length_exp_dist, length_exp_list),
        exposure_type =
          ifelse(length_exp_dist == 1,
                 "population_weighted_mean",
                 "exposure_distribution")) %>%
      # Remove all columns with all values being NA
      dplyr::select(where(~ !all(is.na(.)))) %>%
      # Add lifetable-related data as nested tibble
      # Build the data set
      # The life table has to be provided (by sex)
      # Rename column names to standard names

      # Pivot longer to show all combinations of central, lower and upper estimate
      # (relevant for iteration)
      ## For exposure,
      tidyr::pivot_longer(.,
                          cols = starts_with("exp_"),
                          names_to = "exp_ci",
                          names_prefix = "exp_",
                          values_to = "exp") %>%
      ## Exposure response function &
      {if(is.null(erf_c_central))
        tidyr::pivot_longer(.,
                            cols = starts_with("rr_"),
                            names_to = "erf_ci",
                            names_prefix = "rr_",
                            values_to = "rr")
        else
          tidyr::pivot_longer(.,
                              cols = starts_with("erf_c_"),
                              names_to = "erf_ci",
                              names_prefix = "erf_c_",
                              values_to = "erf_c")}%>%
      ## Baseline health data
      {if(!is.null(bhd_central))
        tidyr::pivot_longer(.,
                            cols = starts_with("bhd_"),
                            names_to = "bhd_ci",
                            names_prefix = "bhd_",
                            values_to = "bhd") else .}


    if(grepl("lifetable", health_metric)){

      # Build the data set for lifetable-related data
      # The life table has to be provided (by sex)
      # Rename column names to standard names

      # Define variables to be used below
      age_sequence <- seq(from = first_age_pop,
                          to = last_age_pop,
                          by = 1)
      age_end_sequence <- seq(from = first_age_pop + 1,
                              to = last_age_pop + 1,
                              by = 1)



      # Create a template of the lifetables
      # Use crossing to enable all combination of the vectors
      lifetable_with_pop_template <-
        tidyr::crossing(
          geo_id_raw,
          age = age_sequence) %>%
        # Add age end with mutate instead of crossing
        # because no additional combinations are needed (already included in age)
        # The function rep(, length.out=)
        # is needed to ensure that the vector length matches with number of rows of the tibble.
        dplyr::mutate(
          .,
          age_end = rep(age_end_sequence, length.out = n()))

      # Based on the template create lifetable for male
      lifetable_with_pop_male <-
        lifetable_with_pop_template %>%
        dplyr::mutate(
          sex = "male",
          deaths = rep(unlist(deaths_male), length.out = n()),
          prob_natural_death = rep(unlist(prob_natural_death_male), length.out = n()),
          prob_total_death = rep(unlist(prob_total_death_male), length.out = n()),
          population = rep(unlist(population_midyear_male), length.out = n()))

      # The same for female
      lifetable_with_pop_female <-
        lifetable_with_pop_template %>%
        dplyr::mutate(
          sex = "female",
          deaths = rep(unlist(deaths_female), length.out = n()),
          prob_natural_death = rep(unlist(prob_natural_death_female), length.out = n()),
          prob_total_death = rep(unlist(prob_total_death_female), length.out = n()),
          population = rep(unlist(population_midyear_female), length.out = n()))

      lifetable_with_pop_male_female <-
        # Bind male and female tibbles
        dplyr::bind_rows(
          lifetable_with_pop_male,
          lifetable_with_pop_female)

      lifetable_with_pop <-
        lifetable_with_pop_male_female %>%
        # Nest the lifetable elements
        tidyr::nest(
          lifetable_with_pop_nest =
            c(age, age_end,
              prob_natural_death, prob_total_death,
              population))


      # if(grepl("airqplus", health_metric)){
        # lifetable_with_pop_male <-
        #   lifetable_with_pop_male %>%
        #   dplyr::mutate(deaths = rep(unlist(deaths_male), length.out = n()))
        #
        # lifetable_with_pop_female <-
        #   lifetable_with_pop_female %>%
        #   dplyr::mutate(deaths = rep(unlist(deaths_female), length.out = n()))

        # The same for total
        lifetable_with_pop_total <-
          lifetable_with_pop_template %>%
          dplyr::mutate(
            sex = "total",
            population = lifetable_with_pop_male$population + lifetable_with_pop_female$population,
            deaths = lifetable_with_pop_male$deaths + lifetable_with_pop_female$deaths)

        lifetable_with_pop_male_female_total <-
          dplyr::bind_rows(lifetable_with_pop_male,
                           lifetable_with_pop_female,
                           lifetable_with_pop_total)

        lifetable_with_pop <-
          lifetable_with_pop_male_female_total %>%
          # Nest the lifetable elements
          tidyr::nest(
            lifetable_with_pop_nest =
              c(age, age_end,
                prob_natural_death, prob_total_death,
                population, deaths))
      # }



      # Join the input without and with lifetable variable into one tibble
      input <-
        dplyr::left_join(input_wo_lifetable,
                         lifetable_with_pop,
                         by = "geo_id_raw")

      } else {
      # If no lifetable, only use input_wo_lifetable
      input <- input_wo_lifetable}




  }
