# Title and description

#' Get population impact over time
#'
#' Get population impact over time
#' @param year_of_analysis \code{Numeric value} of the year of analysis, which corresponds to the first year of the life table
#' @param input_with_risk_and_pop_fraction \code{Tibble} showing the input data and the PAF (population attributable fraction) or PIF (population impact fraction)
#' @param outcome_metric \code{String} to define the outcome metric. Choose between "death", "yll" and "yld"
#'
#' @return
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
#' Moreover, the data frame include columns such as:
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
#' @keywords internal

get_pop_impact <-
  function(year_of_analysis,
           input_with_risk_and_pop_fraction,
           outcome_metric){

    second_year <- year_of_analysis + 1

    pop_impact <-
      input_with_risk_and_pop_fraction %>%
      dplyr::mutate(
        pop_impact_nest = purrr::map(
          lifetable_with_pop_nest,
          ~ dplyr::rename(
            .x,
            !!paste0("population_", year_of_analysis) := population))) %>%
      # Calculate the population the second year (first column after first year) considering the health effect of air pollution
      # And move column up one row: lead()
      dplyr::mutate(
        pop_impact_nest = purrr::map2(
          pop_impact_nest, pop_fraction,
          ~ dplyr::mutate(
            .x,
            "population_{second_year}" :=
              lead(
                dplyr::lag(!!as.symbol(paste0("population_",year_of_analysis))) *
                  dplyr::lag(death_probability_natural) * .y))))


    if(outcome_metric %in% c("yll", "yld", "daly")){
      # Now calculate population over time for the rest of year starting with YOA without considering air pollution
      period <- c( (year_of_analysis + 1) :
                     ((year_of_analysis +
                         unique(purrr::map_int(input_with_risk_and_pop_fraction$lifetable_with_pop_nest,
                                               ~nrow(.x))) - 2)) )
      length_period <- length(period)
      population_period <- paste0("population_", period)


      pop_impact <-
        pop_impact %>%
        dplyr::mutate(
          pop_impact_nest = purrr::map(
            pop_impact_nest,
            function(.x) {

              # length_period minus 1 because year_of_analysis+1 is already calculated
              for (i in 0:(length_period-1)){
                current_year <- period[i+1]
                col_current <- paste0("population_", current_year)
                col_next <- paste0("population_", current_year + 1)
                # avoiding the later introduction of NAs in the right top corner:
                .x[1:(length_period-i), col_next] <-
                   .x[1:(length_period-i), col_current] * (1 - .x$prob_total_death[(i+2):(length_period+1)])


                # Alternative code
                # Simpler but it does not provide the right result
                # .x[[col_next]] <-
                #  .x[[col_current]] * (1 - .x[["prob_total_death"]])
              }
              return(.x)
            } ))
      #
      # %>%
      #
      #
      #
      #   # Empty the top-right half of the table (for the alternative code above)
      #   # They are people that were not born when the exposure happened
      #   # (year of analysis)
      #   dplyr::mutate(.,
      #     pop_impact_nest = purrr::map(
      #       pop_impact_nest,
      #       function(.x){
      #
      #         x_matrix <-
      #           .x %>%
      #           dplyr::select(., all_of(population_period))%>%
      #           as.matrix(.)
      #
      #         x_matrix[row(x_matrix)<=col(x_matrix)] <- NA
      #
      #         .x <-
      #           dplyr::bind_cols(
      #             dplyr::select(.x, -all_of(population_period)),
      #             as_tibble(x_matrix))
      #
      #         return(.x)
      #       }
      #     ))

          }

    return(pop_impact)

  }

