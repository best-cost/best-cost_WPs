#' Prepare exposure data

#' @description
#' This function prepares tabular population exposure data compatible with the attribute() and compare() functions, based on gridded pollution concentration data and vector data representing geographic units. The function calculates an average concentration value in each geographic unit, weighted by the fraction of the population in each sub-unit.
#' @param poll_grid \code(SpatRaster) of the pollution concentration data.
#' @param geo_units \code(sf) of the geographic sub-units.
#' @param population \code(Vector) containing the total population number in each geographic sub-unit.
#' @param geo_id_aggregated \code(Vector) containing the id code of the geographic unit the sub-unit belongs to.
#' @return
#' This function returns a vector of population exposure values.
#' @import terra
#' @import sf
#' @import dplyr
#' @export
#' @author Arno Pauwels
#' @note Experimental function
#' @keywords internal
#' @examples
#' TBD

prepare_exposure_data <-
  function(
    poll_grid,
    geo_units,
    population,
    geo_id_aggregated
  ){
    ## extract mean concentration in each raw geo unit
    poll_mean <- terra::extract(poll_grid, geo_units, fun = mean)[, 2]
    ## create table of non-aggregated exposure for raw output
    non_aggregated_exposure <- as.data.frame(
      cbind(
        geo_id_aggregated,
        population,
        poll_mean
      )
    )
    ## create table to calculate pop-weighted exposure
    exposure <- cbind(
      geo_id_aggregated,
      population,
      poll_mean,
      geo_units
    )

    ## calculate population-weighted mean concentration in each aggregated geo unit
    exposure <- exposure |>
      group_by(geo_id_aggregated) |>
      mutate(poll_weighted = population/sum(population)*poll_mean) |>
      summarise(exposure_value = sum(poll_weighted)) |>
      mutate(exposure_type = 'Population-weighted mean concentration') |>
      st_drop_geometry()

    ## build output list
    main <- as.list(exposure)
    raw <- as.list(non_aggregated_exposure)

    detailed <- list(raw)
    names(detailed) <- 'raw'

    output <- list(main, detailed)
    names(output) <- c('main', 'detailed')

    return(output)
  }

