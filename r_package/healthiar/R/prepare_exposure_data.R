#' Prepare exposure data

#' @description
#' This function prepares tabular population exposure data compatible with the attribute() and compare() functions, based on spatial pollution and geographic units data.
#' @param poll_grid \code(SpatRaster) of the pollution data.
#' @param geo_units \code(sf) of the geographic units data.
#' @return
#' This function returns a vector of population exposure values.
#' @importFrom terra extract
#' @export
#' @author Arno Pauwels
#' @note Experimental function
#' @keywords internal
#' @examples
#' TBD

prepare_exposure_data <-
  function(
    poll_grid,
    geo_units
  ){## calculate mean concentration in each geo unit
    exp <- terra::extract(poll_grid, geo_units, fun = mean)
    return(as.vector(exp[,2]))
  }

