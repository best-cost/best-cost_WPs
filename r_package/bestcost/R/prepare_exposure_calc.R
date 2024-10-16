#' Calculate exposure as mean concentration

#' @description
#' This function ...
#' @param erf_ci \code{Vector} with the central, lower and upper estimate of the relative risk
#' @return
#' This function returns ...
#' @import dplyr
#' @import purrr
#' @import terra
#' @import sf
#' @export
#' @author Arno Pauwels
#' @note Experimental function
#' @keywords internal
#' @examples
#' TBD



## Function to calculate exposure
calc_exp <- function(geo_id, geo_unit, poll_grid) {    
  geo_unit <- filter(sectors_shp, sector_id == id) 
  conc <- poll_raster %>% terra::extract(sect)
  exp  <- replicate(
    n = 1e3, 
    mean(
      sample(
        x = unlist(conc),
        size = length(conc),
        replace = T
      )
    )
  )
  return(exp)  
}

