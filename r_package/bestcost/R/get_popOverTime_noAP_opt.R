# Title and description

#' Get population over time without changing air pollution exposure
#'
#' Get population over time without changing air pollution exposure
#' @param lifetable_wPop \code{Data frame} with three columns: the first one should refer to age, the second one to the probability of dying and the third one to the population (sex specific),
#' @param firstYear_lifetable Numeric value of the year of analysis, which corresponds to the first year of the life table
#' @param year_loopStart Numeric value of the year where the deaths have to discounted (normally the first year of the lifetable +1 or +2)
#' @return
#' This function returns a \code{data.frame} with the life table plus an additional column for population one year later without changing air pollution exposure.
#' @import dplyr
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
get_popOverTime_noAP_opt <-
function(lifetable_wPop, firstYear_lifetable, year_loopStart){

  # End of the loop
  year_loopEnd <- firstYear_lifetable + nrow(lifetable_wPop) - 1

  # Calculate population for the next years without considering the effect of air pollution
  # output <-
  #   lifetable_wPop
  output <-
    bind_cols(lifetable_wPop, as_tibble(matrix(0, nrow = 100, ncol = 99)))
  names(output) <- c("age", "death_probability", "population_2019", "population_2020",
                     "population_2021",  "population_2022",  "population_2023",  "population_2024",
                     "population_2025",  "population_2026",  "population_2027",  "population_2028",
                     "population_2029",  "population_2030",  "population_2031",  "population_2032",
                     "population_2033",  "population_2034",  "population_2035",  "population_2036",
                     "population_2037",  "population_2038",  "population_2039",  "population_2040",
                     "population_2041",  "population_2042",  "population_2043",  "population_2044",
                     "population_2045",  "population_2046",  "population_2047",  "population_2048",
                     "population_2049",  "population_2050",  "population_2051",  "population_2052",
                     "population_2053",  "population_2054",  "population_2055",  "population_2056",
                     "population_2057",  "population_2058",  "population_2059",  "population_2060",
                     "population_2061",  "population_2062",  "population_2063",  "population_2064",
                     "population_2065",  "population_2066",  "population_2067",  "population_2068",
                     "population_2069",  "population_2070",  "population_2071",  "population_2072",
                     "population_2073",  "population_2074",  "population_2075",  "population_2076",
                     "population_2077",  "population_2078",  "population_2079",  "population_2080",
                     "population_2081",  "population_2082",  "population_2083",  "population_2084",
                     "population_2085",  "population_2086",  "population_2087",  "population_2088",
                     "population_2089",  "population_2090",  "population_2091",  "population_2092",
                     "population_2093",  "population_2094",  "population_2095",  "population_2096",
                     "population_2097",  "population_2098",  "population_2099",  "population_2100",
                     "population_2101",  "population_2102",  "population_2103",  "population_2104",
                     "population_2105",  "population_2106",  "population_2107",  "population_2108",
                     "population_2109",  "population_2110",  "population_2111",  "population_2112",
                     "population_2113",  "population_2114",  "population_2115",  "population_2116",
                     "population_2117",  "population_2118")

  for (yl in year_loopStart : year_loopEnd){
    output[, paste0("population_", yl)] <-
      dplyr::lag(output[, paste0("population_", yl-1)]) *
      (1 - dplyr::lag(output$death_probability))
  }
  return(output)

}
