#' Get discount factor

#' @description Get discount factor based on discount rate (already corrected for inflation)
#' @inheritParams attribute
#' @param time_period \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @author Alberto Castro
#' @note Experimental function
#' @export

get_discount_factor <-
  function(corrected_discount_rate,
           time_period,
           approach_discount = "exponential"){
    # If no corrected_discount_rate is provided,
    # then assume discount_factor = 1
    # This does not change the results

    if(any(is.null(corrected_discount_rate),
           is.null(approach_discount))){

      discount_factor <- 1
    } else{
      # If there is a corrected_discount_rate,
      # apply the function get_discount_factor()


      discount_factor <-
        ifelse(
          approach_discount == "exponential",
          1/((1 + corrected_discount_rate) ^ time_period),
          ifelse(approach_discount == "hyperbolic_harvey_1986",
                 1/((1 + time_period) ^ corrected_discount_rate),
                 ifelse(approach_discount == "hyperbolic_mazur_1987",
                        1/(1 + corrected_discount_rate * time_period),
                        NA)))

    }
  }
