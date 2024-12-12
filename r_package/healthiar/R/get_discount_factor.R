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
           discount_shape = "exponential"){
    # If no corrected_discount_rate is provided,
    # then assume discount_factor = 1
    # This does not change the results

    if(any(is.null(corrected_discount_rate),
           is.null(discount_shape))){

      discount_factor <- 1
    } else{
      # If there is a corrected_discount_rate,
      # apply the function get_discount_factor()


      discount_factor <-
        ifelse(
          discount_shape == "exponential",
          1/((1 + corrected_discount_rate) ^ time_period),
          ifelse(discount_shape == "hyperbolic_harvey_1986",
                 1/((1 + time_period) ^ corrected_discount_rate),
                 ifelse(discount_shape == "hyperbolic_mazur_1987",
                        1/(1 + corrected_discount_rate * time_period),
                        NA)))

    }
  }
