#' Re-scale the relative risk

#' @description Re-scale the relative risk from the increment value in the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual population exposure)
#' @inheritParams attribute
#' @param rr
#' \code{Numeric vector} containing the relative risk. The data frame must contain the central estimate as well as the lower and upper bound of the exposure-response function.
#' @param exp
#' Population exposure to the stressor (e.g. annual population-weighted mean).
#' @param cutoff
#' \code{Numeric value} showing the cut-off exposure level in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param erf_eq \code Equation of the user-defined exposure-response function that puts the relative risk (y) in relation with exposure (x). If the function is provided as \code{string}, it can only contains one variable: x (exposure). E.g. "3+x+x^2". If the function is provided as a \code{function}, the object should have a function class. If only the values of the x-axis (exposure) and y axis (relative risk) of the dots in the exposure-response function are available, a cubic spline natural interpolation can be assumed to get the function using, e.g., \code{stats::splinefun(x, y, method="natural")}
#' @return
#' This function returns three \code{values} corresponding to the central estimate as well as the lower and upper bound of the exposure-response function.
#' @examples
#' get_risk(rr=1.05, exp=10, cutoff=5, erf_shape="linear" )
#' @author Alberto Castro
#' @note Experimental function
#' @export

get_risk <-
  function(rr = NULL,
           exp,
           cutoff = NULL,
           erf_increment = NULL,
           erf_shape = NULL,
           erf_eq = NULL){

    # The function assumes that the user of the package does not define the function entirely,
    # but using arguments such as exp, cutoff, erf_increment and erf_shape
    # Therefore, the default value of the argument erf_eq should be NULL
    # If the user enter a TRUE, erf_eq is read. Otherwise the arguments
    # exp, cutoff, erf_increment and erf_shape.

    # Let's write the exposure-response function (erf)
    # based on c (concentration) as single data

    # A first (and most usual) option is to define the erf using
    # the shape of the function (erf_shape) and
    # the relative risk from the literature

    if ( is.null(erf_eq) ) {


      if ( erf_shape == "linear" ) {
        erf <-
          function(c){
            1+( (rr-1) * (c-cutoff)/erf_increment )
          }
        rr_c <- erf(exp)
        return(rr_c)
      }


      if ( erf_shape == "log_linear" ) {
        erf <-
          function(c){
            exp(log(rr) *(c-cutoff)/erf_increment)
          }
        rr_c <- erf(exp)
        return(rr_c)
      }


      if ( erf_shape == "linear_log" ) {
        erf <-
          function(c){
            1+( (rr-1) * (log(c)-log(cutoff))/log(erf_increment) )
          }
        rr_c <- erf(exp)
        return(rr_c)
      }

      if ( erf_shape == "log_log" ) {
        erf <-
          function(c){
            exp( log(rr) *(log(c)-log(cutoff))/log(erf_increment) )
          }
        rr_c <- erf(exp)
        return(rr_c)
      }

    }


    # A second option is to define the erf using
    # an own defined option

    if ( !is.null(erf_eq) & is.character(erf_eq) ) {

      ## Original function
      erf <- function(c){
        # eval() and parse() convert the string into a function
        base::eval(base::parse(text = erf_eq))
      }
      rr_c <- erf(exp)
        return(rr_c)
      ## Option using mapply (runs correctly without rowwise(), see #377 )
      # erf <- function(c, erf_eq) {
      #   mapply(function(eq, val) {
      #     base::eval(base::parse(text = eq), list(c = val))
      #   }, erf_eq, c)
      # }
      # rr_c <- erf(c = exp, erf_eq = erf_eq)
      # return(rr_c)


    }

    # A third option is to define the erf using
    # a set of points (x = exposure, y = relative risk)
    # It will be assumed that

    if ( !is.null(erf_eq) & is.function(erf_eq) ){


     erf <- erf_eq

     if(!is.null(cutoff)){
       exp <- exp - cutoff

       if (exp < 0){
         exp <- 0 # Avoid negative exposures
       }
       rr_c <- erf(exp)
       return(rr_c)
     }


    }

  }
