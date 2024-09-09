# Title and description

#' Re-scale the relative risk
#'
#' Re-scale the relative risk from the increment value in the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual population exposure)
#' @param rr
#' \code{Numeric vector} containing the relative risk. The data frame must contain the central estimate as well as the lower and upper bound of the exposure-response function.
#' @param exp
#' Population exposure to the stressor (e.g. annual population-weighted mean).
#' @param cutoff
#' \code{Numeric value} showing the cut-off exposure in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param erf_increment
#' \code{Numeric value} showing the size of the increment in concentration related to the relative risk provided in the literature (e.g. for 10 ug/m3 PM2.5).
#' @param erf_shape
#' \code{String} showing the shape of the exposure-response function to be assumed using the relative risk from the literature as support point. Options: "linear", log_linear", "linear_log", "log_log".
#' @param erf_eq
#' \code{String} refer to the user-defined function that puts the relative risk in relation with concentration. A string or a data frame can be entered. If a string is entered, it will be converted into a function. The string has to show the function in terms of one variable: "c", which means concentration. E.g. "3+c+c^2". If a data frame is entered, a first column (or with name "c" or "x") should refer to concentration (x-axis) while a second column (or with name "rr" or "y") should refer to relative risk for that concentration (y-axis). A cubic spline natural interpolation will be assumed. Default value = NULL.
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



    if(is.null(erf_eq)){


      if(erf_shape == "linear"){
        erf <-
          function(c){
            1+( (rr-1) * (c-cutoff)/erf_increment )
          }
      }


      if(erf_shape == "log_linear"){
        erf <-
          function(c){
            exp(log(rr) *(c-cutoff)/erf_increment)
          }
      }


      if(erf_shape == "linear_log"){
        erf <-
          function(c){
            1+( (rr-1) * (log(c)-log(cutoff))/log(erf_increment) )
          }
      }

      if(erf_shape == "log_log"){
        erf <-
          function(c){
            exp( log(rr) *(log(c)-log(cutoff))/log(erf_increment) )
          }
      }

    }


    # A second option is to define the erf using
    # an own defined option

    if(!is.null(erf_eq) & is.character(erf_eq)){


      erf <- function(c){
        # eval() and parse() convert the string into a function
        eval(parse(text = erf_eq))

      }

    }

    # A third option is to define the erf using
    # a set of points (x = exposure, y = relative risk)
    # It will be assumed that

    if(!is.null(erf_eq) & is.data.frame(erf_eq)){


      x <-
        ifelse("c" %in% names(erf_eq),
               erf_eq$c,
               ifelse("x" %in% names(erf_eq),
                      erf_eq$x,
                      erf_eq[, 1]))

      y <-
        ifelse("rr" %in% names(erf_eq),
               erf_eq$rr,
               ifelse("y" %in% names(erf_eq),
                      erf_eq$y,
                      erf_eq[, 2]))


      erf <-
        stats::splinefun(
          x = x,
          y = y,
          method = "natural")

    }

    # rr for the specific concentration
    rr_c <-
      erf(exp)

    return(rr_c)

  }
