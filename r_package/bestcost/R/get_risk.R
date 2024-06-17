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
#' @param erf_full
#' \code{Boolean value} to show if the exposure-response function is entirely defined by the user  with the argument erf_c (erf_full = TRUE) or by the arguments exp, cutoff, erf_increment and erf_shape (erf_full = TRUE). Default value = FALSE.
#' @param erf_c
#' \code{String} showing the user-defined function that puts the relative risk in relation with concentration. The function must have only one variable: c, which means concentration. E.g. "3+c+c^2". Default value = NULL.
#' @return
#' This function returns three \code{values} corresponding to the central estimate as well as the lower and upper bound of the exposure-response function.
#' @examples
#' get_risk(rr=1.05, exp=10, cutoff=5, erf_shape="linear" )
#' @author Alberto Castro
#' @note Experimental function
#' @export

get_risk <-
  function(rr,
           exp,
           cutoff,
           erf_increment,
           erf_shape,
           erf_full = FALSE,
           erf_c = NULL){

    # The function assumes that the user of the package does not define the function entirely,
    # but using arguments such as exp, cutoff, erf_increment and erf_shape
    # Therefore, the default value of the argument erf_full is FALSE
    # If the user enter a TRUE, erf_c is read. Otherwise the arguments
    # exp, cutoff, erf_increment and erf_shape.

    # Let's write the exposure-response function (erf)
    # based on c (concentration) as single argument

    # A first (and most usual) option is to define the erf using
    # the shape of the function (erf_shape) and
    # the relative risk from the literature

    if(erf_full == FALSE){


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


    # A second (and less usual) option is to define the erf using
    # an own defined option

    if(erf_full == TRUE){

      erf <- function(c){
        # eval() and parse() convert the string into a function
        eval(parse(text = erf_c))

      }

    }

    # rr for the specific concentration
    rr_c <-
      erf(c = exp)

    return(rr_c)

  }
