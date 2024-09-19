# Title and description
#' Find joining columns
#'
#' Find columns in two data frames that have same name and identical values (excluding exceptions).- The resulting joining columns are used to dplyr::x_join data frames and add the vector to by=.
#' @param df1 First \code{Data frame}
#' @param df2 Second \code{Data frame}
#' @param except \code{Vector} of strings showing columns that have to be excluded although they fulfill the inclusion criteria (same name and value)

#'
#' @return
#' This function returns a \code{vector} of strings
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @keywords internal

find_joining_columns <-
  function(df1,
           df2,
           except = NULL){

    joining_columns <-
      # First identify the columns that are common for df1 and df2
      intersect(names(df1),
                names(df2))|>
      # Second, the identical columns of the common ones
      # They are the columns to be used when joining data frames
      purrr::keep(~ identical(df1[[.x]],
                              df2[[.x]]))|>
        # Finally exclude scenario specific columns
        dplyr::setdiff(., except)


  }
