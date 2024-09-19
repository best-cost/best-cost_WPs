# Title and description

#' Add meta-information in the data frame containing the input data
#'
#' Add meta-information of the input data within the data frame containing the input data.
#' @param df \code{Data frame} containing the input data
#' @param info \code{String} or \code{Data frame} with one row or \code{Vector} of length 1 showing additional information or id for the pollutant.
#'
#' @return
#' This function returns a \code{data frame} with binding the input data with the info columns (info_ is added to the column names)
#'
#' @author Alberto Castro
#'
#' @note Experimental function
#'
#' @keywords internal
#'
#'


add_info <- function(df, info){

  if(is.null(info)){
    output <-
      dplyr::mutate(df, info = NULL)

  } else if(is.vector(info) & length(info) == 1) {
    output <-
      dplyr::mutate(df, info = info)

    # nrow(vector) = NULL
    # Use sum() to avoid NULL in case of vectors sum(NULL)=0
  } else if(is.data.frame(info) & sum(nrow(info)) == 1){

    output <-
      setNames(info, paste0("info_", names(info))) |>
      dplyr::bind_cols(df, .)

  }else{

    warning("info must be a vector of length 1 or a data frame with one row")
  }


  return(output)

}
