
#' Incremental Net (Monetary) Benefit
#'
#' @param delta.e
#' @param delta.c
#' @param wtp
#'
#' @return
#' @export
#'
#' @examples
#'
calc.INMB <- function(delta.e, delta.c, wtp){

  if(wtp<0)
    stop("Willingness to pay must be non-negative.")

  return(delta.e*wtp - delta.c)
}


#' Incremental Cost Effectiveness Ratio
#'
#' @param delta.e
#' @param delta.c
#'
#' @return
#' @export
#'
#' @examples
#'
calc.ICER <- function(delta.e, delta.c){

  if(any(delta.e==0))
    warning("One or more health difference is 0.")

  return(delta.c/delta.e)
}
