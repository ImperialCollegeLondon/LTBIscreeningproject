

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
INMB <- function(delta.e, delta.c, wtp){
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
ICER <- function(delta.e, delta.c){
  return(delta.c/delta.e)
}
