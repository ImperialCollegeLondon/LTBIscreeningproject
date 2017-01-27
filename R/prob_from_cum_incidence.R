#' prob_from_cum_incidence
#'
#' @param cum_incidence
#'
#' @return
#' @export
#'
#' @examples
prob_from_cum_incidence <- function(cum_incidence){

  diff(cum_incidence)/(1 - cum_incidence)
}
