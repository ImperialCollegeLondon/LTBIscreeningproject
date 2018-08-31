
#' bcea_incremental
#'
#' @param ce_incr
#'
#' @return
#' @export
#'
#' @examples
#'
bcea_incremental <- function(ce_incr) {

  BCEA::bcea(e = -ce_incr$e,  # Q1 - Q0 different way round in original function!
             c =  -ce_incr$c,
             ref = 1,
             interventions = colnames(ce_incr$e))
}
