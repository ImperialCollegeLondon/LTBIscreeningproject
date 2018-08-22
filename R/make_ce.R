
#' make_ce0
#'
#' @return
#' @export
#'
#' @examples
make_ce0 <- function(aTB_CE_stats) {

  list(e = do.call(cbind, aTB_CE_stats$QALY.statusquo_person),
       c = do.call(cbind, aTB_CE_stats$cost.statusquo_person))
}


#' make_ce1
#'
#' @return
#' @export
#'
#' @examples
make_ce1 <- function(aTB_CE_stats) {

  list(e = do.call(cbind, aTB_CE_stats$QALY.screened_person),
       c = do.call(cbind, aTB_CE_stats$cost.screened_person))
}
