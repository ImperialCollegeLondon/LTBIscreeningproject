
#' combine_popmod_dectree_res
#'
#' @param cohort
#' @param interv
#' @param popmod_res
#' @param dectree_res
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
combine_popmod_dectree_res <- function(cohort,
                                       interv,
                                       popmod_res,
                                       dectree_res,
                                       folders) {

  screen_discount <- screen_discount(cohort,
                                     interv$discount_rate)

  ce_incr <- make_incremental_ce(popmod_res,
                                 dectree_res,
                                 screen_discount,
                                 folders)

  ce0 <- make_ce0(popmod_res)

  ce1 <- make_ce1(popmod_res,
                  dectree_res,
                  screen_discount)

  list(ce0 = ce0,
       ce1 = ce1,
       ce_incr = ce_incr)
}
