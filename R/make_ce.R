
#' make_ce0
#'
#' @return
#' @export
#'
#' @examples
#'
make_ce0 <- function(popmod_res) {

  list(e = do.call(cbind, popmod_res$QALY.statusquo_person),
       c = do.call(cbind, popmod_res$cost.statusquo_person))
}


#' make_ce1
#'
#' @return
#' @export
#'
#' @examples
#'
make_ce1 <- function(dectree_res,
                     popmod_res,
                     cohort,
                     interv,) {

  ##TODO: move discounting to much earlier when output created
  # then dont need cohort, interv arguments
  screen_discount <- screen_discount(cohort,
                                     discount_rate = interv$discount_rate)

  LTBI_qaly <- map(dectree_res, "mc_health")
  LTBI_cost <- map(dectree_res, "mc_cost")

  LTBI_qaly <- do.call(cbind, LTBI_qaly) * screen_discount
  LTBI_cost <- do.call(cbind, LTBI_cost) * screen_discount

  popmod_qaly <- do.call(cbind, popmod_res$QALY.screened_person)
  popmod_cost <- do.call(cbind, popmod_res$cost.screened_person)

  list(e = LTBI_qaly + popmod_qaly,
       c = LTBI_cost + popmod_cost)
}

