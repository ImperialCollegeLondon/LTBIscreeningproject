
#' make_ce0
#'
#' @param popmod_res
#'
#' @return
#' @export
#'
#' @examples
#'
make_ce0 <- function(popmod_res) {

  list(e = list_to_BCEA(popmod_res$QALY.statusquo_person),
       c = list_to_BCEA(popmod_res$cost.statusquo_person))
}


#' make_ce1
#'
#' @param popmod_res
#' @param dectree_res
#' @param sdiscount
#'
#' @return
#' @export
#'
#' @examples
#'
make_ce1 <- function(popmod_res,
                     dectree_res,
                     sdiscount) {

  LTBI_qaly <- map(dectree_res, "mc_health")
  LTBI_cost <- map(dectree_res, "mc_cost")

  LTBI_qaly <- list_to_BCEA(LTBI_qaly, -sdiscount)
  LTBI_cost <- list_to_BCEA(LTBI_cost, sdiscount)

  popmod_qaly <- list_to_BCEA(popmod_res$QALY.screened_person)
  popmod_cost <- list_to_BCEA(popmod_res$cost.screened_person)

  list(e = LTBI_qaly + popmod_qaly,
       c = LTBI_cost + popmod_cost)
}
