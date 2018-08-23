
#' make_incremental_ce
#'
#' @param popmod_res
#' @param dectree_res
#' @param sdiscount
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
make_incremental_ce <- function(popmod_res,
                                dectree_res,
                                sdiscount,
                                folders = NA) {

  tb_cost <- list_to_BCEA_incr(popmod_res$cost_incur_person)
  tb_QALYgain <- list_to_BCEA_incr(popmod_res$QALYgain_person)

  mc_cost <- purrr::map(dectree_res, "mc_cost")
  mc_health <- purrr::map(dectree_res, "mc_health")

  LTBI_cost <- list_to_BCEA_incr(mc_cost, sdiscount)
  LTBI_QALYgain <- list_to_BCEA_incr(mc_health, -sdiscount)

  incr_e <- as.matrix(LTBI_QALYgain + tb_QALYgain)
  incr_c <- as.matrix(LTBI_cost + tb_cost)

  if (!all(is.na(folders))) {
    save(incr_e, incr_c,
         file = pastef(folders$output$scenario, "e_and_c_totals.RData"))
  }

  return(list(e = incr_e,
              c = incr_c))
}

