
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

  mc_cost <- purrr::map(dectree_res, "mc_cost")
  mc_health <- purrr::map(dectree_res, "mc_health")

  tb_cost <- list_to_BCEA_incr(popmod_res$cost_incur_person)
  tb_QALYgain <- list_to_BCEA_incr(popmod_res$QALYgain_person)

  LTBI_cost <- list_to_BCEA_incr(mc_cost, sdiscount)
  LTBI_QALYgain <- list_to_BCEA_incr(mc_health, -sdiscount)

  incr_e <- LTBI_QALYgain + tb_QALYgain
  incr_c <- LTBI_cost + tb_cost

  if (!all(is.na(folders))) {
    save(incr_e, incr_c,
         file = pastef(folders$output$scenario, "e_and_c_totals.RData"))
  }

  list(e = as.matrix(incr_e),
       c = as.matrix(incr_c))
}

