
#' make_incremental_ce
#'
#' @param popmod_res
#' @param t_dectree
#' @param sdiscount
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
make_incremental_ce <- function(popmod_res,
                                t_dectree,
                                sdiscount,
                                folders = NA) {

  tb_cost <- list_to_BCEA_incr(popmod_res$cost_incur_person)
  tb_QALYgain <- list_to_BCEA_incr(popmod_res$QALYgain_person)

  LTBI_cost <- list_to_BCEA_incr(scenario_list = t_dectree$cost_person,
                                 discount = sdiscount)
  LTBI_QALYgain <- list_to_BCEA_incr(t_dectree$QALY_person, -sdiscount)

  incr_e <- LTBI_QALYgain + tb_QALYgain
  incr_c <- LTBI_cost + tb_cost

  if (!all(is.na(folders))) {
    # save(incr_e, incr_c,
    #      file = pastef(folders$output$scenario, "e_and_c_totals.RData"))
  }

  list(e = as.matrix(incr_e),
       c = as.matrix(incr_c))
}

