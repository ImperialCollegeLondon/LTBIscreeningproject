
#' total_incremental_ce
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
total_incremental_ce <- function(cohort,
                                 interv,
                                 popmod_res,
                                 dectree_res,
                                 folders = NA) {

  screen_discount <- screen_discount(cohort,
                                     discount_rate = interv$discount_rate)

  # model checking subsets
  # from_list_to_BCEA(QALYloss_scenario$statusquo_morbidity)
  # from_list_to_BCEA(QALYloss_scenario$statusquo_mortality)
  # from_list_to_BCEA(QALYloss_scenario$screened_morbidity)
  # from_list_to_BCEA(QALYloss_scenario$screened_mortality)
  # from_list_to_BCEA(QALYloss_scenario$statusquo_morb_pp)
  # from_list_to_BCEA(QALYloss_scenario$statusquo_mort_pp)
  # from_list_to_BCEA(QALYloss_scenario$screened_morb_pp)
  # from_list_to_BCEA(QALYloss_scenario$screened_mort_pp)

  tb_cost <- from_list_to_BCEA(popmod_res$cost_incur_person)
  tb_QALYgain <- from_list_to_BCEA(popmod_res$QALYgain_person)
  LTBI_cost <- from_list_to_BCEA(purrr::map(dectree_res, "mc_cost"), screen_discount)
  LTBI_QALYgain <- from_list_to_BCEA(purrr::map(dectree_res, "mc_health"), -screen_discount)

  incr_e <- as.matrix(LTBI_QALYgain + tb_QALYgain)
  incr_c <- as.matrix(LTBI_cost + tb_cost)

  if (!all(is.na(folders))) {
    save(incr_e, incr_c,
         file = pastef(folders$output$scenario, "e_and_c_totals.RData"))
  }

  return(list(e = incr_e,
              c = incr_c))
}

