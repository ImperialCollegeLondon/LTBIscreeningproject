
#' combine_dectree_and_pop_outputs
#'
#' @param cohort
#' @param interv
#' @param aTB_CE_stats
#' @param dectree_res
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
combine_dectree_and_pop_outputs <- function(cohort,
                                            interv,
                                            aTB_CE_stats,
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

  tb_cost <- from_list_to_BCEA(aTB_CE_stats$cost_incur_person)
  tb_QALYgain <- from_list_to_BCEA(aTB_CE_stats$QALYgain_person)
  LTBI_cost <- from_list_to_BCEA(purrr::map(dectree_res, "mc_cost"), screen_discount)
  LTBI_QALYgain <- from_list_to_BCEA(purrr::map(dectree_res, "mc_health"), -screen_discount)

  c.total <- as.matrix(LTBI_cost + tb_cost)
  e.total <- as.matrix(LTBI_QALYgain + tb_QALYgain)

  if (!all(is.na(folders))) {
    save(e.total, c.total,
         file = pastef(folders$output$scenario, "e_and_c_totals.RData"))
  }

  return(list(e = e.total,
              c = c.total))
}

