
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
                                            folders) {

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

  save(e.total, c.total,
       file = pastef(folders$output$scenario, "e_and_c_totals.RData"))

  return(list(e = e.total,
              c = c.total))
}


#' Net monetary benefit matrix
#'
#' This is _not_ incremental.
#'
#' @param total
#' @param design_matrix
#' @param aTB_CE_stats
#'
#' @return
#' @export
#'
#' @examples
#'
nmb_matrix <- function(total,
                       design_matrix,
                       aTB_CE_stats) {

  e_screened <- total$e
  c_screened <- total$c

  e_statusquo <- aTB_CE_stats$QALY.statusquo_person
  c_statusquo <- aTB_CE_stats$cost.statusquo_person

  # net monetary benefit by wtp
  nmb_long <-
    lapply(seq(10000, 30000, by = 500),
           FUN = function(wtp) nmb(e_statusquo, c_statusquo,
                                   e_screened, c_screened,
                                   wtp)) %>%
    do.call(what = rbind, args = .)

  # join inputs and outputs
  sim_matrix <-
    merge(x = design_matrix,
          y = nmb_long,
          by = "scenario") %>%
    mutate(policy = factor(policy,
                           levels = c("statusquo", "screened")))

  save(sim_matrix, file = here("data", "sim_matrix.RData"))

  return(sim_matrix)
}
