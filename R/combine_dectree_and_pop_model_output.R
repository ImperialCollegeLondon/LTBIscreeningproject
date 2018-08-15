
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

  if (!is.na(folders)) {
    save(e.total, c.total,
         file = pastef(folders$output$scenario, "e_and_c_totals.RData"))
  }

  return(list(e = e.total,
              c = c.total))
}


#' Net monetary benefit matrix
#'
#' This is _not_ incremental.
#'
#' @param ce1
#' @param ce0
#' @param folders
#' @param design_mat
#' @param wtp_min
#' @param wtp_max
#'
#' @return list by wtp
#' @export
#'
#' @examples
#'
nmb_matrix <- function(ce1,
                       ce0,
                       folders = NA,
                       design_mat = NA,
                       wtp_min = 10000,
                       wtp_max = 30000) {

  if (!is.na(folders)) {
    design_mat <-
      pastef(folders$output$scenario,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()
  }

  wtp_seq <- seq(wtp_min, wtp_max, by = 10000)

  nmb_wtp <-
    lapply(wtp_seq,
           FUN = function(wtp) nmb_scenarios(ce0$e, ce0$c,
                                             ce1$e, ce1$c,
                                             wtp)) %>%
    do.call(what = rbind, args = .)

  # join inputs and outputs
  nmb_mat <-
    merge(x = design_mat,
          y = nmb_wtp,
          by = "scenario") %>%
    mutate(type = factor(type,
                         levels = c("statusquo", "screened"))) %>%
    arrange(scenario, wtp, type)

  if (!is.na(folders)) {
    save(nmb_mat,
         file = pastef(folders$output$scenario, "sim_matrix.RData"))
  }

  # return as list
  nmb_mat <- split(nmb_mat, nmb_mat$wtp)

  return(nmb_mat)
}


#' nmb_matrix_tb
#'
#' @param aTB_CE_stats
#' @param ...
#'
#' @return
#' @export
#'
nmb_matrix_tb <- function(aTB_CE_stats,
                          ...) {

  ce0 <- make_ce0(aTB_CE_stats)
  ce1 <- make_ce1(aTB_CE_stats)

  nmb_matrix(ce1, ce0, ...)
}
