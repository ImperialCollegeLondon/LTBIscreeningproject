
#' activetb_qaly_cost
#'
#' @param dectree_res
#' @param interv
#' @param cohort
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
#'
activetb_qaly_cost <- function(dectree_res,
                               interv,
                               cohort,
                               folders) {

  scenario_res <-
    dectree_res %>%
    purrr::transpose()

  n.scenarios <- length(dectree_res)

  interv_cost <- vector(length = interv$N.mc, mode = "list")
  interv_QALY <- vector(length = interv$N.mc, mode = "list")
  interv_QALYloss <- vector(length = interv$N.mc, mode = "list")

  stats_scenario <- vector(length = n.scenarios, mode = "list")
  QALYloss_scenario <- vector(length = n.scenarios, mode = "list")

  costeff_cohort <-
    cohort %>%
    dplyr::filter(as.logical(all_tb)) %>%
    select(cfr,
           uk_tb,
           all_tb,
           tb_fatality,
           QALY_statusquo,
           QALY_diseasefree,
           QALY_cured,
           QALY_fatality,
           uk_notif_discounts,
           all_notif_discounts,
           uk_secondary_inf_discounts,
           all_secondary_inf_discounts,
           num_2nd_inf,
           num_contacts,
           id_avoided_tb)

  costeff_cohort <-
    costeff_cohort %>%
    expected_cost_QALY(means)


  ########
  # main #
  ########

  interv_scenario_cost <- partial(scenario_cost,
                                  endpoint = interv$ENDPOINT_cost,
                                  unit_cost = unit_cost,
                                  probs = p_contact_tracing,
                                  costeff_cohort = costeff_cohort)

  interv_scenario_QALY <- partial(scenario_QALY,
                                  endpoint = interv$ENDPOINT_QALY,
                                  costeff_cohort = costeff_cohort)

  for (ss in seq_len(n.scenarios)) {

    message(sprintf("[ population model ] scenario: %s", green(ss)))

    for (i in seq_len(interv$N.mc)) {

      # set.seed(12345)

      p_LTBI_to_cured <- scenario_res$subset_pop[[ss]][i, 'p_LTBI_to_cured']

      interv_cost[[i]] <- interv_scenario_cost(prop_avoided = p_LTBI_to_cured)
      interv_QALY[[i]] <- interv_scenario_QALY(prop_avoided = p_LTBI_to_cured)

      ##TODO: rewrite; this is a hack to get some numbers for code checking
      interv_QALYloss[[i]] <-
        scenario_QALYloss(prop_avoided = p_LTBI_to_cured,
                          endpoint = interv$ENDPOINT_QALY,
                          costeff_cohort = costeff_cohort)
    }

    stats_scenario[[ss]] <-
      costeff_stats(scenario_dat = dectree_res[[ss]],
                    interv_QALY = interv_QALY,
                    interv_cost = interv_cost,
                    pop_year = nrow(cohort))

    QALYloss_scenario[[ss]] <-
      interv_QALYloss %>%
      purrr::transpose() %>%
      simplify_all()
  }

  aTB_CE_stats <-
    stats_scenario %>%
    purrr::transpose()

  QALYloss_scenario <-
    QALYloss_scenario %>%
    purrr::transpose()

  save(aTB_CE_stats,
       file = pastef(folders$output$scenario, "aTB_CE_stats.RData"))

  invisible(aTB_CE_stats)
}
