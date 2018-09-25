
#' activetb_qaly_cost
#'
#' @param dectree_res
#' @param interv
#' @param cohort
#' @param folders list of strings
#'
#' @return
#' \itemize{
#'   \item QALY.statusquo
#'   \item QALY.screened
#'   \item E_cost_screened
#'   \item cost.screened_person
#'   \item cost.statusquo_person
#'   \item cost_incur
#'   \item cost.statusquo
#'   \item cost.screened
#'   \item E_QALY_screened
#'   \item QALY.screened_person
#'   \item QALY.statusquo_person
#'   \item QALYgain
#'   \item cost_incur_person
#'   \item E_cost_incur
#'   \item E_cost_incur_person
#'   \item QALYgain_person
#'   \item E_QALYgain
#'   \item E_QALYgain_person
#' }
#' @export
#'
#' @examples
#'
activetb_qaly_cost <- function(dectree_res,
                               interv,
                               cohort,
                               folders = NA) {

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
    dplyr::select(cfr,
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

  n_cohort <- nrow(cohort)

  ########
  # main #
  ########

  interv_scenario_cost <- partial(scenario_cost,
                                  endpoint = interv$ENDPOINT_cost,
                                  unit_cost = unit_cost,
                                  probs_contact = p_contact_tracing,
                                  cohort = costeff_cohort)

  interv_scenario_QALY <- partial(scenario_QALY,
                                  endpoint = interv$ENDPOINT_QALY,
                                  cohort = costeff_cohort)

  interv_scenario_QALYloss <- partial(scenario_QALYloss,
                                      endpoint = interv$ENDPOINT_QALY,
                                      cohort = costeff_cohort)

  for (ss in seq_len(n.scenarios)) {

    message(sprintf("[ population model ] scenario: %s", green(ss)))

    p_cured_scenario <- scenario_res$subset_pop[[ss]][, 'p_LTBI_to_cured']

    interv_cost <- map(p_cured_scenario, interv_scenario_cost)

    interv_QALY <- map(p_cured_scenario, interv_scenario_QALY, ordered = FALSE)

    interv_QALYloss <- map(p_cured_scenario, interv_scenario_QALYloss)

    stats_scenario[[ss]] <-
      costeff_stats(scenario_dat = dectree_res[[ss]],
                    interv_QALY = interv_QALY,
                    interv_cost = interv_cost,
                    pop_year = n_cohort)

    QALYloss_scenario[[ss]] <-
      interv_QALYloss %>%
      purrr::transpose() %>%
      simplify_all()
  }

  popmod_res <-
    stats_scenario %>%
    purrr::transpose()

  QALYloss_scenario <-
    QALYloss_scenario %>%
    purrr::transpose()

  if (!all(is.na(folders))) {

    save(popmod_res,
         file = pastef(folders$output$scenario, "popmod_res.RData"))
  }

  invisible(popmod_res)
}
