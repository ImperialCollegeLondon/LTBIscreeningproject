
#' activetb_qaly_cost
#'
#' For the Population model,
#' calculate various QALYs and costs accounting for active TB progression
#' in non-cured cohort subset.
#'
#' @param dectree_res Output of \code{parallel_decision_tree()}. This contains the probability of being cured of LTBI via screening.
#' @param interv list of fixed model run parameter values
#' @param cohort dataframe of individual level data
#' @param folders list of strings locations for data and plots
#'
#' @return
#' \itemize{
#'   \item QALY.statusquo: For each scenario a vector of total QALYs without screening programme, length number of sims. These are all the same because population QALYs are not varied for the cohort.
#'   \item QALY.screened: For each scenario a vector of total QALYs with screening programme, length number of sims.
#'   \item E_cost_screened: For each scenario single expected cost with screening programme.
#'   \item cost.screened_person: For each scenario a vector of QALYs per person with screening programme, length number of sims.
#'   \item cost.statusquo_person: For each scenario a vector of costs per person without screening programme, length number of sims.
#'   \item cost_incur: For each scenario a vector of incurred costs by screening programme, length number of sims.
#'   \item cost.statusquo: For each scenario a vector of total costs without screening programme, length number of sims. The are not identical because TB costs are randomly sampled.
#'   \item cost.screened: For each scenario a vector of total costs with screening programme, length number of sims.
#'   \item E_QALY_screened: For each scenario single expected QALYs with screening programme.
#'   \item QALY.screened_person: For each scenario a vector of QALYs per person with screening programme, length number of sims.
#'   \item QALY.statusquo_person: For each scenario a vector of QALYs per person without screening programme, length number of sims. These are all the same.
#'   \item QALYgain: For each scenario a vector of total QALYs gained with screening programme as the difference between screening and status-quo, length number of sims.
#'   \item cost_incur_person: For each scenario a vector of total costs incured per person with screening programme as the difference between screening and status-quo, length number of sims.
#'   \item E_cost_incur: For each scenario the expectedd total cost incured with screening programme as the difference between screening and status-quo.
#'   \item E_cost_incur_person: For each scenario the expected total cost incured per person with screening programme as the difference between screening and status-quo.
#'   \item QALYgain_person: For each scenario a vector of total QALY gained per person with screening programme as the difference between screening and status-quo.
#'   \item E_QALYgain: For each scenario the expected total QALY gained with screening programme as the difference between screening and status-quo.
#'   \item E_QALYgain_person: For each scenario the expected total QALy gained per person with screening programme as the difference between screening and status-quo.
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

  p_LTBI_to_cured <-
    map_df(scenario_res$subset_pop,
           function(x) as.data.frame(x)$p_LTBI_to_cured)


  ########
  # main #
  ########

  cost_partial <- partial(scenario_cost,
                          endpoint = interv$ENDPOINT_cost,
                          unit_cost = unit_cost,
                          probs_contact = p_contact_tracing,
                          cohort = costeff_cohort)

  QALY_partial <- partial(scenario_QALY,
                          endpoint = interv$ENDPOINT_QALY,
                          cohort = costeff_cohort)

  QALYloss_partial <- partial(scenario_QALYloss,
                              endpoint = interv$ENDPOINT_QALY,
                              cohort = costeff_cohort)

  for (ss in seq_len(n.scenarios)) {

    message(sprintf("[ population model ] scenario: %s", green(ss)))

    p_cured <- p_LTBI_to_cured[[ss]]

    interv_cost <- map(p_cured, cost_partial, ordered = FALSE)

    interv_QALY <- map(p_cured, QALY_partial, ordered = FALSE)

    interv_QALYloss <- map(p_cured, QALYloss_partial)

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
