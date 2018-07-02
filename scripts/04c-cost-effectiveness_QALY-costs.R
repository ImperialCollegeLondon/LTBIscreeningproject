# ***************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB
# random sampling individuals


# data format prep --------------------------------------------------------

# convert from scenario-wise to remain-exit format
scenario_res <-
  dectree_res %>%
  purrr::transpose()

n.scenarios <- length(dectree_res)

interv_cost <- vector(length = interv$N.mc, mode = "list")
interv_QALY <- vector(length = interv$N.mc, mode = "list")
interv_QALYloss <- vector(length = interv$N.mc, mode = "list")

stats_scenario <- vector(length = n.scenarios, mode = "list")
QALYloss_scenario <- vector(length = n.scenarios, mode = "list")


# extract cost-effectiveness variables
# for tb cases

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
         id_avoided_tb) %>%
  mutate(E_cost_sec_inf = means$num_sec_inf * means$cost.aTB_TxDx * all_secondary_inf_discounts,
         E_cost_statusquo = (all_notif_discounts * means$cost.aTB_TxDx) + E_cost_sec_inf,
         E_QALY_statusquo = (cfr * QALY_fatality) + ((1 - cfr) * QALY_cured))


########
# main #
########

interv_scenario_cost <- partial(scenario_cost,
                                endpoint = interv$ENDPOINT_cost,
                                unit_cost.aTB_TxDx = unit_cost$aTB_TxDx,
                                costeff_cohort = costeff_cohort)

interv_scenario_QALY <- partial(scenario_QALY,
                                endpoint = interv$ENDPOINT_QALY,
                                costeff_cohort = costeff_cohort)

for (s in seq_len(n.scenarios)) {

  message(sprintf("[ population model ] scenario: %s", green(s)))

  for (i in seq_len(interv$N.mc)) {

    # set.seed(12345)

    p_LTBI_to_cured <- scenario_res$subset_pop[[s]][i, 'p_LTBI_to_cured']

    interv_cost[[i]] <- interv_scenario_cost(prop_avoided = p_LTBI_to_cured)
    interv_QALY[[i]] <- interv_scenario_QALY(prop_avoided = p_LTBI_to_cured)

    ##TODO: this is a hack to get some numbers for code checking
    interv_QALYloss[[i]] <- scenario_QALYloss(prop_avoided = p_LTBI_to_cured,
                                              endpoint = interv$ENDPOINT_cost,
                                              costeff_cohort = costeff_cohort)
  }

  stats_scenario[[s]] <- costeff_stats(scenario_dat = dectree_res[[s]],
                                       interv_QALY = interv_QALY,
                                       interv_cost = interv_cost,
                                       pop_year = nrow(cohort))

  QALYloss_scenario[[s]] <-
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
     file = pastef(diroutput, "aTB_CE_stats.RData"))

