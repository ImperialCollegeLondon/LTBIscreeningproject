#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB


aTB_cost.screened <- aTB_QALY.screened <- list()
aTB_cost_diff <- aTB_cost_diff_person <- list()
aTB_QALYgain <- aTB_QALYgain_person <- list()
aTB_ICER <- aTB_INMB <- list()
aTB_p.costEffective <- list()
aTB_QALY.statusquo <- list()

E.aTB_cost.screened <- NA
E.aTB_QALY.screened <- NA


for (s in seq_len(n.scenarios)) {

  print(sprintf("scenario: %d", s))

  aTB_cost.screened[[s]] <- aTB_QALY.screened[[s]] <- NA
  aTB_cost_diff[[s]] <- aTB_cost_diff_person[[s]] <- NA   #cost[screen] - cost[statusquo]
  aTB_QALYgain[[s]] <- aTB_QALYgain_person[[s]] <- NA     #QALY[screen] - QALY[statusquo]
  aTB_ICER[[s]] <- aTB_INMB[[s]] <- NA
  aTB_p.costEffective[[s]] <- NA
  aTB_QALY.statusquo[[s]] <- NA


  # QALYs and cost with screening -------------------------------------------

  for (i in seq_len(N.mc)) {

    unit_cost.aTB_TxDx <-
      unit_cost$aTB_TxDx %>%
      sample_distributions() %>%
      sum()

    num_avoided.all_tb <-
      dplyr::filter(n.tb_screen.all_tb[[s]],
                    status == "disease-free",
                    sim == i) %>%
      use_series(n)

    num_avoided.uk_tb <-
      dplyr::filter(n.tb_screen.uk_tb[[s]],
                    status == "disease-free",
                    sim == i) %>%
      use_series(n)

    # TRUE if death due to active TB
    tb_fatality <-
      IMPUTED_sample_year_cohort %>%
      with(.,
           runif(n = nrow(cfr)) %>%
             is_less_than(cfr) %>%
             na.omit())

    # substitute in QALYs for active TB death
    QALY_all_tb$cured[tb_fatality] <- QALY_all_tb$fatality[tb_fatality]

    aTB_QALY.statusquo[[s]][i] <- sum(QALY_all_tb$cured)

    aTB_QALY.screened[[s]][i] <-
      screened_cohort_QALYs(num_avoided.all_tb,
                            QALY_all_tb) %>%
      sum()


    aTB_cost.statusquo <- unit_cost.aTB_TxDx * num_all_tb_cost

    aTB_cost.screened[[s]][i] <- screened_cohort_cost(num_avoided.uk_tb,
                                                      aTB_cost.statusquo,
                                                      unit_cost.aTB_TxDx)
    # reset to status-quo QALYs
    QALY_all_tb$cured <- QALY_tb_cured_original
  }


  # final cost-effectiveness statistics  ----------------------------------------------------

  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  aTB_QALYgain[[s]] <- aTB_QALY.screened[[s]] - aTB_QALY.statusquo[[s]]
  aTB_QALYgain[[s]] <- rm_na(aTB_QALYgain[[s]])

  # per person
  aTB_QALYgain_person[[s]] <- aTB_QALYgain[[s]]/pop_year

  # cost difference per person for each simulation
  aTB_cost.screened[[s]] <- rm_na(aTB_cost.screened[[s]])

  aTB_cost_diff[[s]] <- aTB_cost.screened[[s]] - aTB_cost.statusquo
  aTB_cost_diff_person[[s]] <- aTB_cost_diff[[s]]/pop_year

  # expected total aTB screening cost over all simulations in scenario
  E.aTB_cost.screened[s] <-
    aTB_cost.screened[[s]] %>%
    mean(na.rm = TRUE)

  # expected total aTB screening QALYs over all simulations in scenario
  E.aTB_QALY.screened[s] <-
    aTB_QALY.screened[[s]] %>%
    mean(na.rm = TRUE)

  # proportion CE at wtp_threshold/QALY
  aTB_p.costEffective[[s]] <- prop.table(table(aTB_INMB[[s]] > 0, useNA = "no"))
}


#  save --------------------------------------------------------------------

aTB_CE_stats <- list(aTB_QALY.statusquo = aTB_QALY.statusquo,
                     aTB_cost.statusquo = aTB_cost.statusquo,
                     aTB_cost_diff = aTB_cost_diff,
                     aTB_QALYgain = aTB_QALYgain,
                     aTB_cost_diff_person = aTB_cost_diff_person,
                     aTB_QALYgain_person = aTB_QALYgain_person,
                     aTB_p.costEffective = aTB_p.costEffective)

aTB_CE_stats_scenario <- cbind(E.aTB_cost.screened,
                               E.aTB_QALY.screened,
                               aTB_QALY.statusquo)

save(aTB_CE_stats,
     file = pastef(diroutput, "aTB_CE_stats.RData"))

save(aTB_CE_stats_scenario,
     file = pastef(diroutput, "aTB_CE_stats_scenario.RData"))

