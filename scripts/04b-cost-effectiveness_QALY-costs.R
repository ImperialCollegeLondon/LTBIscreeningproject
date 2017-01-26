#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost incurred due to active TB in UK arrivals


n.tb_year.ENDPOINT <- if (cost.ENDPOINT=="exit uk"){n.tb_year
                      }else if (cost.ENDPOINT=="death"){n.tb_year + n.exit_tb}


QALY_uk_tb <- calc_QALY_uk_tb(data = IMPUTED_sample_year_cohort,
                              utility$disease_free,
                              utility$activeTB,
                              endpoint = QALY.ENDPOINT)

cfr_age_groups <- subset(x = IMPUTED_sample_year_cohort,
                         subset = uk_tb_TRUE_year,
                         select = cfr_age_groups) %>%
                         as.matrix()

# CFR for each active TB case
cfr_uk_tb <- cfr_age_lookup[cfr_age_groups, "cfr"]


# status-quo --------------------------------------------------------------

QALY_uk_tb_cured_original <- QALY_uk_tb$cured


# screened ----------------------------------------------------------------

aTB_ICER <- list()
aTB_INMB <- list()
aTB_p.costEffective <- list()
aTB_QALYgain <- list()
aTB_QALYgain_person <- list()
aTB_cost.screened <- list()
aTB_QALY.screened <- list()
aTB_QALY.statusquo <- list()
aTB_cost_diff <- list()
aTB_cost_diff_person <- list()

E.aTB_cost.screened <- NA
E.aTB_QALY.screened <- NA


for (scenario in seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario))

  aTB_cost.screened[[scenario]] <- NA
  aTB_QALY.screened[[scenario]] <- NA
  aTB_QALY.statusquo[[scenario]] <- NA
  aTB_ICER[[scenario]] <- NA
  aTB_INMB[[scenario]] <- NA
  aTB_p.costEffective[[scenario]] <- NA
  aTB_QALYgain[[scenario]]  <- NA         #QALY[screen] - QALY[statusquo]
  aTB_QALYgain_person[[scenario]]  <- NA
  aTB_cost_diff[[scenario]] <- NA         #cost[screen] - cost[statusquo]
  aTB_cost_diff_person[[scenario]] <- NA


  # QALYs and cost with screening
  for (simnum in uk_tbX_names){

    unit_cost.aTB_TxDx <- sample_distributions(param.distns = unit_cost$aTB_TxDx) %>%
                           sum()

    # total cohort cost due to diagnosis and treatment of active TB
    aTB_cost.statusquo <- unit_cost.aTB_TxDx * n.tb_year.ENDPOINT

    n.diseasefree <- filter(n.tb_screen[[scenario]],
                            status=="disease-free", sim==simnum)$n

    # random sample and TRUE if death due to active TB
    uk_tb_fatality <- (cfr_uk_tb > runif(n.tb_year))

    # substitute in QALYs for active TB death
    QALY_uk_tb$cured[uk_tb_fatality] <- QALY_uk_tb$fatality[uk_tb_fatality]

    aTB_QALY.statusquo[[scenario]][simnum] <- sum(QALY_uk_tb$cured)

    aTB_QALY.screened[[scenario]][simnum] <- create_screened_cohort_QALYs(n.diseasefree, QALY_uk_tb)

    aTB_cost.screened[[scenario]][simnum] <- create_screened_cohort_cost(n.diseasefree,
                                                                         aTB_cost.statusquo,
                                                                         unit_cost.aTB_TxDx)
    # reset to status-quo QALYs
    QALY_uk_tb$cured <- QALY_uk_tb_cured_original
  }


  ######################
  # cost-effectiveness #
  # statistics         #
  ######################
  # Q1 - Q0: +ve good
  # C1 - C0: +ve bad

  aTB_QALYgain[[scenario]] <- aTB_QALY.screened[[scenario]] - aTB_QALY.statusquo[[scenario]]
  aTB_QALYgain[[scenario]] <- aTB_QALYgain[[scenario]][!is.na(aTB_QALYgain[[scenario]])]

  # per person
  aTB_QALYgain_person[[scenario]] <- aTB_QALYgain[[scenario]]/pop_year

  # cost difference per person for each simulation
  aTB_cost.screened[[scenario]] <- aTB_cost.screened[[scenario]][!is.na(aTB_cost.screened[[scenario]])]

  aTB_cost_diff[[scenario]] <- aTB_cost.screened[[scenario]] - aTB_cost.statusquo
  aTB_cost_diff_person[[scenario]] <- aTB_cost_diff[[scenario]]/pop_year

  # expected total aTB screening cost over all simulations in scenario
  E.aTB_cost.screened[scenario] <- mean(aTB_cost.screened[[scenario]], na.rm = TRUE)

  # expected total aTB screening QALYs over all simulations in scenario
  E.aTB_QALY.screened[scenario] <- mean(aTB_QALY.screened[[scenario]], na.rm = TRUE)

  # proportion CE at wtp_threshold/QALY
  aTB_p.costEffective[[scenario]] <- prop.table(table(aTB_INMB[[scenario]]>0, useNA = "no"))
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
     file = paste(diroutput, "aTB_CE_stats.RData", sep = "/"))

save(aTB_CE_stats_scenario,
     file = paste(diroutput, "aTB_CE_stats_scenario.RData", sep = "/"))
