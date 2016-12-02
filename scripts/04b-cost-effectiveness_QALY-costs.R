#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost due to active TB in UK arrivals


QALY_uk_tb <- calc_QALY_uk_tb(IMPUTED_sample_year_cohort,
                              utility$disease_free,
                              utility$activeTB,
                              endpoint = "death")

cfr_age_groups <- IMPUTED_sample_year_cohort$cfr_age_groups[uk_tb_TRUE_year]

# CFR for each active TB case
cfr_uk_tb <- cfr_age_lookup[cfr_age_groups, "cfr"]


# status-quo --------------------------------------------------------------

QALY_uk_tb_cured_original <- QALY_uk_tb$cured

# total cost due to diagnosis and treatment
aTB_cost.statusquo <- unit_cost$aTB_TxDx * n.tb_year


# screened ----------------------------------------------------------------

ICER <- list()
INMB <- list()
p.costEffective <- list()
aTB_QALYgain <- list()
aTB_cost.screened <- list()
aTB_QALY.screened <- list()
aTB_cost_diff <- list()

E.aTB_cost.screened <- NA
E.aTB_QALY.screened <- NA


for (scenario in seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario))

  aTB_cost.screened[[scenario]] <- NA
  aTB_QALY.screened[[scenario]] <- NA
  ICER[[scenario]] <- NA
  INMB[[scenario]] <- NA
  p.costEffective[[scenario]] <- NA
  aTB_QALYgain[[scenario]]  <- NA         #QALY[screen] - QALY[statusquo]
  aTB_cost_diff[[scenario]] <- NA         #cost[screen] - cost[statusquo]


  for (simnum in uk_tbX_names){

    n.diseasefree <- filter(n.tb_screen[[scenario]],
                            status=="disease-free", sim==simnum)$n

    # random sample and include death status due to active TB
    uk_tb_death <- (cfr_uk_tb > runif(n.tb_year))
    QALY_uk_tb$cured[uk_tb_death] <- QALY_uk_tb$death[uk_tb_death]

    aTB_QALY.statusquo[[scenario]][simnum] <- sum(QALY_uk_tb$cured)

    aTB_QALY.screened[[scenario]][simnum] <- create_screened_cohort_QALYs(n.diseasefree, QALY_uk_tb)
    aTB_cost.screened[[scenario]][simnum] <- create_screened_cohort_cost(n.diseasefree, aTB_cost.statusquo, unit_cost$aTB_TxDx)

    QALY_uk_tb$cured <- QALY_uk_tb_cured_original
  }


  ######################
  # cost-effectiveness #
  # statistics         #
  ######################

  aTB_QALYgain[[scenario]] <- aTB_QALY.screened[[scenario]] - aTB_QALY.statusquo[[scenario]]
  aTB_QALYgain[[scenario]] <- aTB_QALYgain[[scenario]][!is.na(aTB_QALYgain[[scenario]])]
  aTB_QALYgain[[scenario]] <- aTB_QALYgain[[scenario]]/pop_year

  aTB_cost.screened[[scenario]] <- aTB_cost.screened[[scenario]][!is.na(aTB_cost.screened[[scenario]])]

  # cost difference for each simulation
  aTB_cost_diff[[scenario]] <- (aTB_cost.screened[[scenario]] - aTB_cost.statusquo)/pop_year

  # expected screening cost over all simulations in scenario
  E.aTB_cost.screened[scenario] <- mean(aTB_cost.screened[[scenario]], na.rm = TRUE)

  # expected screening QALYs over all simulations in scenario
  E.aTB_QALY.screened[scenario] <- mean(aTB_QALY.screened[[scenario]], na.rm = TRUE)

  # ICER by sims
  ICER[[scenario]] <- calc.ICER(delta.e = aTB_cost_diff[[scenario]],
                                delta.c = aTB_QALYgain[[scenario]])

  # INMB by sims
  INMB[[scenario]] <- calc.INMB(delta.e = aTB_QALYgain[[scenario]],
                                delta.c = aTB_cost_diff[[scenario]],
                                wtp = threshold)

  # proportion CE at threshold/QALY
  p.costEffective[[scenario]] <- prop.table(table(INMB[[scenario]]>0, useNA = "no"))
}

CE_stats_indiv <- list(aTB_QALY.statusquo, aTB_cost.statusquo,
                       aTB_cost_diff, aTB_QALYgain,
                       ICER, INMB, p.costEffective)

CE_stats_scenario <- cbind(E.aTB_cost.screened, aTB_cost.statusquo, E.aTB_QALY.screened, aTB_QALY.statusquo)


save(CE_stats_indiv,
     file = paste(diroutput, "CE_stats_indiv.RData", sep = "/"))

save(CE_stats_scenario,
     file = paste(diroutput, "CE_stats_scenario.RData", sep = "/"))
