#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost due to active TB in uk arrivals


QALY_uk_tb <- calc_QALY_uk_tb(IMPUTED_sample_year_cohort,
                              utility$disease_free,
                              utility$activeTB,
                              endpoint = "death")

cfr_age_groups <- IMPUTED_sample_year_cohort$cfr_age_groups[uk_tb_TRUE_year]

# CFR for each active TB case
cfr_uk_tb <- cfr_age_lookup[cfr_age_groups, "cfr"]


# status-quo --------------------------------------------------------------

# random sample death status due to active TB
uk_tb_death.statusquo <- cfr_uk_tb > runif(n.tb_year)

totalQALY.statusquo <- QALY_uk_tb$cured
totalQALY.statusquo[uk_tb_death.statusquo] <- QALY_uk_tb$death[uk_tb_death.statusquo]

aTB_QALY.statusquo <- sum(totalQALY.statusquo)

# total cost due to diagnosis and treatment
aTB_cost.statusquo <- aTB_TxDx_cost * n.tb_year


# screened ----------------------------------------------------------------

ICER <- list()
INMB <- list()
p.costEffective <- list()
aTB_QALYgain <- list()
aTB_cost.screened <- list()
aTB_QALY.screened <- list()
aTB_cost_diff <- list()


for (scenario in seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario))

  aTB_cost.screened[[scenario]] <- NA
  aTB_QALY.screened[[scenario]] <- NA
  ICER[[scenario]] <- NA
  INMB[[scenario]] <- NA
  p.costEffective[[scenario]] <- NA
  aTB_QALYgain[[scenario]]  <- NA         #E_QALY[screen] - E_QALY[statusquo]
  aTB_cost_diff[[scenario]] <- NA         #E_cost[screen] - E_cost[statusquo]

  for (simnum in uk_tbX_names){

    # QALY gain
    totalQALY.screened <- totalQALY.statusquo

    n.diseasefree <- filter(n.tb_screen[[scenario]],
                            status=="disease-free", sim==simnum)$n

    if (length(n.diseasefree)>0) {

      which_diseasefree <- sample(1:n.tb_year, n.diseasefree)
      totalQALY.screened[which_diseasefree] <- QALY_uk_tb$diseasefree[which_diseasefree]
    }else {
      n.diseasefree <- 0}

    aTB_QALY.screened[[scenario]][simnum] <- sum(totalQALY.screened)

    # cost
    aTB_cost.screened[[scenario]][simnum] <- aTB_cost.statusquo - (aTB_TxDx_cost * n.diseasefree)
  }

    aTB_QALYgain[[scenario]] <- aTB_QALY.screened[[scenario]] - aTB_QALY.statusquo

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
    ICER[[scenario]] <- ICER(aTB_cost_diff[[scenario]], aTB_QALYgain[[scenario]])

    # INMB by sims
    INMB[[scenario]] <- INMB(aTB_QALYgain[[scenario]], aTB_cost_diff[[scenario]], threshold)

    # proportion CE at threshold/QALY
    p.costEffective[[scenario]] <- prop.table(table(INMB[[scenario]]>0, useNA = "no"))
}

save(aTB_cost_diff, file = paste(diroutput, "aTB_cost_diff.RData", sep="/"))
save(aTB_QALYgain, file = paste(diroutput, "aTB_QALYgain.RData", sep="/"))

save(ICER, INMB, p.costEffective,
     E.aTB_cost.screened, E.aTB_QALY.screened, aTB_cost.statusquo, aTB_QALY.statusquo,
     file = paste(diroutput, "CE-statistics.RData", sep="/"))

