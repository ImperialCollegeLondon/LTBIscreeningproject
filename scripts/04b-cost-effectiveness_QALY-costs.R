#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY loss and cost due to active TB


# single year
QALY_uk_tb <- calc_QALY_uk_tb(IMPUTED_sample_year_cohort,
                              utility.disease_free,
                              utility.activeTB,
                              endpoint = "death")

cfr_age_groups <- IMPUTED_sample_year_cohort$cfr_age_groups[uk_tb_TRUE_year]


# imputed samples QALYs and cost ----------------------------------------------

# CFR for each active TB case
cfr_uk_tb <- cfr_age_lookup[cfr_age_groups, "cfr"]

## random sample death status due to active TB
uk_tb_death.statusquo <- (cfr_uk_tb > runif(length(cfr_uk_tb)))

## status-quo
totalQALY.statusquo <- QALY_uk_tb$cured
totalQALY.statusquo[uk_tb_death.statusquo] <- QALY_uk_tb$death[uk_tb_death.statusquo]

aTB_cost.statusquo <- aTB_TxDx_cost * n.tb

aTB_QALYgain <- list()
aTB_cost.screened <- list()
aTB_cost_diff <- list()


for (scenario in seq_len(n.scenarios)){

  aTB_cost.screened[[scenario]] <- NA
  aTB_QALYgain[[scenario]]  <- NA         #E_screen[QALY] - E_statusquo[QALY]
  aTB_cost_diff[[scenario]] <- NA         #E_screen[cost] - E_statusquo[cost]

  for (sim in uk_tbX_names){

    # QALY gain

    totalQALY.screened <- totalQALY.statusquo

    n.diseasefree <- n.tb_year - n.tb_screen[[scenario]]["uk_tb", sim]
    which_diseasefree <- sample(1:n.tb_year, n.diseasefree)

    totalQALY.screened[which_diseasefree] <- QALY_uk_tb$diseasefree[which_diseasefree]

    aTB_QALYgain[[scenario]][sim] <- sum(totalQALY.screened) - sum(totalQALY.statusquo)
    aTB_QALYgain[[scenario]] <- na.omit(aTB_QALYgain[[scenario]])/n.pop

    # cost

    aTB_cost.screened[[scenario]][sim] <- aTB_TxDx_cost * n.tb_screen[[scenario]]["uk_tb", sim]
    aTB_cost.screened[[scenario]] <- na.omit(aTB_cost.screened[[scenario]])

    aTB_cost_diff[[scenario]] <- (aTB_cost.screened[[scenario]] - aTB_cost.statusquo)/n.pop
  }
}


