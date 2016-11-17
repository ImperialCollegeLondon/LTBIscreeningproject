#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY loss and cost due to active TB



# all complete Tx LTBI -> disease-free (assume stay in uk) --------------------

QALY_diseasefree <- QALY::calc_QALY_population(utility = utility.disease_free,
                                         time_horizons = c(uk_tb_only.notification_to_allcause_death))


# all active TB related death -------------------------------------------------

QALY.uk_tb_death <- QALY::calc_QALY_population(utility = utility.activeTB,
                                         time_horizons = 1)

QALY_uk_tb_death <- rep(QALY.uk_tb_death, length(QALY_diseasefree))


# all active TB cured ---------------------------------------------------------

QALY_uk_tb_cured <- QALY::calc_QALY_population(utility = c(utility.activeTB, utility.disease_free),
                                         time_horizons = c(uk_tb_only.notification_to_allcause_death))



# imputed samples QALYs and cost ----------------------------------------------

# CFR for each active TB case
cfr_age_groups_uk_tb <- cfr_age_lookup[sample.uk_tb_only$cfr_age_groups, "cfr"]

## random sample death status due to active TB
uk_tb_death.statusquo <- (cfr_age_groups_uk_tb > runif(n.tb))

## status-quo
totalQALY.statusquo <- QALY_uk_tb_cured
totalQALY.statusquo[uk_tb_death.statusquo] <- QALY_uk_tb_death[uk_tb_death.statusquo]

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

    n.diseasefree <- n.tb - n.tb_screen[[scenario]]["uk_tb", sim]
    which_diseasefree <- sample(1:n.tb, n.diseasefree)

    totalQALY.screened[which_diseasefree] <- QALY_diseasefree[which_diseasefree]

    aTB_QALYgain[[scenario]][sim] <- sum(totalQALY.screened) - sum(totalQALY.statusquo)
    aTB_QALYgain[[scenario]] <- na.omit(aTB_QALYgain[[scenario]])/n.pop

    # cost

    aTB_cost.screened[[scenario]][sim] <- aTB_TxDx_cost * n.tb_screen[[scenario]]["uk_tb", sim]
    aTB_cost.screened[[scenario]] <- na.omit(aTB_cost.screened[[scenario]])

    aTB_cost_diff[[scenario]] <- (aTB_cost.screened[[scenario]] - aTB_cost.statusquo)/n.pop
  }
}


