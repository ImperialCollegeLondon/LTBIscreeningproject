#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY gain and cost due to active TB


# single year
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

# total cost due to diagnosis and treatment
aTB_cost.statusquo <- aTB_TxDx_cost * n.tb_year



# screened ----------------------------------------------------------------

aTB_QALYgain <- list()
aTB_cost.screened <- list()
aTB_cost_diff <- list()


for (scenario in seq_len(n.scenarios)){

  aTB_cost.screened[[scenario]] <- NA
  aTB_QALYgain[[scenario]]  <- NA         #E_screen[QALY] - E_statusquo[QALY]
  aTB_cost_diff[[scenario]] <- NA         #E_screen[cost] - E_statusquo[cost]

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

    aTB_QALYgain[[scenario]][simnum] <- sum(totalQALY.screened) - sum(totalQALY.statusquo)

    # cost
    aTB_cost.screened[[scenario]][simnum] <- aTB_cost.statusquo - (aTB_TxDx_cost * n.diseasefree)
  }

    aTB_QALYgain[[scenario]] <- na.omit(aTB_QALYgain[[scenario]])/pop_year

    aTB_cost.screened[[scenario]] <- na.omit(aTB_cost.screened[[scenario]])
    aTB_cost_diff[[scenario]] <- (aTB_cost.screened[[scenario]] - aTB_cost.statusquo)/pop_year

    # ICER
    ICER[[scenario]] <- aTB_cost_diff/aTB_QALYgain

    # INMB
    INMB[[scenario]] <- aTB_QALYgain*threshold - aTB_cost_diff

    # proportion CE at threshold/QALY
    p.CostEffective[scenario] <- prop.table(table(INMB>0))
}

# save to file?
save(aTB_cost_diff, file = "aTB_cost_diff")
save(aTB_QALYgain, file = "aTB_QALYgain")


