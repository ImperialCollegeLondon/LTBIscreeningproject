#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY loss and cost due to active TB


# assume that after active TB notification the risk of TB related death
# is in the first year only
# and treatment is one year and results in disease-free health
#
# only interested for the QALY gain calculation in active TB cases
# since the other individuals unchanged


##TODO##
# for now just use one of the death time
# date_deathX_issdt_names <- paste("date_death", seq_len(n.uk_tbX), "_issdt", sep="")


##############
# input data #
##############

# 12 month case fatality rate
# Crofts et al (2008)
cfr_age_lookup <- data.frame(age = c("[15,45)", "[45,65)", "[65,200)"),
                             cfr = c(0.0018, 0.0476, 0.1755),
                             a = c(1, 125, 413), #beta distn
                             b = c(564, 2500, 1940))

rownames(cfr_age_lookup) <- cfr_age_lookup$age

# treatment:
aTB_Tx_cost <- 5329
# gamma(8.333, 639.435)

# adverse effects?
# test: cost?


utility.disease_free <- 1.0
utility.activeTB <- 0.933  #Drobniewski/Kruijshaar et al. (2010)


##############
# calc QALYs #
##############

# complete Tx LTBI -> disease-free (assume stay in uk) --------------------

QALY_diseasefree <- calc_QALY_population(utility = utility.disease_free,
                                         time_horizons = c(uk_tb_only.notification_to_allcause_death))


# active TB related death -------------------------------------------------

QALY.uk_tb_death <- calc_QALY_population(utility = utility.activeTB,
                                         time_horizons = 1)

QALY_uk_tb_death <- rep(QALY.uk_tb_death, length(QALY_diseasefree))


# active TB cured ---------------------------------------------------------

QALY_uk_tb_cured <- calc_QALY_population(utility = c(utility.activeTB, utility.disease_free),
                                         time_horizons = c(uk_tb_only.notification_to_allcause_death))



# imputed samples QALYs and cost ---------------------------------------------------

# CFR for each active TB case
cfr_age_groups_uk_tb <- cfr_age_lookup[sample.uk_tb_only$cfr_age_groups, "cfr"]

## random sample death status due to active TB
uk_tb_death.statusquo <- (cfr_age_groups_uk_tb > runif(n.tb))

## status-quo
totalQALY.statusquo <- QALY_uk_tb_cured
totalQALY.statusquo[uk_tb_death.statusquo] <- QALY_uk_tb_death[uk_tb_death.statusquo]

aTB_cost.statusquo <- aTB_Tx_cost * n.tb

aTB_QALYgain <- list()
aTB_cost.screened <- list()
aTB_cost_diff <- list()


for (scenario in seq_len(n.scenarios)){

  aTB_QALYgain[[scenario]] <- NA
  aTB_cost.screened[[scenario]] <- NA
  aTB_cost_diff[[scenario]] <- NA

  for (i in uk_tbX_names){

    # QALY gain

    totalQALY.screened <- totalQALY.statusquo

    n.diseasefree <- n.tb - n.tb_screen[[scenario]]["1", i]
    which_diseasefree <- sample(1:n.tb, n.diseasefree)

    totalQALY.screened[which_diseasefree] <- QALY_diseasefree[which_diseasefree]

    aTB_QALYgain[[scenario]][i] <- sum(totalQALY.screened) - sum(totalQALY.statusquo)
    aTB_QALYgain[[scenario]] <- na.omit(aTB_QALYgain[[scenario]])/n.pop

    # cost

    aTB_cost.screened[[scenario]][i] <- aTB_Tx_cost * n.tb_screen[[scenario]]["1", i]
    aTB_cost.screened[[scenario]] <- na.omit(aTB_cost.screened[[scenario]])
    aTB_cost_diff[[scenario]] <- (aTB_cost.screened[[scenario]] - aTB_cost.statusquo)/n.pop
  }
}


