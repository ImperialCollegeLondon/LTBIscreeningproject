#
# project: LTBI screening
# N Green
# Oct 2016
#
# QALY loss and cost due to active TB


# we assume that after active TB notification the risk of TB related death
# is in the first year
# and that treatment last one year and results in disease-free health
# were are only interest for the QALY gain calculation in the active TB cases
# since the other individuals remain the same


##TODO##
# for now just use one of the death time
# date_deathX_issdt_names <- paste("date_death", seq_len(n.impute), "_issdt", sep="")


# 12 month case fatality rate
# Crofts et al (2008)
cfr_age_lookup <- data.frame(age = c("[15,45)", "[45,65)", "[65,200)"),
                             cfr = c(0.0018, 0.0476, 0.1755),
                             a = c(1, 125, 413), #beta distn
                             b = c(564, 2500, 1940))

rownames(cfr_age_lookup) <- c("[15,45)", "[45,65)", "[65,200)")

# treatment:
aTB_Tx_cost <- 5329
# gamma(8.333, 639.435)

# adverse effects?
# test: cost?


n.impute <- 10

sample.uk_tb_only <- IMPUTED_sample[uk_tb_TRUE, ]


utility.disease_free <- 1.0
utility.activeTB <- 0.933  #Drobniewski/Kruijshaar et al. (2010)

notification_to_allcause_death <- with(sample.uk_tb_only,
                                       floor((date_death1_issdt - rNotificationDate_issdt)/365))


# complete Tx LTBI -> disease-free (assume stay in uk) --------------------

QALY_diseasefree <- calc_QALY_population(utility = utility.disease_free,
                                         time_horizons = c(notification_to_allcause_death))


# active TB related death -------------------------------------------------

QALY.uk_tb_death <- calc_QALY_population(utility = utility.activeTB,
                                         time_horizons = 1)

QALY_uk_tb_death <- rep(QALY.uk_tb_death, length(QALY_diseasefree))


# active TB cured ---------------------------------------------------------

QALY_uk_tb_cured <- calc_QALY_population(utility = c(utility.activeTB, utility.disease_free),
                                         time_horizons = c(notification_to_allcause_death))



# imputed samples QALYs and cost ---------------------------------------------------

# CFR for each active TB case
cfr_age_groups_uk_tb <- sample.uk_tb_only$cfr_age_groups


uk_tbX_names <- paste("uk_tb", seq_len(n.impute), sep = "")

for (i in uk_tbX_names){

  #############
  # QALY loss #
  #############

  # random sample death status due to active TB
  uk_tb_death <- cfr_age_lookup[cfr_age_groups_uk_tb, "cfr"] > runif(length(cfr_age_groups_uk_tb))

  ## status-quo
  totalQALY.statusquo <- QALY_uk_tb_cured
  totalQALY.statusquo[uk_tb_death] <- QALY_uk_tb_death[uk_tb_death]

  ## screened
  completed_LTBI_Tx <- sample.uk_tb_only$uk_tb==1 & sample.uk_tb_only[ ,i]==0

  totalQALY.screened <- totalQALY.statusquo
  totalQALY.screened[completed_LTBI_Tx] <- QALY_diseasefree[completed_LTBI_Tx]

  aTB_QALYloss[i] <- sum(totalQALY.screened) - sum(totalQALY.statusquo)

  ########
  # cost #
  ########

  which_aTB <- IMPUTED_sample[ ,i]==1
  aTB_cost[i] <- mean(aTB_Tx_cost * which_aTB)
}



