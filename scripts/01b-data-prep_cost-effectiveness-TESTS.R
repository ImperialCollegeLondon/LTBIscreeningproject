#
# project: LTBI screening
# N Green
# Oct 2016
#
# input data for QALY gains and costs

# assume that after active TB notification the risk of TB related death
# is in the first year only
# and effective treatment is one year and results in disease-free health
#
# only interested for the QALY gain calculation in active TB cases
# since the other individuals unchanged


library(magrittr)

# willingness to pay (£)
wtp_threshold <- 20000


# 12 month active TB case fatality rate

cfr_age_breaks <- c(15, 45, 65, 200)
cfr_age_levels <- cut(0, cfr_age_breaks, right = FALSE) %>%
                    levels()

cfr_age_lookup <- data.frame(age = cfr_age_levels,
                             cfr = c(0.0, 0.0, 0.0))

rownames(cfr_age_lookup) <- cfr_age_lookup$age

#########
# costs #
#########

unit_cost <- list()

# active TB diagnosis
culture <- list(distn = "unif",
                params = c(min = 0,
                           max = 0))

xray <-  list(distn = "unif",
              params = c(min = 0,
                         max = 0))

smear <-  list(distn = "unif",
               params = c(min = 0,
                          max = 0))

first_visit <-  list(distn = "unif",
                     params = c(min = 0,
                                max = 0))

followup_visit <-  list(distn = "unif",
                        params = c(min = 0,
                                   max = 0))
# active TB treatment
aTB_Tx <-  list(distn = "unif",
                params = c(min = 0,
                           max = 0))

unit_cost$aTB_TxDx <- list(culture = culture,
                           xray = xray,
                           smear = smear,
                           first_visit = first_visit,
                           followup_visit = followup_visit,
                           aTB_Tx = aTB_Tx)

##########
# health #
##########

utility <- list()

utility$falsepos_Tx <- 1

utility$disease_free <- 1

utility$activeTB <- 1

utility$activeTB_preTx <- 1
utility$activeTB_acute <- 1
utility$activeTB_postacute <- 1

QALYloss_activeTB <- 1
QALYloss_falseposLTBI_adverse <- 1
QALYloss_falsepos_activeTB_Tx <- 1
