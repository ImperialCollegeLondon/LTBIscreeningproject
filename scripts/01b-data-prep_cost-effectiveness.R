#
# project: LTBI screening
# N Green
# Oct 2016
#
# input data for QALY loss and cost due to active TB


# assume that after active TB notification the risk of TB related death
# is in the first year only
# and treatment is one year and results in disease-free health
#
# only interested for the QALY gain calculation in active TB cases
# since the other individuals unchanged


##TODO##
# for now just use one of the death time
# date_deathX_issdt_names <- paste("date_death", seq_len(n.uk_tbX), "_issdt", sep="")




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
