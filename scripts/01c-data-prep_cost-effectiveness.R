#
# project: LTBI screening
# N Green
# Oct 2016
#
# input data for QALY loss and cost


# assume that after active TB notification the risk of TB related death
# is in the first year only
# and treatment is one year and results in disease-free health
#
# only interested for the QALY gain calculation in active TB cases
# since the other individuals unchanged



threshold <- 20000  #£


# 12 month case fatality rate
# Crofts et al (2008)
cfr_age_breaks <- c(15, 45, 65, 200)
cfr_age_levels <- levels(cut(0, cfr_age_breaks, right = FALSE))

cfr_age_lookup <- data.frame(age = cfr_age_levels,
                             cfr = c(0.0018, 0.0476, 0.1755),
                             a = c(1, 125, 413), #beta distn
                             b = c(564, 2500, 1940))

rownames(cfr_age_lookup) <- cfr_age_lookup$age


#########
# costs #
#########

unit_cost <- list()

# active TB diagnosis:
unit_cost$culture <- QALY::inflation_adjust_cost(from_year = 2015,
                                                 to_year = 2016,
                                                 from_cost = 22.29)  #Drobniewski (2015)

unit_cost$xray  <- QALY::inflation_adjust_cost(from_year = 2011,
                                               to_year = 2016,
                                               from_cost = 16.54)    #NICE guidance CG117

unit_cost$smear <- QALY::inflation_adjust_cost(from_year = 2015,
                                               to_year = 2016,
                                               from_cost = 8.23)     #2015 #Jit M, Stagg HR, Aldridge RW, et al. Dedicated outreach service for hard to reach patients with tuberculosis

aTB_Dx_cost <- unit_cost$culture + unit_cost$xray + unit_cost$smear

# active TB treatment:
aTB_Tx_cost <- QALY::inflation_adjust_cost(from_year = 2014,
                                           to_year = 2016,
                                           from_cost = 5329)  #2014 #Jit& White/ NICE guidance CG117
# gamma(8.333, 639.435)

aTB_TxDx_cost <- aTB_Dx_cost + aTB_Tx_cost


# adverse effects of LTBI Tx
unit_cost$vomiting <- QALY::inflation_adjust_cost(from_year = 2013,
                                                  to_year = 2016,
                                                  from_cost = 63)        #Jit&White/ NHS Reference costs (Curtis 2013)

unit_cost$hepatotoxicity <- QALY::inflation_adjust_cost(from_year = 2011,
                                                        to_year = 2016,
                                                        from_cost = 587) #Jit&White/ Pareek et al. 2011

# LTBI complete treatment
LTBI_Tx_cost <- QALY::inflation_adjust_cost(from_year = 2006,
                                            to_year = 2016,
                                            from_cost = 483.74)        #HTA VOLUME 20 ISSUE 38 MAY 2016 ISSN 1366-527, p.8

# LFT test
LFT_test_cost <- QALY::inflation_adjust_cost(from_year = 2009,
                                             to_year = 2016,
                                             from_cost = 2.69)        #Lilford

# hep B, C test
hep_test_cost <- QALY::inflation_adjust_cost(from_year = 2009,
                                             to_year = 2016,
                                             from_cost = 25.42)        #
# HIV test
HIV_test_cost <- QALY::inflation_adjust_cost(from_year = 2010,
                                             to_year = 2016,
                                             from_cost = 8)        #

##########
# health #
##########

utility <- list()

utility$disease_free <- 1.0
utility$activeTB <- 0.933  #Drobniewski/Kruijshaar et al. (2010)

