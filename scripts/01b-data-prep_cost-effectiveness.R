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
                             cfr = c(0.012, 0.012, 0.012),
                             distn = c("beta", "beta", "beta"),
                             a = c(NA, NA, NA),
                             b = c(NA, NA, NA))

rownames(cfr_age_lookup) <- cfr_age_lookup$age

attr(cfr_age_lookup, "reference") <- "Crofts et al (2008)"


#########
# costs #
#########

unit_cost <- list()

# active TB diagnosis
culture <- QALY::inflation_adjust_cost(from_year = 2015,
                                       to_year = 2016,
                                       from_cost = 22.29,
                                       reference = "Drobniewski (2015)")
culture <- list(distn = "gamma",
                params = c(shape = 100,
                           scale = culture/100))

xray  <- QALY::inflation_adjust_cost(from_year = 2011,
                                     to_year = 2016,
                                     from_cost = 35,
                                     reference = "NICE guidance CG117")
xray <- list(distn = "pert",
             params = c(mode = xray,
                        min = 23,
                        max = 43))


smear <- QALY::inflation_adjust_cost(from_year = 2015,
                                     to_year = 2016,
                                     from_cost = 8.23,
                                     reference = "(2015) Jit M, Stagg HR, Aldridge RW, et al. Dedicated outreach service for hard to reach patients with tuberculosis")
smear <- list(distn = "gamma",
              params = c(shape = 106,
                         scale = smear/106))

first_visit <- list(distn = "gamma",
                    params = c(shape = 53.3,
                               scale = 4.52)) #241

followup_visit <- list(distn = "gamma",
                       params = c(shape = 18.78,
                                  scale = 7.62)) #143


# active TB treatment
aTB_Tx <- QALY::inflation_adjust_cost(from_year = 2014,
                                      to_year = 2016,
                                      from_cost = 5329,
                                      reference = "(2014) Jit & White. NICE guidance CG117")
aTB_Tx <- list(distn = "gamma",
               params = c(shape = 8.333,
                          scale = aTB_Tx/8.333))


unit_cost$aTB_TxDx <- list(culture = culture,
                           xray = xray,
                           smear = smear,
                           first_visit = first_visit,
                           followup_visit = followup_visit,
                           aTB_Tx = aTB_Tx)


# adverse effects of LTBI Tx
unit_cost$vomiting <- QALY::inflation_adjust_cost(from_year = 2013,
                                                  to_year = 2016,
                                                  from_cost = 63,
                                                  reference = "Jit & White. NHS Reference costs (Curtis 2013)")

unit_cost$hepatotoxicity <- QALY::inflation_adjust_cost(from_year = 2011,
                                                        to_year = 2016,
                                                        from_cost = 587,
                                                        reference = "Jit & White. Pareek et al. (2011)")

# LTBI complete treatment
unit_cost$LTBI_Tx <- QALY::inflation_adjust_cost(from_year = 2006,
                                                 to_year = 2016,
                                                 from_cost = 483.74,
                                                 reference = "HTA VOLUME 20 ISSUE 38 MAY 2016 ISSN 1366-527, p.8")

# LFT test
unit_cost$LFT_test <- QALY::inflation_adjust_cost(from_year = 2009,
                                                  to_year = 2016,
                                                  from_cost = 2.69,
                                                  reference = "Lilford")

# hepatitis B, C test
unit_cost$hep_test <- QALY::inflation_adjust_cost(from_year = 2009,
                                                  to_year = 2016,
                                                  from_cost = 25.42)
# HIV test
unit_cost$HIV_test <- QALY::inflation_adjust_cost(from_year = 2010,
                                                  to_year = 2016,
                                                  from_cost = 8)

##########
# health #
##########

utility <- list()

utility$falsepos_Tx <- 0.9

utility$disease_free <- 1.0 #assume perfect health. we're only interest in relative changes


# relative to disease free = 1
utility$activeTB <- 0.933  #Drobniewski/Kruijshaar et al. (2010)
utility$activeTB <- 0.9   #pre-treatment #Mears, J., (2015) The prospective evaluation of the TB strain typing service in England: a mixed methods study. Thorax
utility$activeTB <- 0.82  #post-acute


utility$activeTB_preTx <- 0.9
utility$activeTB_acute <- 0.675 #2 months
utility$activeTB_postacute <- 0.813

QALYloss_activeTB <- 0.4  #15-34 year olds. includes Tx adverse events
QALYloss_falseposLTBI_adverse <- 0.0008
QALYloss_falsepos_activeTB_Tx <- 0.03

