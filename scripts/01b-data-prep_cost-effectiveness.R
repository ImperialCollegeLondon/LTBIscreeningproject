#
# project: LTBI screening
# N Green
# Oct 2016
#
# input data for QALY gains and costs


# details -----------------------------------------------------------------

# assume that after active TB notification the risk of TB related death
# is in the first year only
#
# effective treatment duration is one year and results in disease-free health
#
# for the QALY gain calculation only interested in active TB cases
# since the other individuals unchanged


# willingness to pay (£)
wtp_threshold <- 20000


# 12 month active TB case fatality rate -----------------------------------

cfr_age_breaks <- c(15, 45, 65, 200)
cfr_age_levels <- cut(0, cfr_age_breaks, right = FALSE) %>%
                    levels()

cfr_age_lookup <- data.frame(age = cfr_age_levels,
                             cfr = c(0.012, 0.048, 0.176),
                             distn = c("beta", "beta", "beta"),
                             a = c(NA, NA, NA),
                             b = c(NA, NA, NA))

rownames(cfr_age_lookup) <- cfr_age_lookup$age

attr(cfr_age_lookup, "reference") <- "Crofts et al (2008)"


#########
# costs #
#########

unit_cost <- list()


# LTBI --------------------------------------------------------------------

## adverse effects of LTBI treatment

unit_cost$vomiting <- QALY::inflation_adjust_cost(from_year = 2015,
                                                  to_year = 2016,
                                                  from_cost = 63,
                                                  reference = "Jit & White (2015). NHS Reference costs (Curtis 2013)")

# unit_cost$vomiting <- list(distn = "gamma",
#                            params = c(shape = 5,
#                                       scale = unit_cost$vomiting/5))


unit_cost$hepatotoxicity <- QALY::inflation_adjust_cost(from_year = 2015,
                                                        to_year = 2016,
                                                        from_cost = 587,
                                                        reference = "Jit & White (2015). Pareek et al. (2011)")
# unit_cost$hepatotoxicity <- list(distn = "gamma",
#                                  params = c(shape = 6.679,
#                                             scale = unit_cost$hepatotoxicity/6.679))

## tests

# LFT test
unit_cost$LFT_test <- QALY::inflation_adjust_cost(from_year = 2013,
                                                  to_year = 2016,
                                                  from_cost = 2.69,
                                                  reference = "Lilford (2013)")

# hepatitis B, C test
unit_cost$hep_test <- QALY::inflation_adjust_cost(from_year = 2013,
                                                  to_year = 2016,
                                                  from_cost = 25.42,
                                                  reference = "Lilford (2013)")
# HIV test
unit_cost$HIV_test <- QALY::inflation_adjust_cost(from_year = 2011,
                                                  to_year = 2016,
                                                  from_cost = 8,
                                                  reference = "Health Protection Agency (2011)")

# unit_cost$HIV_test <- list(distn = "pert",
#                            params = c(mode = unit_cost$HIV_test,
#                                       min = 4.5,
#                                       max = 13.5))


# LTBI complete treatment
unit_cost$LTBI_Tx <- QALY::inflation_adjust_cost(from_year = 2006,
                                                 to_year = 2016,
                                                 from_cost = 483.74,
                                                 reference = "HTA VOLUME 20, ISSUE 38, MAY 2016, ISSN 1366-527, p.8")


unit_cost$LTBI_Tx_6mISO <- QALY::inflation_adjust_cost(from_year = 2015,
                                                       to_year = 2016,
                                                       from_cost = 531,
                                                       reference = "Jit & White (2015)")

unit_cost$LTBI_Tx_3mISORIF <- QALY::inflation_adjust_cost(from_year = 2015,
                                                          to_year = 2016,
                                                          from_cost = 396,
                                                          reference = "Jit & White (2015)")

# active TB -----------------------------------------------------

# diagnosis

culture <- QALY::inflation_adjust_cost(from_year = 2015,
                                       to_year = 2016,
                                       from_cost = 22.29,
                                       reference = "Drobniewski (2015)")

culture <- list(distn = "gamma",
                params = c(shape = 100,
                           scale = culture/100))

xray <- QALY::inflation_adjust_cost(from_year = 2011,
                                    to_year = 2016,
                                    from_cost = 35,
                                    reference = "NICE guidance CG117 (March 2011)")
xray <- list(distn = "pert",
             params = c(mode = xray,
                        min = 30,
                        max = 50))


smear <- QALY::inflation_adjust_cost(from_year = 2015,
                                     to_year = 2016,
                                     from_cost = 8.23,
                                     reference = "(2015) Jit M, Stagg HR, Aldridge RW, et al. Dedicated outreach service for hard to reach patients with tuberculosis")
smear <- list(distn = "gamma",
              params = c(shape = 106,
                         scale = smear/106))

# NHS England. (2013). 2014 / 15 National Tariff Payment System
first_visit <- list(distn = "gamma",
                    params = c(shape = 53.3,
                               scale = 4.52)) #241

# NHS England. (2013). 2014 / 15 National Tariff Payment System
followup_visit <- list(distn = "gamma",
                       params = c(shape = 18.78,
                                  scale = 7.62)) #143


# treatment
aTB_Tx <- QALY::inflation_adjust_cost(from_year = 2015,
                                      to_year = 2016,
                                      from_cost = 5329,
                                      reference = "Jit & White (2015)")
aTB_Tx <- list(distn = "gamma",
               params = c(shape = 8.333,
                          scale = aTB_Tx/8.333))


unit_cost$aTB_TxDx <- list(culture = culture,
                           xray = xray,
                           smear = smear,
                           first_visit = first_visit,
                           followup_visit = followup_visit,
                           HIV_test = list(distn = "unif",
                                           params = c(min = unit_cost$HIV_test,
                                                      max = unit_cost$HIV_test)),
                           LFT_test = list(distn = "unif",
                                           params = c(min = unit_cost$LFT_test,
                                                      max = unit_cost$LFT_test)),
                           hep_test = list(distn = "unif",
                                           params = c(min = unit_cost$hep_test,
                                                      max = unit_cost$hep_test)),
                           aTB_Tx = aTB_Tx)


##########
# health #
##########


# utility -----------------------------------------------------------------

utility <- list()

utility$falsepos_Tx <- 0.9

utility$disease_free <- 1.0 #assume perfect health. we're only interest in relative changes


# relative to disease free = 1
utility$activeTB <- 0.933  #Drobniewski/Kruijshaar et al. (2010), on treatment, outpatient
utility$activeTB <- 0.9   #pre-treatment #Mears, J., (2015) The prospective evaluation of the TB strain typing service in England: a mixed methods study. Thorax
utility$activeTB <- 0.82  #post-acute #Mears, J., (2015)


# where are these from??
# utility$activeTB_preTx <- 0.9
# utility$activeTB_acute <- 0.675 #2 months
# utility$activeTB_postacute <- 0.813



# overall mean QALY loss --------------------------------------------------

QALYloss <- list()

QALYloss$activeTB <- 0.4  #15-34 year olds. includes Tx adverse events   #Mears J, et al. Thorax 2015
QALYloss$falseposLTBI_adverse <- 0.0008   #Mears J, et al. Thorax 2015
QALYloss$falsepos_activeTB_Tx <- 0.03     #Mears J, et al. Thorax 2015


