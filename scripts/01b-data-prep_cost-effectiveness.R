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

TO_YEAR <- 2016

# willingness to pay (£)
wtp_threshold <- 2e+04

NUM_SECONDARY_INF <- list(distn = "pert",
                          params = c(mode = 0.2,
                                     min = 0.1,
                                     max = 0.31))

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


# ref. Pareek M et al. Lancet Infect Dis. Elsevier Ltd; 2011;11(6)
# ages 18-35 pooled

who_levels <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")

pLatentTB.who <-
  c(0.03, 0.13, 0.2, 0.3, 0.27) %>%
  setNames(who_levels)


#########
# costs #
#########

unit_cost <- list()


# LTBI --------------------------------------------------------------------

## adverse effects of LTBI treatment

unit_cost$vomiting <- QALY::inflation_adjust_cost(from_year = 2015,
                                                  to_year = TO_YEAR,
                                                  from_cost = 63,
                                                  reference = "Jit & White (2015). NHS Reference costs (Curtis 2013)")

# unit_cost$vomiting <- list(distn = "gamma",
#                            params = c(shape = 5,
#                                       scale = unit_cost$vomiting/5))


unit_cost$hepatotoxicity <- QALY::inflation_adjust_cost(from_year = 2015,
                                                        to_year = TO_YEAR,
                                                        from_cost = 587,
                                                        reference = "Jit & White (2015). Pareek et al. (2011)")
# unit_cost$hepatotoxicity <- list(distn = "gamma",
#                                  params = c(shape = 6.679,
#                                             scale = unit_cost$hepatotoxicity/6.679))

## tests

# LFT test
unit_cost$LFT_test <- QALY::inflation_adjust_cost(from_year = 2013,
                                                  to_year = TO_YEAR,
                                                  from_cost = 2.69,
                                                  reference = "Lilford (2013)")

# hepatitis B, C test
unit_cost$hep_test <- QALY::inflation_adjust_cost(from_year = 2013,
                                                  to_year = TO_YEAR,
                                                  from_cost = 25.42,
                                                  reference = "Lilford (2013)")
# HIV test
unit_cost$HIV_test <- QALY::inflation_adjust_cost(from_year = 2011,
                                                  to_year = TO_YEAR,
                                                  from_cost = 8,
                                                  reference = "Health Protection Agency (2011)")

# unit_cost$HIV_test <- list(distn = "pert",
#                            params = c(mode = unit_cost$HIV_test,
#                                       min = 4.5,
#                                       max = 13.5))


# LTBI complete treatment
# unit_cost$LTBI_Tx <- QALY::inflation_adjust_cost(from_year = 2006,
#                                                  to_year = 2016,
#                                                  from_cost = 483.74,
#                                                  reference = "HTA VOLUME 20, ISSUE 38, MAY 2016, ISSN 1366-527, p.8")

# https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/442192/030615_LTBI_testing_and_treatment_for_migrants_1.pdf

# these are 'low' conservative estimates
# 6 months isoniazid
unit_cost$LTBI_Tx_6mISO$full <- QALY::inflation_adjust_cost(from_year = 2015,
                                                       to_year = TO_YEAR,
                                                       from_cost = 531,
                                                       reference = "Jit & White (2015)")
# 3 months isoniazid + rifampicin
unit_cost$LTBI_Tx_3mISORIF$full <- QALY::inflation_adjust_cost(from_year = 2015,
                                                          to_year = TO_YEAR,
                                                          from_cost = 396,
                                                          reference = "Jit & White (2015)")

# use ratio from Warwick evidence: appendix H, p 292

unit_cost$LTBI_Tx_6mISO$dropout <- QALY::inflation_adjust_cost(from_year = 2015,
                                                               to_year = TO_YEAR,
                                                               from_cost = 88.5,
                                                               reference = "Jit & White (2015)")

unit_cost$LTBI_Tx_3mISORIF$dropout <- QALY::inflation_adjust_cost(from_year = 2015,
                                                                  to_year = TO_YEAR,
                                                                  from_cost = 66,
                                                                  reference = "Jit & White (2015)")


unit_cost$GP_incentive <-
  list(ltbi_positive = list(distn = "none",
                            params = c(mean = 0)),
                            # params = c(mean = 20)),
       active_tb = list(distn = "none",
                        params = c(mean = 0)))
                        # params = c(mean = 100)))


# active TB -----------------------------------------------------

# diagnosis

culture <- QALY::inflation_adjust_cost(from_year = 2015,
                                       to_year = TO_YEAR,
                                       from_cost = 22.29,
                                       reference = "Drobniewski (2015)")

culture <- list(distn = "gamma",
                params = c(shape = 100,
                           scale = culture/100))

xray <- QALY::inflation_adjust_cost(from_year = 2011,
                                    to_year = TO_YEAR,
                                    from_cost = 35,
                                    reference = "NICE guidance CG117 (March 2011)")
xray <- list(distn = "pert",
             params = c(mode = xray,
                        min = 30,
                        max = 50))


smear <- QALY::inflation_adjust_cost(from_year = 2015,
                                     to_year = TO_YEAR,
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
aTB_Tx_mean <- QALY::inflation_adjust_cost(from_year = 2015,
                                           to_year = TO_YEAR,
                                           from_cost = 5329,
                                           reference = "Jit & White (2015)")
aTB_Tx <- list(distn = "gamma",
               params = c(shape = 8.333,
                          scale = aTB_Tx_mean/8.333))


unit_cost$aTB_TxDx <- list(culture = culture,
                           xray = xray,
                           smear = smear,
                           first_visit = first_visit,
                           followup_visit = followup_visit,
                           LFT_test = list(distn = "unif",
                                           params = c(min = unit_cost$LFT_test,
                                                      max = unit_cost$LFT_test)),
                           # HIV_test = list(distn = "unif",
                           #                 params = c(min = unit_cost$HIV_test,
                           #                            max = unit_cost$HIV_test)),
                           # hep_test = list(distn = "unif",
                           #                 params = c(min = unit_cost$hep_test,
                           #                            max = unit_cost$hep_test)),
                           aTB_Tx = aTB_Tx)


effectiveness <-
  list(LTBI_Tx_3mISORIF = list(pmin = 0.33, pmax = 0.84),
       LTBI_Tx_6mISO = list(pmin = 0.48, pmax = 0.77))

test_performance <-
  list(QFT_GIT =
         list(sens = list(pmin = 0.81, pmax = 0.87),
              spec = list(pmin = 0.98, pmax = 1.0)),
       QFT_plus =
         list(sens = list(pmin = 0.88, pmax = 0.88),
              spec = list(pmin = 0.9717, pmax = 0.9717)),
       TSPOT =
         list(sens = list(pmin = 0.85, pmax = 0.93),
              spec = list(pmin = 0.86, pmax = 1.0)))

means <- list(cost.aTB_TxDx =
                unit_cost$aTB_TxDx %>%
                means_distributions() %>%
                sum(),
              num_sec_inf =
                NUM_SECONDARY_INF %>%
                means_distributions() %>%
                unlist())

##########
# health #
##########

# utility -----------------------------------------------------------------

utility <- list()

utility$disease_free <- 1.0 #assume perfect health. only interested in relative changes


# relative to disease-free = 1
utility$activeTB <- 0.933  #Drobniewski/Kruijshaar et al. (2015), on treatment, outpatient p.83
# utility$activeTB <- 0.9   #pre-treatment #Mears, J., (2015) The prospective evaluation of the TB strain typing service in England: a mixed methods study. Thorax
# utility$activeTB <- 0.82  #post-acute #Mears, J., (2015)

# Pasipanodya (2007)?
##TODO: check this number
utility$postTx <- 0.921
# utility$postTx <- 1 #perfectly recovered


# where are these from??
# utility$activeTB_preTx <- 0.9
# utility$activeTB_acute <- 0.675 #2 months
# utility$activeTB_postacute <- 0.813


save(unit_cost, utility, cfr_age_lookup, pLatentTB.who, NUM_SECONDARY_INF, wtp_threshold, effectiveness, test_performance, means,
     file = "data/cost_effectiveness_params.RData")
