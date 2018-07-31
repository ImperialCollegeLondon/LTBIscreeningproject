#
# project: LTBI screening
# N Green
# Oct 2016
#
# input data for QALY gains and costs
#
# for more details and alternative references see:
# see C:\Users\ngreen1\Google Drive\LTBI-screening-cost-effectiveness\parameter-table_all_refs.xlsx

# assumptions --------------------------------------------------------------
#
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

LFT_test = list(distn = "unif",
                params = c(min = unit_cost$LFT_test,
                           max = unit_cost$LFT_test))

HIV_test = list(distn = "unif",
                params = c(min = unit_cost$HIV_test,
                           max = unit_cost$HIV_test))

hep_test = list(distn = "unif",
                params = c(min = unit_cost$hep_test,
                           max = unit_cost$hep_test))

# treatment
aTB_Tx_mean <- QALY::inflation_adjust_cost(from_year = 2015,
                                           to_year = TO_YEAR,
                                           from_cost = 5329,
                                           reference = "Jit & White (2015)")
aTB_Tx <- list(distn = "gamma",
               params = c(shape = 8.333,
                          scale = aTB_Tx_mean/8.333))


## COMMENT OUT FOR TEST CASES ######

# unit_cost$aTB_TxDx <- list(culture = culture,
#                            xray = xray,
#                            smear = smear,
#                            first_visit = first_visit,
#                            followup_visit = followup_visit,
#                            LFT_test = LFT_test,
#                            # HIV_test = HIV_test,
#                            # hep_test = hep_test,
#                            aTB_Tx = aTB_Tx)

# fixed constant
unit_cost$aTB_TxDx <- list(distn = "none",
                           params = c(mean = 5410))

#####################################



# Warwick evidence (2016)
unit_cost$TSPOT = list(distn = "unif",
                       params = c(min = 50,
                                  max = 106))
# NICE CG117
# unit_cost$TST = list(distn = "unif",
#                      params = c(min = 8,
#                                 max = 36))


# contact tracing

unit_cost$aTB_Dx <-
  list(
    culture = culture,
    xray = xray)

unit_cost$IGRA <- unit_cost$TSPOT

unit_cost$LTBI_DxTx <-
  list(
    LTBI_Tx = list(distn = "none",
                   params = c("mean" = unit_cost$LTBI_Tx_6mISO$full))
  )


#####################################

means <- list(cost.aTB_TxDx =
                unit_cost$aTB_TxDx %>%
                means_distributions() %>%
                sum(),
              num_sec_inf =
                NUM_SECONDARY_INF %>%
                means_distributions() %>%
                unlist())

# probabilities

effectiveness <-
  list(
    LTBI_Tx_3mISORIF = branch_unif_params(pmin = 0.33,
                                          pmax = 0.84,
                                          name = "Effective"),
    LTBI_Tx_6mISO = branch_unif_params(pmin = 0.48,
                                       pmax = 0.77,
                                       name = "Effective")
  )


test_performance <-
  list(
    QFT_GIT =
      test(
        sens = branch_unif_params(pmin = 0.81,
                                  pmax = 0.87,
                                  name = "Sensitivity"),
        spec = branch_unif_params(pmin = 0.98,
                                  pmax = 1.0,
                                  name = "Specificity")
      ),
    QFT_plus =
      test(
        sens = branch_unif_params(pmin = 0.88,
                                  pmax = 0.88,
                                  name = "Sensitivity"),
        spec = branch_unif_params(pmin = 0.9717,
                                  pmax = 0.9717,
                                  name = "Specificity")
      ),
    TSPOT =
      test(
        sens = branch_unif_params(pmin = 0.85,
                                  pmax = 0.93,
                                  name = "Sensitivity"),
        spec = branch_unif_params(pmin = 0.86,
                                  pmax = 1.0,
                                  name = "Specificity")
      )
  )


# Rodger (2003) BMJ
# in years
treatment_delay <-
  list(
    distn = "pert",
    params = c(min = 14/365.25,
               mode = 49/365.25,
               max = 103/365.25))

p_contact_tracing <-
  c(contact = 1,
    aTB_Tx = 0.018, # Cavany (2017)
    # LTBI_DxTx = 0.1) ## Cavany (2017); for children but only available
    LTBI_DxTx = 0.281) # Fox (2013)

p_contact_tracing['aTB_Dx'] <-
  p_contact_tracing['LTBI_DxTx'] + p_contact_tracing['aTB_Tx']

# Cavany (2017)
NUM_CONTACTS <- 4.5


##########
# health #
##########

utility <- list()

utility$disease_free <- 1.0 #assume perfect health. only interested in relative changes

# relative to disease-free = 1
utility$activeTB <- 0.912 #Mears; 0.88 - 0.88*0.9
utility$TB_Tx <- 0.933  #Mears; 0.88 – 0.813 = 0.067
utility$postTx <- 1 #perfectly recovered


# save --------------------------------------------------------------------

save(unit_cost, utility, effectiveness, test_performance, means, pLatentTB.who, wtp_threshold, p_contact_tracing, treatment_delay,
     file = "data/cost_effectiveness_params.RData")

save(cfr_age_lookup, NUM_SECONDARY_INF, NUM_CONTACTS,
     file = "data/synthetic_cohort_params.RData")
