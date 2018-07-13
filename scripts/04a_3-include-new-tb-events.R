# *************************************************
# LTBI screening
# N Green
#
# include new tb events and update dependencies
#
# impute missing/unobserved time to active tb
# prob: scaled progression probs with IMPUTED_sample weighted average LTBI prob
# individually SIMULATE active TB progression times after exit uk and followup


# cohort mean prevalence? incidence atm
p_LTBI_cohort <- mean(IMPUTED_sample$pLTBI)
p_tb_given_LTBI_year <- p_incid_year/p_LTBI_cohort

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(
    tb_years = sim_tb_times(data = .,
                            prob = p_tb_given_LTBI_year),
    exituk_tb.years = ifelse(exit_uk1, tb_years, NA),
    notif_issdt.years = ifelse(exit_uk1, NA, tb_years),
    exituk_tb = !is.na(exituk_tb.years) & !is.infinite(exituk_tb.years),
    uk_tb = !is.na(notif_issdt.years) & !is.infinite(notif_issdt.years))


# is someone screened before something else happens?
IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(
    screened_before_exit = date_exit_uk1_issdt.years >= screen_year,
    screened_before_tb = (notif_issdt.years >= screen_year) | is.na(notif_issdt.years),
    screened_before_death = date_death1_issdt.years >= screen_year,
    screen = ifelse(screened_before_death &
                      screened_before_exit &
                      screened_before_tb,
                    yes = 1, no = 0))


# combine (imputed) exit_uk tb and uk_tb data ------------------------------------------
##TODO: check ceiling(), floor() use
IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(
    all_tb = uk_tb | exituk_tb,
    all_tb_issdt = ifelse(uk_tb,
                          ceiling(notif_issdt.years),
                          ceiling(exituk_tb.years)),

    # progression to death times
    uk_death_notif = date_death1_issdt.years - ifelse(test = is.infinite(notif_issdt.years),
                                                      yes = NA,
                                                      no = notif_issdt.years),
    all_death_notif = ceiling(date_death1_issdt.years - all_tb_issdt),
    all_death_notif = ifelse(all_death_notif < 0, NA, all_death_notif),

    # progression ages
    age_uk_notification = floor(age_at_entry + notif_issdt.years),
    age_exituk_notification = floor(age_at_entry + exituk_tb.years),
    age_all_notification = ifelse(uk_tb,
                                  age_uk_notification,
                                  age_exituk_notification),
    # progression age groups
    agegroup_uk_notif = cut(age_uk_notification,
                            breaks = cfr_age_breaks,
                            right = FALSE),
    agegroup_all_notif = cut(age_all_notification,
                             breaks = cfr_age_breaks,
                             right = FALSE))

# case fatality rate
# for each active TB
IMPUTED_sample <-
  IMPUTED_sample %>%
  left_join(cfr_age_lookup,
            by = c("agegroup_all_notif" = "age")) %>%
  dplyr::select(-distn, -a, -b)


# sample treatment delays
symptoms_to_Tx <-
  replicate(sum(IMPUTED_sample$all_tb),
            sample_distributions(treatment_delay)) %>%
  unlist()

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(
    symptoms_to_Tx = ifelse(all_tb,
                            yes = symptoms_to_Tx,
                            no = NA))

# time intervals for utilities
IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(
    symptoms_to_Tx = symptoms_to_Tx,
    LTBI_to_symptoms = all_tb_issdt - symptoms_to_Tx,
    Tx_to_cured = 1,
    cured_to_death = date_death1_issdt.years - all_death_notif - Tx_to_cured)


# calculate QALYs for all tb cases for all outcomes
QALY_all_tb <-
  IMPUTED_sample %>%
  subset(all_tb == TRUE) %$%
  calc_QALY_tb(
    intervals = data.frame(symptoms_to_Tx,
                           Tx_to_cured,
                           cured_to_death),
    utility = utility,
    age = age_all_notification,
    start_delay = LTBI_to_symptoms,
    discount_rate = interv$discount_rate,
    utility_method = "add"
  )

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(
    tb_fatality = ifelse(test = is.na(cfr),
                         yes = NA,
                         no = runif(n = n()) < cfr),
    QALY_fatality = ifelse(test = all_tb,
                           yes = QALY_all_tb$fatality,
                           no = NA),
    QALY_diseasefree = ifelse(test = all_tb,
                              yes = QALY_all_tb$diseasefree,
                              no = NA),
    QALY_cured = ifelse(test = all_tb,
                        yes = QALY_all_tb$cured,
                        no = NA),
    QALY_statusquo = ifelse(test = tb_fatality,
                            yes = QALY_fatality,
                            no = QALY_cured))

# future discounts for costs of active TB cases
max_tb_issdt <-
  with(IMPUTED_sample,
       all_tb_issdt[is.finite(all_tb_issdt)] %>%
         max(na.rm = TRUE))

ydiscounts <-
  QALY::discount(t_limit = max_tb_issdt + 1,
                 discount_rate = interv$discount_rate)

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(
    uk_notif_issdt = ceiling(notif_issdt.years),
    all_notif_issdt = ceiling(all_tb_issdt),
    uk_notif_discounts = ydiscounts[uk_notif_issdt],
    all_notif_discounts = ydiscounts[all_notif_issdt],
    uk_secondary_inf_discounts = ydiscounts[uk_notif_issdt + 1],
    all_secondary_inf_discounts = ydiscounts[all_notif_issdt + 1])


num_2nd_inf <-
  replicate(sum(IMPUTED_sample$all_tb),
            sample_distributions(NUM_SECONDARY_INF)) %>%
  sapply(FUN = rbernoulli, n = 1)


IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(
    num_2nd_inf =
      ifelse(all_tb,
             yes = num_2nd_inf,
             no = NA_real_))


save(IMPUTED_sample, file = "data/model_input_cohort.RData")

