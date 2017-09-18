# ************************************
# LTBI screening
# N Green
#
# include new tb event and update dependencies


# combine (imputed) exit_uk tb and uk tb data ------------------------------------------

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(all_tb = uk_tb | exituk_tb,
                all_tb_issdt = ifelse(uk_tb,
                                      rNotificationDate_issdt.years,
                                      exituk_tb.years),

                # progression to death times
                uk_death_rNotificationDate = date_death1_issdt.years - rNotificationDate_issdt.years,
                all_death_rNotificationDate = date_death1_issdt.years - all_tb_issdt,

                # progression ages
                age_uk_notification = age_at_entry + rNotificationDate_issdt.years,
                age_exituk_notification = age_at_entry + exituk_tb.years,
                age_all_notification = ifelse(uk_tb,
                                              age_uk_notification,
                                              age_exituk_notification),
                # progression age groups
                agegroup_uk_notification = cut(age_uk_notification,
                                               breaks = cfr_age_breaks,
                                               right = FALSE),
                agegroup_all_notification = cut(age_all_notification,
                                                breaks = cfr_age_breaks,
                                                right = FALSE))


# case fatality rate for each active TB case

IMPUTED_sample <-
  IMPUTED_sample %>%
  left_join(cfr_age_lookup,
            by = c("agegroup_all_notification" = "age")) %>%
  dplyr::select(-distn, -a, -b)

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(tb_fatality = ifelse(is.na(cfr),
                                     NA,
                                     runif(n = n()) < cfr))


# calculate QALYs for all tb cases for all outcomes
# so can sample later
QALY_all_tb <-
  IMPUTED_sample %>%
  subset(all_tb == TRUE) %$%
  calc_QALY_tb(timetoevent = pmax(0.5, all_death_rNotificationDate), #assume at least 6 month between progression and all-cause death
               utility.disease_free = utility$disease_free,
               utility.case = utility$activeTB,
               age = age_all_notification)

IMPUTED_sample$QALY_fatality[IMPUTED_sample$all_tb == TRUE] <- QALY_all_tb$fatality
IMPUTED_sample$QALY_diseasefree[IMPUTED_sample$all_tb == TRUE] <- QALY_all_tb$diseasefree
IMPUTED_sample$QALY_cured[IMPUTED_sample$all_tb == TRUE] <- QALY_all_tb$cured

