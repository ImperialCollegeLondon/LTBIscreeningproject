# ************************************
# LTBI screening
# N Green
#
# include new tb event and update dependencies


# is someone actually screened?
IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(screened_before_exit = date_exit_uk1_issdt.years >= screen_year,
                screened_before_tb = (rNotificationDate_issdt.years >= screen_year) | is.na(rNotificationDate_issdt.years),
                screened_before_death = date_death1_issdt.years >= screen_year,
                screen = ifelse(screened_before_death &
                                screened_before_exit &
                                screened_before_tb,
                                yes = 1, no = 0))


# combine (imputed) exit_uk tb and uk_tb data ------------------------------------------

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(all_tb = uk_tb | exituk_tb,
                all_tb_issdt = ifelse(uk_tb,
                                      rNotificationDate_issdt.years,
                                      exituk_tb.years),

                # progression to death times
                uk_death_rNotificationDate = date_death1_issdt.years - ifelse(test = is.infinite(rNotificationDate_issdt.years),
                                                                              yes = NA,
                                                                              no = rNotificationDate_issdt.years),
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


# calculate QALYs for all tb cases for all outcomes
# so can sample later
QALY_all_tb <-
  IMPUTED_sample %>%
  subset(all_tb == TRUE) %$%
  calc_QALY_tb(timetoevent = all_death_rNotificationDate,
               utility.disease_free = utility$disease_free,
               utility.case = utility$activeTB,
               age = age_all_notification,
               start_delay = all_tb_issdt)


# case fatality rate for each active TB case

IMPUTED_sample <-
  IMPUTED_sample %>%
  left_join(cfr_age_lookup,
            by = c("agegroup_all_notification" = "age")) %>%
  dplyr::select(-distn, -a, -b)

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(tb_fatality = ifelse(test = is.na(cfr),
                                     yes = NA,
                                     no = runif(n = n()) < cfr),
                QALY_fatality = ifelse(test = all_tb,
                                       yes = QALY_all_tb$fatality,
                                       no = NA),
                QALY_diseasefree = ifelse(test = all_tb,
                                          yes = QALY_all_tb$diseasefree,
                                          no = NA),
                QALY_cured = ifelse(test = all_tb,
                                    yes = QALY_all_tb$diseasefree,
                                    no = NA),
                QALY_statusquo = ifelse(test = tb_fatality,
                                        yes = QALY_fatality,
                                        no = QALY_cured))


# future discounts for costs of active TB cases

max_tb_issdt <-
  with(IMPUTED_sample,
       all_tb_issdt[is.finite(all_tb_issdt)] %>%
         max(na.rm = TRUE))

ydiscounts <- QALY::discount(t_limit = max_tb_issdt + 1)

IMPUTED_sample <-
  IMPUTED_sample %>%
  mutate(uk_notif_issdt = ceiling(rNotificationDate_issdt.years),
         all_notif_issdt = ceiling(all_tb_issdt),
         uk_notif_discounts = ydiscounts[uk_notif_issdt],
         all_notif_discounts = ydiscounts[all_notif_issdt],
         uk_secondary_inf_discounts = ydiscounts[uk_notif_issdt + 1],
         all_secondary_inf_discounts = ydiscounts[all_notif_issdt + 1])

