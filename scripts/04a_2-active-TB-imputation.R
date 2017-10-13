#******************************************
# project: LTBI screening
# N Green
# April 2017
#
# impute missing/unobserved time to active tb

# prob: scaled progression probs with IMPUTED_sample weighted average LTBI prob


# individually SIMULATE active TB progression times after exit uk and followup -------------------

IMPUTED_sample <-
  IMPUTED_sample %>%
  dplyr::mutate(exituk_tb.years = sim_exituk_tb_times(data = .,
                                                      prob = year_prob.activetb_cens_exituk/0.278),
                rNotificationDate_issdt.years = sim_uktb_times(data = .,
                                                               prob = year_prob.activetb_cmprsk_exituk/0.278),
                exituk_tb = !is.na(exituk_tb.years) &
                            !is.infinite(exituk_tb.years),
                uk_tb = !is.na(rNotificationDate_issdt.years) &
                        !is.infinite(rNotificationDate_issdt.years))


IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  dplyr::mutate(exituk_tb.years = sim_exituk_tb_times(data = .,
                                                      prob = year_prob.activetb_cens_exituk/0.278),
                rNotificationDate_issdt.years = sim_uktb_times(data = .,
                                                               prob = year_prob.activetb_cmprsk_exituk/0.278),
                exituk_tb = !is.na(exituk_tb.years) &
                  !is.infinite(exituk_tb.years),
                uk_tb = !is.na(rNotificationDate_issdt.years) &
                  !is.infinite(rNotificationDate_issdt.years))
