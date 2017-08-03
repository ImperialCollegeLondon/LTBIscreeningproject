#
# project: LTBI screening
# N Green
# April 2017
#
# impute missing/unobserved values



# individually SIMULATE active TB progression times after exit uk ----------------

IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(exituk_tb.years = sim_exituk_tb_times(data = .,
                                               prob = year_prob.activetb_cens_exituk),
         exituk_tb = !is.na(exituk_tb.years) &
           !is.infinite(exituk_tb.years))

table(IMPUTED_sample_year_cohort$exituk_tb.years, useNA = "always")

n.exit_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(exituk_tb) %>%
  dplyr::count()


# # _proportion_ calc for exit_uk tb ------------------------------------------
#
# prob_year <- list()
# maxyear_exituk <- 10
#
# for (n in seq_len(maxyear_exituk)) {
#
#   prob <-
#     c(year_prob.activetb_cens_exituk,
#       1 - sum(year_prob.activetb_cens_exituk)) %>%
#     unname()
#
#   prob[1:n] <- 0
#   prob <- prob/sum(prob)
#   prob <- rm_last(prob)
#
#   prob_year[[n]] <- round(LTBI_ukexit_year_pop[n + 1] * prob)
# }
#
# # sum across all (exit year) curves
# num_exituk_tb_year <- prob_year %>% reduce(`+`)
#
# n.exit_tb <- sum(num_exituk_tb_year)



# individually SIMULATE active TB progression times uk tb after followup ----------------------

IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(rNotificationDate_issdt.years = sim_uktb_times(data = .,
                                                        prob = year_prob.activetb_cmprsk_exituk),
         uk_tb = !is.na(rNotificationDate_issdt.years) &
           !is.infinite(rNotificationDate_issdt.years))

table(
  round(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years), useNA = "always")

n.uk_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(uk_tb) %>%
  dplyr::count()


# calc number tb_uk (extrapolated) using _proportions_ -----------------------------
# straight forward, direct method
#
# num_uk_tb_year <- pop_year * year_prob.activetb_cmprsk_exituk * 0.3
#
# n.uk_tb <- sum(num_uk_tb_year)
# num_all_tb_year <- num_exituk_tb_year + num_uk_tb_year


# include a year 0 baseline
strat_pop_year <- cbind(c(0, 0, 0, 0, pop_year),
                        strat_pop_year)

