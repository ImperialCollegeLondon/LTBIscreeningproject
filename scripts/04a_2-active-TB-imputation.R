#
# project: LTBI screening
# N Green
# April 2017
#
# impute missing/unobserved values


# include a year 0 baseline
strat_pop_year <- cbind(c(0, 0, 0, 0, pop_year),
                        strat_pop_year)


# calc number of LTBI in exit uk pop each year ---------------------------

##TODO: not quite as good as individual level calc cos fixed prob LTBI

PROB_LTBI <- 0.3   #i.e. fixed

LTBI_ukexit_year_pop <-
  strat_pop_year["exit_uk", ] %>%
  diff() %>%
  c(strat_pop_year["exit_uk", 1], .) %>%
  multiply_by(PROB_LTBI)

# remove trailing 0's
LTBI_ukexit_year_pop <- remove_duplicates(LTBI_ukexit_year_pop)


# individually SIMULATE active TB progression times after exit uk ----------------

IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(exituk_tb.years = sim_exituk_tb_times(data = .,
                                               prob = year_prob.activetb_cens_exituk),
         exituk_tb = !is.na(exituk_tb.years) &
                      !is.infinite(exituk_tb.years))

table(IMPUTED_sample_year_cohort$exituk_tb.years, useNA = "always")


## split by exit year
# strat_exit_year <- subpop_by_exituk_year(IMPUTED_sample_year_cohort)
#
# # sum across all curves for each year
# num_exituk_tb_year <- rowSums(sapply(strat_exit_year,
#                                      function(x) diff(x["tb", ])))
#
# n.exit_tb <- sum(num_exituk_tb_year, na.rm = TRUE)


# _proportion_ calc for exit_uk tb ------------------------------------------

prob_year <- list()
maxyear_exituk <- 10

for (n in seq_len(maxyear_exituk)) {

  prob <-
    c(year_prob.activetb_cens_exituk,
      1 - sum(year_prob.activetb_cens_exituk)) %>%
    unname()

  prob[1:n] <- 0
  prob <- prob/sum(prob)
  prob <- rm_last(prob)

  prob_year[[n]] <- round(LTBI_ukexit_year_pop[n + 1] * prob)
}

# sum across all (exit year) curves
num_exituk_tb_year <- prob_year %>% reduce(`+`)


## use sampled values for C-E calcs
## rather than expected values
## WHY?...
## cos individual better?

# n.exit_tb <- sum(num_exituk_tb_year)
n.exit_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(exituk_tb) %>%
  dplyr::count()


# individually SIMULATE active TB progression times uk tb after followup ----------------------

IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(rNotificationDate_issdt.years = sim_uktb_times(data = .,
                                                        prob = year_prob.activetb_cmprsk_exituk),
         uk_tb = !is.na(rNotificationDate_issdt.years) &
                 !is.infinite(rNotificationDate_issdt.years))

table(
  round(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years), useNA = "always")


# calc number tb_uk (extrapolated) using _proportions_ -----------------------------
# straight forward, direct method

# num_uk_tb_year <- pop_year * year_prob.activetb_cmprsk_exituk * PROB_LTBI

# n.uk_tb <- sum(num_uk_tb_year)
n.uk_tb <-
  IMPUTED_sample_year_cohort %>%
  dplyr::filter(uk_tb) %>%
  dplyr::count()


# num_all_tb_year <- num_exituk_tb_year + num_uk_tb_year


notifDate_issdt.years <- remove_duplicates(strat_pop_year["tb", ])

num_uk_tb_orig_year <-
  c(0, notifDate_issdt.years) %>%
  diff()

