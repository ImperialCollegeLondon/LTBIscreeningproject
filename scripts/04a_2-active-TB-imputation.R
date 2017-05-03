#
# project: LTBI screening
# N Green
#



# year 0 baseline
strat_pop_year <- cbind(c(0, 0, 0, 0, pop_year),
                        strat_pop_year)


# calc number of LTBI in exit uk pop each year ---------------------------
##TODO: not quite as good as individual level calc cos fixed prob LTBI

PROB_LTBI <- 0.3   #fixed probability of LTBI

LTBI_ukexit_year_pop <-
  strat_pop_year["exit_uk", ] %>%
  diff() %>%
  c(strat_pop_year["exit_uk", 1], .) %>%
  multiply_by(PROB_LTBI)


# remove trailing 0's
LTBI_ukexit_year_pop <- remove_duplicates(LTBI_ukexit_year_pop)


# simulate active TB progression times after exit uk _for_each_individual_  ----------------------

IMPUTED_sample_year_cohort$exituk_tb_year <- sim_aTB_times(pop = pop_year,
                                                           data = IMPUTED_sample_year_cohort,
                                                           prob = year_prob.activetb_cens_exituk)


# count number deaths & active TB cases in each exit uk year group ---------------------

strat_exit_year <- list()
exit_max_year <- 10


for (yeari in seq_len(exit_max_year)) {

  # single year cohort
  # exit in yeari and exit first event (before death, active tb, followup censoring)

  cohort_subset <-
    IMPUTED_sample_year_cohort %>%
    dplyr::filter((yeari - 1) < date_exit_uk1_issdt.years,
                  date_exit_uk1_issdt.years < yeari,
                  exit_uk1 == TRUE)

  strat_exit_year[[yeari]] <-
    list(tb = cohort_subset$exituk_tb_year,
         death = cohort_subset$date_death1_issdt.years) %>%
    count_comprsk_events()
}

# remove entries after at-risk pop = 0
# strat_exit_year <- sapply(strat_exit_year,
#                           function(x) x[, x["at-risk", ]!=0])


# sum across all curves for each year
num_exituk_tb_year <- rowSums(sapply(strat_exit_year,
                                     function(x) diff(x["tb", ])))

n.exit_tb <- sum(num_exituk_tb_year, na.rm = TRUE)


# plot(num_exituk_tb_year, type = "o")



##TODO: do the same as above so that each _individual_ has an extrapolated progression time
##TODO: do the above as _proportions_


# calc number tb_uk (extrapolated) using trans probs  -----------------------------


num_uk_tb_year <- pop_year * year_prob.activetb_cens_exituk * PROB_LTBI

notifDate_issdt.years <- remove_duplicates(strat_pop_year["tb", ])
obs_uk_tb_year <- diff(c(0, notifDate_issdt.years))


plot(x = 0:19, y = num_uk_tb_year[1:20],
     type = "o", ylim = c(0, 85),
     xlab = "Year", ylab = "Frequency",
     main = "Active TB cases in EWNI for\n observed (red)\n and estimated (black)")
lines(x = obs_uk_tb_year,
      type = "o",
      col = "red")

num_all_tb_year <- num_exituk_tb_year + num_uk_tb_year

