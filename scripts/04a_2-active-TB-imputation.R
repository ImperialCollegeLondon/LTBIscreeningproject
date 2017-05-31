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


# individually SIMULATE active TB progression times after exit uk ----------------------

IMPUTED_sample_year_cohort$exituk_tb.years <-
  sim_aTB_times(data = IMPUTED_sample_year_cohort,
                prob = year_prob.activetb_cens_exituk)

# active tb status
IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(exituk_tb = !is.na(exituk_tb.years) &
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
  count()


# multiple samples
# for (i in 1:5) {
#
#   exituk_tb.years <- sim_aTB_times(pop = pop_year,
#                                    data = IMPUTED_sample_year_cohort,
#                                    prob = year_prob.activetb_cens_exituk)
#
#   x <- hist(exituk_tb.years,
#             breaks = 100, xlim = c(0, 20), ylim = c(0,25),
#             xlab = "year", main = "", plot = F)
#
#   points(x$mids[x$counts > 0] + rnorm(sum(x$counts > 0), 0, 0.5),
#          x$counts[x$counts > 0] + rnorm(sum(x$counts > 0), 0, 0.5),
#          pch = 16, col = rgb(0, 0, 0, 0.3))
# }



# individually SIMULATE active TB progression times uk tb after followup ----------------------

IMPUTED_sample_year_cohort <-
  IMPUTED_sample_year_cohort %>%
  mutate(issdt.asnumeric = issdt - as.Date("1960-01-01"),
         fup_issdt_days = fup1 - issdt.asnumeric,
         fup_issdt = days_to_years(fup_issdt_days),
         rNotificationDate_issdt.years = sim_uktb_times(data = .,
                                                        prob = year_prob.activetb_cmprsk_exituk),
         age_uk_notification = age_at_entry + rNotificationDate_issdt.years,
         agegroup_uk_notification = cut(age_uk_notification,
                                        breaks = cfr_age_breaks,
                                        right = FALSE),
         uk_tb = !is.na(rNotificationDate_issdt.years) &
                 !is.infinite(rNotificationDate_issdt.years))

table(
  round(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years), useNA = "always")


# calc number tb_uk (extrapolated) using _proportions_ -----------------------------
# straight forward, direct method

num_uk_tb_year <- pop_year * year_prob.activetb_cmprsk_exituk * PROB_LTBI

num_all_tb_year <- num_exituk_tb_year + num_uk_tb_year


notifDate_issdt.years <- remove_duplicates(strat_pop_year["tb", ])

obs_uk_tb_year <-
  c(0, notifDate_issdt.years) %>%
  diff()




# plots --------------------------------------------------------------------

hist(IMPUTED_sample_year_cohort$exituk_tb.years,
     breaks = 50, xlim = c(0, 50), ylim = c(0,25),
     xlab = "year", main = "")

lines(num_exituk_tb_year, type = 'l', xlim = c(0, 20),
      col = "red") #rgb(0, 0, 0, 0.5))

plot(x = 0:19,
     y = num_uk_tb_year[1:20],
     type = "o", ylim = c(0, 85),
     lty = 1,
     xlab = "Time (year)", ylab = "Frequency",
     main = "Active TB cases")

# lines(x = seq_along(obs_uk_tb_year) - 1,
#       y = obs_uk_tb_year,
#       type = "o",
#       col = "red")

lines(x = seq_along(num_exituk_tb_year) - 1,
      y = num_exituk_tb_year,
      col = "green",
      lty = 2,
      type = 'o')

lines(x = seq_along(num_all_tb_year) - 1,
      y = num_all_tb_year,
      col = "blue",
      lty = 3,
      type = 'o')

legend("topright",
       # legend = c("Observed", "Estimated uk", "Estimated exit", "Total"),
       # col = c("red", "black", "green", "blue"), lty = 1)
legend = c("Estimated uk", "Estimated exit", "Total"),
col = c("black", "green", "blue"), lty = c(1,2,3))


