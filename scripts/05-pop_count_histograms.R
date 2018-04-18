#*******************************************
#
# LTBI screening
# N Green
#
# histograms of sampled input values in model


# LTBI --------------------------------------------------------------------

number <- vector(length = 1000)
for (i in 1:1000) {

  number[i] <-
    sample_tb(prob = 1 - cohort$pLTBI) %>%
    sum()
}

hist(number, breaks = 30, main = "", xlab = "Number LTBI in cohort")

proportion <- number/nrow(cohort)
hist(proportion, breaks = 30, main = "", xlab = "Proportion LTBI in cohort")


#  screening eligible by time -----------------------------------------------

number <- vector(length = 1000)
for (i in 1:1000) {

  # 5 year window
  IMPUTED_sample$screen_year <- runif(n = nrow(IMPUTED_sample))*5

  IMPUTED_sample %<>%
    dplyr::mutate(screen = ifelse(date_death1_issdt.years >= screen_year &
                                    date_exit_uk1_issdt.years >= screen_year &
                                    (rNotificationDate_issdt.years >= screen_year | is.na(rNotificationDate_issdt.years)), 1, 0))

  number[i] <- IMPUTED_sample$screen %>% sum()
}

hist(number, breaks = 30, main = "", xlab = "Number offered screening w/ Unif[0,5] registration time")

proportion <- number/nrow(IMPUTED_sample)
hist(proportion, breaks = 30, main = "", xlab = "Proportion offered screening w/ Unif[0,5] registration time")


#  expected number of LTBI to start screening pathway ---------------------
##TODO:

x <-
  aggregate(x = IMPUTED_sample$pLTBI,
          by = list(IMPUTED_sample$issdt_year),
          sum) %>%
  set_names(c("year", "LTBI"))

probs <- p_complete_screen_lookup$prob[p_complete_screen_lookup$who_prev_cat_Pareek2011 == "(350,1e+05]"]

tab <-
  merge(x, probs) %>%
  dplyr::mutate(scenario = rep(1:14, each = 8),
                num_avoid = round(probs * LTBI, digits = 0)) %>%
  dcast(year ~ scenario, value.var = "num_avoid")



