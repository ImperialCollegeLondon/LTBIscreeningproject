

number <- NA
for (i in 1:1000) {

  number <- c(number,
                  sample_tb(prob = 1 - IMPUTED_sample_year_cohort$pLTBI) %>%
                    sum())
}

hist(number, breaks = 30, main = "")

proportion <- number/nrow(IMPUTED_sample_year_cohort)
hist(proportion, breaks = 30, main = "")



#  ------------------------------------------------------------------------

number <- NA
for (i in 1:1000) {

  IMPUTED_sample$screen_year <- runif(n = nrow(IMPUTED_sample))*5

  IMPUTED_sample %<>%
    mutate(screen = ifelse(date_death1_issdt.years >= screen_year &
                             date_exit_uk1_issdt.years >= screen_year &
                             (rNotificationDate_issdt.years >= screen_year | is.na(rNotificationDate_issdt.years)), 1, 0))

  number <- c(number, IMPUTED_sample$screen %>% sum())
}

hist(number, breaks = 30, main = "")

proportion <- number/nrow(IMPUTED_sample)
hist(proportion, breaks = 30, main = "")


#  ------------------------------------------------------------------------


# expected number of LTBI to start screening pathway
x <- aggregate(x = IMPUTED_sample$pLTBI,
          by = list(IMPUTED_sample$issdt_year),
          sum) %>%
  set_names(c("year", "LTBI"))


probs <- p_complete_screen_lookup$prob[p_complete_screen_lookup$who_prev_cat_Pareek2011 == "(350,1e+05]"]

tab <- merge(x, probs) %>%
  mutate(scenario = rep(1:14, each = 8),
         num_avoid = round(probs * LTBI, digits = 0)) %>%
  dcast(year ~ scenario, value.var = "num_avoid")










