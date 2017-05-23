

# 100% fatality cumulative active TB QALY loss
sapply(QALY_diseasefree,
       FUN = function(x) attr(x, "yearly_QALYs")) %>%
  rowSums(na.rm = TRUE) %>%
  cumsum %>%
  plot(type = 'l')


plot(NULL, xlim = c(0, 50), ylim = c(0, 500),
     xlab = "Time", ylab = "Total cumulative QALYs")
abline(h = sum(E_fatality_QALYloss), lwd = 2)

for (i in 1:100) {

  # TRUE if death due to active TB
  IMPUTED_sample_year_cohort <-
    IMPUTED_sample_year_cohort %>%
    mutate(fatality = runif(n()) < cfr)

  QALYloss_diseasefree <-
    IMPUTED_sample_year_cohort %>%
    subset(fatality == TRUE) %>%
    with(.,
         map2(.x = age_all_notification,
              .y = all_death_rNotificationDate,
              .f = adjusted_life_years,
              start_year = 0,
              end_year = NA,
              utility = utility$disease_free,
              discount_rate = 0.035)) %>%
    map(total_QALYs)

  sapply(QALYloss_diseasefree,
         FUN = function(x) attr(x, "yearly_QALYs")) %>%
    rowSums(na.rm = TRUE) %>%
    cumsum %>%
    lines(type = 'l', col = rgb(0, 0, 0, 0.2))
}
