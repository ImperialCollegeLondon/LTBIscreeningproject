

sapply(QALY_diseasefree,
       FUN = function(x) attr(x, "yearly_QALYs")) %>%
  rowSums(na.rm = TRUE) %>%
  cumsum %>%
  plot(type = 'l',
       main = "100% fatality cumulative active TB QALY loss",
       xlab = "Time")

num_scenarios <- length(unique(p_complete_screen_lookup$scenario))

x11()
par(mfrow = c(4,4))

for (SCENARIO in seq_len(num_scenarios)) {

  plot(NULL, xlim = c(0, 50), ylim = c(0, 105),
       xlab = "Time", ylab = "Total cumulative QALYs",
       main = paste("Scenario", SCENARIO))

  screen_prob_scenario <-
    p_complete_screen_lookup %>%
    subset(who_prev_cat_Pareek2011 == "(350,1e+05]" &
             scenario == SCENARIO,
           select = prob)

  abline(h = E_total_fatality_QALYloss * screen_prob_scenario, lwd = 2)

  IMPUTED_sample_scenarios <-
    IMPUTED_sample_year_cohort %>%
    left_join(p_complete_screen_lookup,
              by = "who_prev_cat_Pareek2011")

  for (i in 1:30) {

    try(plot_dat <-
      IMPUTED_sample_scenarios %>%
        mutate(fatality = runif(n()) < cfr,
               screen_success = runif(n()) < prob) %>%
        dplyr::filter(fatality == TRUE,
                      scenario == SCENARIO,
                      screen_success == TRUE) %>%
        with(.,
             map2(.x = age_all_notification,
                  .y = all_death_rNotificationDate,
                  .f = QALY::adjusted_life_years,
                  start_year = 0,
                  end_year = NA,
                  utility = utility$disease_free,
                  discount_rate = 0.035)) %>%
        map(total_QALYs) %>%
        sapply(.,
               FUN = function(x) attr(x, "yearly_QALYs")) %>%
        rowSums(na.rm = TRUE) %>%
        cumsum)

        lines(plot_dat, type = 'l', col = rgb(0, 0, 0, 0.2))
  }

  text(10, 100, labels = paste("Cases averted:", round(num_all_tb_QALY * screen_prob_scenario, 2)))
  text(10, 90,  labels = paste("Fatalities averted:", round(E_total_fatalities * screen_prob_scenario, 2)))
  text(10, 80,  labels = paste("Cost averted:", round(sum(sample_distributions(unit_cost$aTB_TxDx)) * num_all_tb_cost * screen_prob_scenario)))
}
