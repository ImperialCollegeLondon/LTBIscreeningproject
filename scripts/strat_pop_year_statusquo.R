# *****************************************************
# LTBI screening
# N Green
# 2018
#
# gridded plots of the subpopulation sizes over time
# can do this on the raw, fitted, extrapolated or subsample data

# tb is for cases in EWNI only
# status-quo

event_times <-
  list(
    # tb = cohort$exituk_tb.years,
    # exit_uk = rep(1000, nrow(cohort)),
    tb = cohort$rNotificationDate_issdt.years,
    cohort$date_exit_uk1_issdt.years,
    death = cohort$date_death1_issdt.years)

strat_pop_year <- count_comprsk_events(event_times)


#########
# plots #
#########

filename <- paste(plots_folder_scenario, "time-to-event-grid-plot.png", sep = "/")
png(filename, width = 400, height = 350, res = 45)

par(mfrow = c(2,2))

strat_plot <- pryr::partial(plot,
                            type = "s", xlim = c(0,20), xlab = "Year")

strat_plot(unlist(strat_pop_year["tb", ]),
           ylim = c(0,500), ylab = "active TB")

strat_plot(unlist(strat_pop_year["death", ]),
           ylab = "all-cause death")

strat_plot(unlist(strat_pop_year["exit_uk", ]),
           ylim = c(0, max(unlist(strat_pop_year["exit_uk", ]))), ylab = "exit EWNI")

strat_plot(unlist(strat_pop_year["at-risk", ]),
           ylab = "remain in EWNI")

dev.off()


##########
# tables #
##########

strat_pop_year <-
  t(strat_pop_year) %>%
  data.frame(row.names = NULL) %>%
  mutate(tb_diff = diff(c(0, tb)),
         discount = discount(t_limit = n()),
         c_tb = means$cost.aTB_TxDx + (means$num_sec_inf * means$cost.aTB_TxDx)/1.035,
         t_tb = tb_diff * c_tb,
         dis_tb = discount * t_tb)

write.csv(res,
          file = pastef(diroutput, 'num-competing-events-by-year_statusquo.csv'))

