# *****************************************************
# LTBI screening
# N Green
# 2018
# strat_pop_year_statusquo.R
#
# gridded plots of the subpopulation sizes over time
# can do this on the raw, fitted, extrapolated or subsample data
#
# tb is for cases in EWNI only


# subset by covariate?
cohort <-
  IMPUTED_sample %>%
  # subset(IMPUTED_sample$who_inc_Pareek2011 %in% "(150,250]")
  # subset(IMPUTED_sample$who_inc_Pareek2011 %in% "(250,350]")
  subset(IMPUTED_sample$who_inc_Pareek2011 %in% "(350,1e+05]")

# sample fixed size cohort?
cohort <- cohort[sample.int(n = nrow(cohort),
                            size = 100000,
                            replace = TRUE), ]

event_times <-
  list(
    tb = cohort$rNotificationDate_issdt.years,
    exit_uk = cohort$date_exit_uk1_issdt.years,
    death = cohort$date_death1_issdt.years)


strat_pop_year <- count_comprsk_events(event_times)


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

plot('incidence', type = 'n', xlim = c(0, 100), ylim = c(0, 150))
lines(strat_pop_year$tb_diff, type = 'o')
lines(strat_pop_year$tb_diff, type = 'o', col = "blue")
lines(strat_pop_year$tb_diff, type = 'o', col = "red")
legend('topright', legend = c("(150,250]","(250,350]","(350,1e+05]"), col = c("black","blue","red"), lty = 1)


# smoothed fit
lo <- loess(tb_diff~year, strat_pop_year, span = 0.1, degree = 2)
lines(predict(lo), col = 'black', lwd = 2, type = 'l')
lines(predict(lo), col = 'blue', lwd = 2, type = 'l')
lines(predict(lo), col = 'red', lwd = 2, type = 'l')


hist(cohort$rNotificationDate_issdt.years, breaks = 50)












write.csv(res,
          file = pastef(diroutput, 'num-competing-events-by-year_statusquo.csv'))



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
