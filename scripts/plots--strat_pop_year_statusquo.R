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


SCREENED <- c(0,1)
# SCREENED <- 1

# subset by covariate?
cohort1 <-
  IMPUTED_sample %>%
  dplyr::filter(IMPUTED_sample$who_inc_Pareek2011 %in% "(150,250]",
                screen %in% SCREENED)
cohort2 <-
  IMPUTED_sample %>%
  dplyr::filter(IMPUTED_sample$who_inc_Pareek2011 %in% "(250,350]",
                screen %in% SCREENED)
cohort3 <-
  IMPUTED_sample %>%
  dplyr::filter(IMPUTED_sample$who_inc_Pareek2011 %in% "(350,1e+05]",
                screen %in% SCREENED)

# sample fixed size cohort?
# cohort1 <- cohort1[sample.int(n = nrow(cohort1),
#                               size = 100000,
#                               replace = TRUE), ]
# cohort2 <- cohort2[sample.int(n = nrow(cohort2),
#                               size = 100000,
#                               replace = TRUE), ]
# cohort3 <- cohort3[sample.int(n = nrow(cohort3),
#                               size = 100000,
#                               replace = TRUE), ]

event_times1 <-
  list(
    tb = cohort1$rNotificationDate_issdt.years,
    exit_uk = cohort1$date_exit_uk1_issdt.years,
    death = cohort1$date_death1_issdt.years)
event_times2 <-
  list(
    tb = cohort2$rNotificationDate_issdt.years,
    exit_uk = cohort2$date_exit_uk1_issdt.years,
    death = cohort2$date_death1_issdt.years)
event_times3 <-
  list(
    tb = cohort3$rNotificationDate_issdt.years,
    exit_uk = cohort3$date_exit_uk1_issdt.years,
    death = cohort3$date_death1_issdt.years)


strat_pop_year1 <- count_comprsk_events(event_times1)
strat_pop_year2 <- count_comprsk_events(event_times2)
strat_pop_year3 <- count_comprsk_events(event_times3)


##########
# tables #
##########

strat_pop_year1 <-
  t(strat_pop_year1) %>%
  data.frame(row.names = NULL) %>%
  mutate(tb_diff = diff(c(0, tb)),
         discount = discount(t_limit = n()),
         c_tb = means$cost.aTB_TxDx + (means$num_sec_inf * means$cost.aTB_TxDx)/1.035,
         t_tb = tb_diff * c_tb,
         dis_tb = discount * t_tb)
strat_pop_year2 <-
  t(strat_pop_year2) %>%
  data.frame(row.names = NULL) %>%
  mutate(tb_diff = diff(c(0, tb)),
         discount = discount(t_limit = n()),
         c_tb = means$cost.aTB_TxDx + (means$num_sec_inf * means$cost.aTB_TxDx)/1.035,
         t_tb = tb_diff * c_tb,
         dis_tb = discount * t_tb)
strat_pop_year3 <-
  t(strat_pop_year3) %>%
  data.frame(row.names = NULL) %>%
  mutate(tb_diff = diff(c(0, tb)),
         discount = discount(t_limit = n()),
         c_tb = means$cost.aTB_TxDx + (means$num_sec_inf * means$cost.aTB_TxDx)/1.035,
         t_tb = tb_diff * c_tb,
         dis_tb = discount * t_tb)


# write.csv(res,
#           file = pastef(diroutput, 'num-competing-events-by-year_statusquo.csv'))


#########
# plots #
#########


# incidence ---------------------------------------------------------------

plot('incidence', type = 'n', xlim = c(0, 100), ylim = c(0, 1000))
lines(strat_pop_year1$tb_diff, type = 'o')
lines(strat_pop_year2$tb_diff, type = 'o', col = "blue")
lines(strat_pop_year3$tb_diff, type = 'o', col = "red")
lines(strat_pop_year1$tb_diff + strat_pop_year2$tb_diff + strat_pop_year3$tb_diff, type = 'o', col = "green")
lines(2:(length(p_incid_year) + 1), p_incid_year*nrow(IMPUTED_sample), type = 'o', col = "magenta")
legend('topright',
       legend = c("(150,250]","(250,350]","(350,1e+05]","combined","Aldridge"),
       col = c("black","blue","red","green","magenta"), lty = 1)


# smoothed fit
lo1 <- loess(tb_diff~year, strat_pop_year1, span = 0.1, degree = 2)
lo2 <- loess(tb_diff~year, strat_pop_year2, span = 0.1, degree = 2)
lo3 <- loess(tb_diff~year, strat_pop_year3, span = 0.1, degree = 2)
lines(predict(lo1), col = 'black', lwd = 2, type = 'l')
lines(predict(lo2), col = 'blue', lwd = 2, type = 'l')
lines(predict(lo3), col = 'red', lwd = 2, type = 'l')


# cumulative incidence -----------------------------------------------------

plot('incidence', type = 'n', xlim = c(0, 100), ylim = c(0, 5000))
lines(cumsum(strat_pop_year1$tb_diff), type = 'o')
lines(cumsum(strat_pop_year2$tb_diff), type = 'o', col = "blue")
lines(cumsum(strat_pop_year3$tb_diff), type = 'o', col = "red")
lines(cumsum(strat_pop_year1$tb_diff) + cumsum(strat_pop_year2$tb_diff) + cumsum(strat_pop_year3$tb_diff), type = 'o', col = "green")
lines(2:(length(p_incid_year) + 1), cumsum(p_incid_year*nrow(IMPUTED_sample)), type = 'o', col = "magenta")
legend('topright',
       legend = c("(150,250]","(250,350]","(350,1e+05]","combined","Aldridge"),
       col = c("black","blue","red","green","magenta"), lty = 1)


# log-incidence -----------------------------------------------------------

strat_pop_year1$tb_diff_log <- log(strat_pop_year1$tb_diff)
strat_pop_year2$tb_diff_log <- log(strat_pop_year2$tb_diff)
strat_pop_year3$tb_diff_log <- log(strat_pop_year3$tb_diff)

strat_pop_year1$tb_diff_log[strat_pop_year1$tb_diff_log < 0] <- NA
strat_pop_year2$tb_diff_log[strat_pop_year2$tb_diff_log < 0] <- NA
strat_pop_year3$tb_diff_log[strat_pop_year3$tb_diff_log < 0] <- NA

plot('log-incidence', type = 'n', xlim = c(0, 100), ylim = c(0, log(3500)))
lines(log(strat_pop_year1$tb_diff), type = 'o')
lines(log(strat_pop_year2$tb_diff), type = 'o', col = "blue")
lines(log(strat_pop_year3$tb_diff), type = 'o', col = "red")
lines(log(strat_pop_year1$tb_diff + strat_pop_year2$tb_diff + strat_pop_year3$tb_diff), type = 'o', col = "green")
lines(2:(length(p_incid_year) + 1), log(p_incid_year*nrow(IMPUTED_sample)), type = 'o', col = "magenta")
legend('topright',
       legend = c("(150,250]","(250,350]","(350,1e+05]","combined","Aldridge"),
       col = c("black","blue","red","green","magenta"), lty = 1)

# smoothed fit
lo1 <- loess(tb_diff_log~year, strat_pop_year1, span = 0.3, degree = 2)
lo2 <- loess(tb_diff_log~year, strat_pop_year2, span = 0.3, degree = 2)
lo3 <- loess(tb_diff_log~year, strat_pop_year3, span = 0.3, degree = 2)
lines(2:(length(predict(lo1)) + 1), predict(lo1), col = 'black', lwd = 2, type = 'l')
lines(2:(length(predict(lo2)) + 1), predict(lo2), col = 'blue', lwd = 2, type = 'l')
lines(2:(length(predict(lo3)) + 1), predict(lo3), col = 'red', lwd = 2, type = 'l')


# grid of separate events -----------------------------------------------------

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
